{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Lens (Lens', makeLenses, (^.), (.~))
import Control.Monad.State

import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text qualified as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Void

import Generics.Deriving.Base as G

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import System.Exit

import Test

type TestParser = ParsecT Void TestStream (State (Set ProcName))

type TestStream = TL.Text

instance MonadEval TestParser where
    lookupStringVar _ = return T.empty

skipLineComment :: TestParser ()
skipLineComment = L.skipLineComment $ TL.pack "#"

scn :: TestParser ()
scn = L.space space1 skipLineComment empty

sc :: TestParser ()
sc = L.space (void $ takeWhile1P Nothing f) skipLineComment empty
    where f x = x == ' ' || x == '\t'

wordChar :: TestParser (Token TestStream)
wordChar = alphaNumChar <|> char '_'

lexeme :: TestParser a -> TestParser a
lexeme = L.lexeme sc

symbol :: String -> TestParser ()
symbol = void . L.symbol sc . TL.pack

wsymbol :: String -> TestParser ()
wsymbol str = void $ lexeme $ string (TL.pack str) <* notFollowedBy wordChar

toplevel :: TestParser a -> TestParser a
toplevel = L.nonIndented scn

block :: (a -> [b] -> TestParser c) -> TestParser a -> TestParser b -> TestParser c
block merge header item = L.indentBlock scn $ do
    h <- header
    choice
        [ do try $ void $ lexeme $ char ':'
             return $ L.IndentSome Nothing (merge h) item
        , L.IndentNone <$> merge h []
        ]

listOf :: TestParser a -> TestParser [a]
listOf item = do
    sc
    x <- item
    sc
    (x:) <$> choice [ char ',' >> listOf item, return [] ]

nodeName :: TestParser NodeName
nodeName = label "network node name" $ lexeme $ do
    c <- lowerChar
    cs <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '-')
    return $ NodeName $ TL.toStrict (c `TL.cons` cs)

procName :: TestParser ProcName
procName = label "process name" $ lexeme $ do
    c <- lowerChar
    cs <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '-')
    return $ ProcName $ TL.toStrict (c `TL.cons` cs)

identifier :: TestParser Text
identifier = do
    TL.toStrict <$> takeWhile1P Nothing (\x -> isAlphaNum x || x == '_')

varExpansion :: TestParser VarName
varExpansion = do
    void $ char '$'
    choice
        [ VarName . (:[]) <$> identifier
        ,do void $ char '{'
            name <- takeWhile1P Nothing (/='}')
            void $ char '}'
            return $ VarName $ T.splitOn (T.singleton '.') (TL.toStrict name)
        ]

quotedString :: TestParser StringExpr
quotedString = label "string" $ lexeme $ do
    symbol "\""
    let inner = choice
            [ char '"' >> return []
            , takeWhile1P Nothing (`notElem` "\"\\$") >>= \s -> (Left (TL.toStrict s):) <$> inner
            ,do void $ char '\\'
                c <- choice
                    [ char '\\' >> return '\\'
                    , char '"' >> return '"'
                    , char '$' >> return '$'
                    , char 'n' >> return '\n'
                    , char 'r' >> return '\r'
                    , char 't' >> return '\t'
                    ]
                (Left (T.singleton c) :) <$> inner
            ,do name <- varExpansion
                (Right name :) <$> inner
            ]
    StringExpr <$> inner

regex :: TestParser RegexExpr
regex = label "regular expression" $ lexeme $ do
    symbol "/"
    let inner = choice
            [ char '/' >> return []
            , takeWhile1P Nothing (`notElem` "/\\$") >>= \s -> (Left (TL.unpack s) :) <$> inner
            ,do void $ char '\\'
                s <- choice
                    [ char '/' >> return (Left $ "/")
                    , anySingle >>= \c -> return (Left ['\\', c])
                    ]
                (s:) <$> inner
            ,do name <- varExpansion
                (Right name :) <$> inner
            ]
    expr <- RegexExpr <$> inner
    _ <- evalRegexExpr expr -- test regex parsing with empty variables
    return expr


class GInit f where ginit :: f x
instance GInit U1 where ginit = U1
instance GInit (K1 i (Maybe a)) where ginit = K1 Nothing
instance GInit f => GInit (M1 i c f) where ginit = M1 ginit
instance (GInit f, GInit h) => GInit (f :*: h) where ginit = ginit :*: ginit

data Param a = forall b. Param String (Lens' a (Maybe b)) (TestParser b)

command :: (Generic b, GInit (Rep b)) => String -> [Param b] -> (b -> TestParser a) -> TestParser [a]
command name params fin = do
    wsymbol name
    let blockHelper prev cur = L.indentBlock scn $ helper prev cur
        helper prev cur = choice $ concat
            [[ do void $ eol
                  L.IndentNone . (:[]) <$> fin cur
             ]
            ,[ do void $ lexeme (char ':')
                  return $ L.IndentSome Nothing (return . concat) (blockHelper prev cur)
             ]
            , flip map params $ \(Param sym l p) -> do
                x <- if null sym
                    then do
                        x <- p
                        when (any null prev) $ do
                            fail $ "multiple unnamed parameters"
                        return x
                    else do
                        wsymbol sym
                        when (any (== sym) prev) $ do
                            fail $ "multiple '" ++ sym ++ "' parameters"
                        p
                helper (sym:prev) (l .~ Just x $ cur)
            ]

    blockHelper [] (G.to ginit)


data SpawnBuilder = SpawnBuilder
    { _spawnBuilderProc :: Maybe ProcName
    , _spawnBuilderNode :: Maybe NodeName
    }
    deriving (Generic)

makeLenses ''SpawnBuilder

testSpawn :: TestParser [TestStep]
testSpawn = command "spawn"
    [ Param "on" spawnBuilderNode nodeName
    , Param "as" spawnBuilderProc procName
    ] $ \b -> Spawn
        <$> (maybe (fail "missing 'as' <proc>") return $ b ^. spawnBuilderProc)
        <*> (maybe (fail "missing 'on' <node>") return $ b ^. spawnBuilderNode)


data SendBuilder = SendBuilder
    { _sendBuilderProc :: Maybe ProcName
    , _sendBuilderLine :: Maybe StringExpr
    }
    deriving (Generic)

makeLenses ''SendBuilder

testSend :: TestParser [TestStep]
testSend = command "send"
    [ Param "to" sendBuilderProc procName
    , Param "" sendBuilderLine quotedString
    ] $ \b -> Send
        <$> (maybe (fail "missing 'to' <proc>") return $ b ^. sendBuilderProc)
        <*> (maybe (fail "missing line to send") return $ b ^. sendBuilderLine)


data ExpectBuilder = ExpectBuilder
    { _expectBuilderProc :: Maybe ProcName
    , _expectBuilderRegex :: Maybe RegexExpr
    , _expectBuilderCaptures :: Maybe [VarName]
    }
    deriving (Generic)

makeLenses ''ExpectBuilder

testExpect :: TestParser [TestStep]
testExpect = command "expect"
    [ Param "from" expectBuilderProc procName
    , Param "" expectBuilderRegex regex
    , Param "capture" expectBuilderCaptures (listOf $ VarName . (:[]) <$> identifier)
    ] $ \b -> Expect
        <$> (maybe (fail "missing 'from' <proc>") return $ b ^. expectBuilderProc)
        <*> (maybe (fail "missing regex to match") return $ b ^. expectBuilderRegex)
        <*> (maybe (return []) return $ b ^. expectBuilderCaptures)


testWait :: TestParser [TestStep]
testWait = do
    wsymbol "wait"
    return [Wait]

parseTestDefinition :: TestParser Test
parseTestDefinition = label "test definition" $ toplevel $ do
    block (\name steps -> return $ Test name $ concat steps) header $ choice
        [ testSpawn
        , testSend
        , testExpect
        , testWait
        ]
    where header = do
              wsymbol "test"
              lexeme $ TL.toStrict <$> takeWhileP (Just "test name") (/=':')

parseTestDefinitions :: TestParser [Test]
parseTestDefinitions = do
    tests <- many parseTestDefinition
    eof
    return tests

parseTestFile :: FilePath -> IO [Test]
parseTestFile path = do
    content <- TL.readFile path
    case evalState (runParserT parseTestDefinitions path content) S.empty of
         Left err -> putStr (errorBundlePretty err) >> exitFailure
         Right tests -> return tests
