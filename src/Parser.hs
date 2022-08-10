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

varName :: TestParser VarName
varName = do
    VarName . T.splitOn (T.singleton '.') . TL.toStrict <$>
        takeWhile1P Nothing (\x -> isAlphaNum x || x == '_' || x == '.')

varExpansion :: TestParser VarName
varExpansion = do
    void $ char '$'
    choice
        [ VarName . (:[]) <$> identifier
        ,do void $ char '{'
            name <- varName
            void $ char '}'
            return name
        ]

quotedString :: TestParser (Expr Text)
quotedString = label "string" $ lexeme $ do
    symbol "\""
    let inner = choice
            [ char '"' >> return []
            , takeWhile1P Nothing (`notElem` "\"\\$") >>= \s -> (StringLit (TL.toStrict s):) <$> inner
            ,do void $ char '\\'
                c <- choice
                    [ char '\\' >> return '\\'
                    , char '"' >> return '"'
                    , char '$' >> return '$'
                    , char 'n' >> return '\n'
                    , char 'r' >> return '\r'
                    , char 't' >> return '\t'
                    ]
                (StringLit (T.singleton c) :) <$> inner
            ,do name <- varExpansion
                (StringVar name :) <$> inner
            ]
    Concat <$> inner

regex :: TestParser (Expr Regex)
regex = label "regular expression" $ lexeme $ do
    symbol "/"
    let inner = choice
            [ char '/' >> return []
            , takeWhile1P Nothing (`notElem` "/\\$") >>= \s -> (StringLit (TL.toStrict s) :) <$> inner
            ,do void $ char '\\'
                s <- choice
                    [ char '/' >> return (StringLit $ T.singleton '/')
                    , anySingle >>= \c -> return (StringLit $ T.pack ['\\', c])
                    ]
                (s:) <$> inner
            ,do name <- varExpansion
                (StringVar name :) <$> inner
            ]
    expr <- Regex <$> inner
    _ <- eval expr -- test regex parsing with empty variables
    return expr

stringExpr :: TestParser (Expr Text)
stringExpr = choice
    [ quotedString
    , StringVar <$> varName
    ]

boolExpr :: TestParser (Expr Bool)
boolExpr = do
    x <- stringExpr
    sc
    op <- choice
        [ symbol "==" >> return (==)
        , symbol "/=" >> return (/=)
        ]
    y <- stringExpr
    sc
    return $ BinOp op x y


class GInit f where ginit :: f x
instance GInit U1 where ginit = U1
instance GInit (K1 i (Maybe a)) where ginit = K1 Nothing
instance GInit f => GInit (M1 i c f) where ginit = M1 ginit
instance (GInit f, GInit h) => GInit (f :*: h) where ginit = ginit :*: ginit

data Param a = forall b. Param String (Lens' a (Maybe b)) (TestParser b)

getSourceLine :: TestParser SourceLine
getSourceLine = do
    pstate <- statePosState <$> getParserState
    return $ SourceLine $ T.concat
        [ T.pack $ sourcePosPretty $ pstateSourcePos pstate
        , T.pack ": "
        , TL.toStrict $ TL.takeWhile (/='\n') $ pstateInput pstate
        ]


letStatement :: TestParser [TestStep]
letStatement = do
    line <- getSourceLine
    wsymbol "let"
    name <- VarName . (:[]) <$> identifier
    sc
    symbol "="
    value <- stringExpr
    return [Let line name value]


command :: (Generic b, GInit (Rep b)) => String -> [Param b] -> (SourceLine -> b -> TestParser a) -> TestParser [a]
command name params fin = do
    origline <- getSourceLine
    wsymbol name
    let blockHelper line prev cur = L.indentBlock scn $ helper line prev cur
        helper line prev cur = choice $ concat
            [[ do void $ eol
                  L.IndentNone . (:[]) <$> fin line cur
             ]
            ,[ do void $ lexeme (char ':')
                  return $ L.IndentSome Nothing (return . concat) $ do
                      line' <- getSourceLine
                      blockHelper line' prev cur
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
                helper line (sym:prev) (l .~ Just x $ cur)
            ]

    blockHelper origline [] (G.to ginit)


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
    ] $ \_ b -> Spawn
        <$> (maybe (fail "missing 'as' <proc>") return $ b ^. spawnBuilderProc)
        <*> (maybe (fail "missing 'on' <node>") return $ b ^. spawnBuilderNode)


data SendBuilder = SendBuilder
    { _sendBuilderProc :: Maybe ProcName
    , _sendBuilderLine :: Maybe (Expr Text)
    }
    deriving (Generic)

makeLenses ''SendBuilder

testSend :: TestParser [TestStep]
testSend = command "send"
    [ Param "to" sendBuilderProc procName
    , Param "" sendBuilderLine quotedString
    ] $ \_ b -> Send
        <$> (maybe (fail "missing 'to' <proc>") return $ b ^. sendBuilderProc)
        <*> (maybe (fail "missing line to send") return $ b ^. sendBuilderLine)


data ExpectBuilder = ExpectBuilder
    { _expectBuilderProc :: Maybe ProcName
    , _expectBuilderRegex :: Maybe (Expr Regex)
    , _expectBuilderCaptures :: Maybe [VarName]
    }
    deriving (Generic)

makeLenses ''ExpectBuilder

testExpect :: TestParser [TestStep]
testExpect = command "expect"
    [ Param "from" expectBuilderProc procName
    , Param "" expectBuilderRegex regex
    , Param "capture" expectBuilderCaptures (listOf $ VarName . (:[]) <$> identifier)
    ] $ \s b -> Expect s
        <$> (maybe (fail "missing 'from' <proc>") return $ b ^. expectBuilderProc)
        <*> (maybe (fail "missing regex to match") return $ b ^. expectBuilderRegex)
        <*> (maybe (return []) return $ b ^. expectBuilderCaptures)


data GuardBuilder = GuardBuilder
    { _guardBuilderExpr :: Maybe (Expr Bool)
    }
    deriving (Generic)

makeLenses ''GuardBuilder

testGuard :: TestParser [TestStep]
testGuard = command "guard"
    [ Param "" guardBuilderExpr boolExpr
    ] $ \s b -> Guard s
        <$> (maybe (fail "missing guard expression") return $ b ^. guardBuilderExpr)


testWait :: TestParser [TestStep]
testWait = do
    wsymbol "wait"
    return [Wait]

parseTestDefinition :: TestParser Test
parseTestDefinition = label "test definition" $ toplevel $ do
    block (\name steps -> return $ Test name $ concat steps) header $ choice
        [ letStatement
        , testSpawn
        , testSend
        , testExpect
        , testGuard
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
