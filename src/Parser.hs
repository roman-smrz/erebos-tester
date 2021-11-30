{-# LANGUAGE TemplateHaskell #-}

module Parser (
    parseTestFile,
) where

import Control.Lens (Lens', makeLenses, (^.), (.~))
import Control.Monad.State

import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Void

import Generics.Deriving.Base as G

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String

import System.Exit

import Test

type TestParser = ParsecT Void TestStream (State (Set ProcName))

type TestStream = TL.Text

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

quotedString :: TestParser Text
quotedString = label "string" $ lexeme $ do
    symbol "\""
    str <- takeWhileP Nothing (/='"')
    symbol "\""
    return $ TL.toStrict str

regex :: TestParser (Regex, Text)
regex = label "regular expression" $ lexeme $ do
    symbol "/"
    pat <- takeWhileP Nothing (/='/')
    symbol "/"
    case compile defaultCompOpt defaultExecOpt ("^" ++ TL.unpack pat ++ "$") of
         Left err -> fail err
         Right re -> return (re, TL.toStrict pat)


class GInit f where ginit :: f x
instance GInit U1 where ginit = U1
instance GInit (K1 i (Maybe a)) where ginit = K1 Nothing
instance GInit f => GInit (M1 i c f) where ginit = M1 ginit
instance (GInit f, GInit h) => GInit (f :*: h) where ginit = ginit :*: ginit

data Param a = forall b. Param String (Lens' a (Maybe b)) (TestParser b)

command :: (Generic b, GInit (Rep b)) => String -> [Param b] -> (b -> TestParser a) -> TestParser a
command name params fin = do
    wsymbol name
    let helper prev cur = do
            (s, cur') <- choice $ flip map params $ \(Param sym l p) -> do
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
                return $ (sym, l .~ Just x $ cur)
            (eol >> return cur') <|> helper (s:prev) cur'

    fin =<< helper [] (G.to ginit)


data SpawnBuilder = SpawnBuilder
    { _spawnBuilderProc :: Maybe ProcName
    , _spawnBuilderNode :: Maybe NodeName
    }
    deriving (Generic)

makeLenses ''SpawnBuilder

testSpawn :: TestParser TestStep
testSpawn = command "spawn"
    [ Param "on" spawnBuilderNode nodeName
    , Param "as" spawnBuilderProc procName
    ] $ \b -> Spawn
        <$> (maybe (fail "missing 'as' <proc>") return $ b ^. spawnBuilderProc)
        <*> (maybe (fail "missing 'on' <node>") return $ b ^. spawnBuilderNode)


data SendBuilder = SendBuilder
    { _sendBuilderProc :: Maybe ProcName
    , _sendBuilderLine :: Maybe Text
    }
    deriving (Generic)

makeLenses ''SendBuilder

testSend :: TestParser TestStep
testSend = command "send"
    [ Param "to" sendBuilderProc procName
    , Param "" sendBuilderLine quotedString
    ] $ \b -> Send
        <$> (maybe (fail "missing 'to' <proc>") return $ b ^. sendBuilderProc)
        <*> (maybe (fail "missing line to send") return $ b ^. sendBuilderLine)


data ExpectBuilder = ExpectBuilder
    { _expectBuilderProc :: Maybe ProcName
    , _expectBuilderRegex :: Maybe (Regex, Text)
    }
    deriving (Generic)

makeLenses ''ExpectBuilder

testExpect :: TestParser TestStep
testExpect = command "expect"
    [ Param "from" expectBuilderProc procName
    , Param "" expectBuilderRegex regex
    ] $ \b -> Expect
        <$> (maybe (fail "missing 'from' <proc>") return $ b ^. expectBuilderProc)
        <*> (maybe (fail "missing regex to match") (return . fst) $ b ^. expectBuilderRegex)
        <*> (maybe (fail "missing regex to match") (return . snd) $ b ^. expectBuilderRegex)


testWait :: TestParser TestStep
testWait = do
    wsymbol "wait"
    return $ Wait

parseTestDefinition :: TestParser Test
parseTestDefinition = label "test definition" $ toplevel $ do
    block (\name steps -> return $ Test name steps) header (testSpawn <|> testSend <|> testExpect <|> testWait)
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
