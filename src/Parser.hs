module Parser (
    parseTestFile,
) where

import Control.Monad.State

import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Void

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

testSpawn :: TestParser TestStep
testSpawn = do
    wsymbol "spawn"
    wsymbol "on"
    nname <- nodeName
    wsymbol "as"
    pname <- procName
    return $ Spawn pname nname

testSend :: TestParser TestStep
testSend = do
    wsymbol "send"
    line <- quotedString
    wsymbol "to"
    pname <- procName
    return $ Send pname line

testExpect :: TestParser TestStep
testExpect = do
    wsymbol "expect"
    (re, pat) <- regex
    wsymbol "from"
    pname <- procName
    return $ Expect pname re pat

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
