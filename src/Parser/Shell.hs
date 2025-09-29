module Parser.Shell (
    ShellScript,
    shellScript,
) where

import Control.Applicative (liftA2)
import Control.Monad

import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Parser.Core
import Parser.Expr
import Script.Expr
import Script.Shell

parseArgument :: TestParser (Expr Text)
parseArgument = lexeme $ fmap (App AnnNone (Pure T.concat) <$> foldr (liftA2 (:)) (Pure [])) $ some $ choice
    [ doubleQuotedString
    , singleQuotedString
    , standaloneEscapedChar
    , stringExpansion
    , unquotedString
    ]
  where
    specialChars = [ '"', '\'', '\\', '$', '#', '|', '>', '<', ';', '[', ']', '{', '}', '(', ')', '*', '?', '~', '&', '!' ]

    stringSpecialChars = [ '"', '\\', '$' ]

    unquotedString :: TestParser (Expr Text)
    unquotedString = do
        Pure . TL.toStrict <$> takeWhile1P Nothing (\c -> not (isSpace c) && c `notElem` specialChars)

    doubleQuotedString :: TestParser (Expr Text)
    doubleQuotedString = do
        void $ char '"'
        let inner = choice
                [ char '"' >> return []
                , (:) <$> (Pure . TL.toStrict <$> takeWhile1P Nothing (`notElem` stringSpecialChars)) <*> inner
                , (:) <$> stringEscapedChar <*> inner
                , (:) <$> stringExpansion <*> inner
                ]
        App AnnNone (Pure T.concat) . foldr (liftA2 (:)) (Pure []) <$> inner

    singleQuotedString :: TestParser (Expr Text)
    singleQuotedString = do
        Pure . TL.toStrict <$> (char '\'' *> takeWhileP Nothing (/= '\'') <* char '\'')

    stringEscapedChar :: TestParser (Expr Text)
    stringEscapedChar = do
        void $ char '\\'
        fmap Pure $ choice $
            map (\c -> char c >> return (T.singleton c)) stringSpecialChars ++
            [ char 'n' >> return "\n"
            , char 'r' >> return "\r"
            , char 't' >> return "\t"
            , return "\\"
            ]

    standaloneEscapedChar :: TestParser (Expr Text)
    standaloneEscapedChar = do
        void $ char '\\'
        fmap Pure $ choice $
            map (\c -> char c >> return (T.singleton c)) specialChars ++
            [ char ' ' >> return " "
            ]

parseArguments :: TestParser (Expr [ Text ])
parseArguments = foldr (liftA2 (:)) (Pure []) <$> many parseArgument

parseCommand :: TestParser (Expr ShellCommand)
parseCommand = label "shell statement" $ do
    line <- getSourceLine
    command <- parseArgument
    args <- parseArguments
    return $ ShellCommand
        <$> command
        <*> args
        <*> pure line

parsePipeline :: Maybe (Expr ShellPipeline) -> TestParser (Expr ShellPipeline)
parsePipeline mbupper = do
    cmd <- parseCommand
    let pipeline =
            case mbupper of
                Nothing -> fmap (\ecmd -> ShellPipeline ecmd Nothing) cmd
                Just upper -> liftA2 (\ecmd eupper -> ShellPipeline ecmd (Just eupper)) cmd upper
    choice
        [ do
            osymbol "|"
            parsePipeline (Just pipeline)

        , do
            return pipeline
        ]

parseStatement :: TestParser (Expr [ ShellStatement ])
parseStatement = do
    line <- getSourceLine
    fmap ((: []) . flip ShellStatement line) <$> parsePipeline Nothing

shellScript :: TestParser (Expr ShellScript)
shellScript = do
    indent <- L.indentLevel
    fmap ShellScript <$> blockOf indent parseStatement
