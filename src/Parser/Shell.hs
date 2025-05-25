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
    , escapedChar
    , stringExpansion
    , unquotedString
    ]
  where
    specialChars = [ '\"', '\\', '$' ]

    unquotedString :: TestParser (Expr Text)
    unquotedString = do
        Pure . TL.toStrict <$> takeWhile1P Nothing (\c -> not (isSpace c) && c `notElem` specialChars)

    doubleQuotedString :: TestParser (Expr Text)
    doubleQuotedString = do
        void $ char '"'
        let inner = choice
                [ char '"' >> return []
                , (:) <$> (Pure . TL.toStrict <$> takeWhile1P Nothing (`notElem` specialChars)) <*> inner
                , (:) <$> escapedChar <*> inner
                , (:) <$> stringExpansion <*> inner
                ]
        App AnnNone (Pure T.concat) . foldr (liftA2 (:)) (Pure []) <$> inner

    singleQuotedString :: TestParser (Expr Text)
    singleQuotedString = do
        Pure . TL.toStrict <$> (char '\'' *> takeWhileP Nothing (/= '\'') <* char '\'')

    escapedChar :: TestParser (Expr Text)
    escapedChar = do
        void $ char '\\'
        Pure <$> choice
            [ char '\\' >> return "\\"
            , char '"' >> return "\""
            , char '$' >> return "$"
            , char 'n' >> return "\n"
            , char 'r' >> return "\r"
            , char 't' >> return "\t"
            ]

parseArguments :: TestParser (Expr [ Text ])
parseArguments = foldr (liftA2 (:)) (Pure []) <$> many parseArgument

shellStatement :: TestParser (Expr [ ShellStatement ])
shellStatement = label "shell statement" $ do
    command <- parseArgument
    args <- parseArguments
    return $ fmap (: []) $ ShellStatement
        <$> command
        <*> args

shellScript :: TestParser (Expr ShellScript)
shellScript = do
    indent <- L.indentLevel
    fmap ShellScript <$> blockOf indent shellStatement
