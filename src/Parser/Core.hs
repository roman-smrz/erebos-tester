module Parser.Core where

import Control.Monad.Identity
import Control.Monad.State

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Typeable
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Network ()
import Test

type TestParser = ParsecT Void TestStream (State TestParserState)

type TestStream = TL.Text

data TestParserState = TestParserState
    { testVars :: [(VarName, SomeExprType)]
    , testContext :: SomeExpr
    }

data SomeExpr = forall a. ExprType a => SomeExpr (Expr a)
data SomeExprType = forall a. ExprType a => SomeExprType (Proxy a)

someEmptyVar :: SomeExprType -> SomeVarValue
someEmptyVar (SomeExprType (Proxy :: Proxy a)) = SomeVarValue $ emptyVarValue @a

textSomeExprType :: SomeExprType -> Text
textSomeExprType (SomeExprType p) = textExprType p

instance MonadEval TestParser where
    lookupVar name = maybe (fail $ "variable not in scope: '" ++ unpackVarName name ++ "'") (return . someEmptyVar) =<< gets (lookup name . testVars)
    rootNetwork = return emptyVarValue

skipLineComment :: TestParser ()
skipLineComment = L.skipLineComment $ TL.pack "#"

scn :: TestParser ()
scn = L.space space1 skipLineComment empty

sc :: TestParser ()
sc = L.space hspace1 skipLineComment empty

wordChar :: TestParser (Token TestStream)
wordChar = alphaNumChar <|> char '_'

lexeme :: TestParser a -> TestParser a
lexeme = L.lexeme sc

symbol, osymbol, wsymbol :: String -> TestParser ()
symbol str  = void $       (string (TL.pack str)) <* sc
osymbol str = void $ try $ (string (TL.pack str) <* notFollowedBy operatorChar) <* sc
wsymbol str = void $ try $ (string (TL.pack str) <* notFollowedBy wordChar) <* sc

operatorChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
operatorChar = satisfy $ (`elem` ['.', '+', '-', '*', '/', '='])
{-# INLINE operatorChar #-}

localState :: TestParser a -> TestParser a
localState inner = do
    s <- get
    x <- inner
    put s
    return x

toplevel :: TestParser a -> TestParser a
toplevel = L.nonIndented scn

block :: (a -> [b] -> TestParser c) -> TestParser a -> TestParser b -> TestParser c
block merge header item = L.indentBlock scn $ do
    h <- header
    choice
        [ do symbol ":"
             return $ L.IndentSome Nothing (merge h) item
        , L.IndentNone <$> merge h []
        ]

listOf :: TestParser a -> TestParser [a]
listOf item = do
    x <- item
    (x:) <$> choice [ symbol "," >> listOf item, return [] ]
