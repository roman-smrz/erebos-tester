module Script.Expr.Class (
    ExprType(..),
    RecordSelector(..),
    ExprListUnpacker(..),
    ExprEnumerator(..),
) where

import Data.Scientific
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable
import Data.Void

class Typeable a => ExprType a where
    textExprType :: proxy a -> Text
    textExprValue :: a -> Text

    recordMembers :: [(Text, RecordSelector a)]
    recordMembers = []

    exprListUnpacker :: proxy a -> Maybe (ExprListUnpacker a)
    exprListUnpacker _ = Nothing

    exprEnumerator :: proxy a -> Maybe (ExprEnumerator a)
    exprEnumerator _ = Nothing

instance ExprType Integer where
    textExprType _ = T.pack "integer"
    textExprValue x = T.pack (show x)

    exprEnumerator _ = Just $ ExprEnumerator enumFromTo enumFromThenTo

instance ExprType Scientific where
    textExprType _ = T.pack "number"
    textExprValue x = T.pack (show x)

instance ExprType Bool where
    textExprType _ = T.pack "bool"
    textExprValue True = T.pack "true"
    textExprValue False = T.pack "false"

instance ExprType Text where
    textExprType _ = T.pack "string"
    textExprValue x = T.pack (show x)

instance ExprType Void where
    textExprType _ = T.pack "void"
    textExprValue _ = T.pack "<void>"

instance ExprType a => ExprType [a] where
    textExprType _ = "[" <> textExprType @a Proxy <> "]"
    textExprValue x = "[" <> T.intercalate ", " (map textExprValue x) <> "]"

    exprListUnpacker _ = Just $ ExprListUnpacker id (const Proxy)


data RecordSelector a = forall b. ExprType b => RecordSelector (a -> b)

data ExprListUnpacker a = forall e. ExprType e => ExprListUnpacker (a -> [e]) (Proxy a -> Proxy e)

data ExprEnumerator a = ExprEnumerator (a -> a -> [a]) (a -> a -> a -> [a])
