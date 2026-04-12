module Script.Expr.Class (
    ExprType(..),
    ExprTypeConstr1(..),
    TypeDeconstructor(..),
    RecordSelector(..),
    ExprListUnpacker(..),
    ExprEnumerator(..),
) where

import Data.Kind
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable
import Data.Void

class Typeable a => ExprType a where
    textExprType :: proxy a -> Text
    textExprValue :: a -> Text

    matchTypeConstructor :: proxy a -> TypeDeconstructor a
    matchTypeConstructor _ = NoTypeDeconstructor

    recordMembers :: [(Text, RecordSelector a)]
    recordMembers = []

    exprExpansionConvTo :: ExprType b => Maybe (a -> b)
    exprExpansionConvTo = Nothing

    exprExpansionConvFrom :: ExprType b => Maybe (b -> a)
    exprExpansionConvFrom = Nothing

    exprListUnpacker :: proxy a -> Maybe (ExprListUnpacker a)
    exprListUnpacker _ = Nothing

    exprEnumerator :: proxy a -> Maybe (ExprEnumerator a)
    exprEnumerator _ = Nothing

class (Typeable a, forall b. ExprType b => ExprType (a b)) => ExprTypeConstr1 (a :: Type -> Type) where
    textExprTypeConstr1 :: proxy a -> Text -> Text

data TypeDeconstructor a
    = NoTypeDeconstructor
    | forall c x. (ExprTypeConstr1 c, ExprType x, c x ~ a) => TypeDeconstructor1 (Proxy c) (Proxy x)


data RecordSelector a = forall b. ExprType b => RecordSelector (a -> b)

data ExprListUnpacker a = forall e. ExprType e => ExprListUnpacker (a -> [e]) (Proxy a -> Proxy e)

data ExprEnumerator a = ExprEnumerator (a -> a -> [a]) (a -> a -> a -> [a])


instance ExprType () where
    textExprType _ = "Unit"
    textExprValue () = "()"

instance ExprType Integer where
    textExprType _ = T.pack "integer"
    textExprValue x = T.pack (show x)

    exprExpansionConvTo = listToMaybe $ catMaybes
        [ cast (T.pack . show :: Integer -> Text)
        ]

    exprEnumerator _ = Just $ ExprEnumerator enumFromTo enumFromThenTo

instance ExprType Scientific where
    textExprType _ = T.pack "number"
    textExprValue x = T.pack (show x)

    exprExpansionConvTo = listToMaybe $ catMaybes
        [ cast (T.pack . show :: Scientific -> Text)
        ]

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

instance ExprType a => ExprType [ a ] where
    textExprType _ = textExprTypeConstr1 @[] Proxy (textExprType @a Proxy)
    textExprValue x = "[" <> T.intercalate ", " (map textExprValue x) <> "]"
    matchTypeConstructor _ = TypeDeconstructor1 Proxy Proxy

    exprListUnpacker _ = Just $ ExprListUnpacker id (const Proxy)

instance ExprTypeConstr1 [] where
    textExprTypeConstr1 _ x = "[" <> x <> "]"

instance ExprType a => ExprType (Maybe a) where
    textExprType _ = textExprType @a Proxy <> "?"
    textExprValue (Just x) = textExprValue x
    textExprValue Nothing = "Nothing"

instance (ExprType a, ExprType b) => ExprType (Either a b) where
    textExprType _ = textExprType @a Proxy <> "|" <> textExprType @b Proxy
    textExprValue (Left x) = "Left " <> textExprValue x
    textExprValue (Right x) = "Right " <> textExprValue x
