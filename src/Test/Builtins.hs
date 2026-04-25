module Test.Builtins (
    builtins,
) where

import Data.Map qualified as M
import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Text qualified as T

import Process
import Process.Signal
import Script.Expr
import Test

builtins :: GlobalDefs
builtins = M.fromList $ concat
    [ [ fq "send" builtinSend
      , fq "flush" builtinFlush
      , fq "ignore" builtinIgnore
      , fq "guard" builtinGuard
      , fq "multiply_timeout" builtinMultiplyTimeout
      , fq "wait" builtinWait
      , fq "concat" builtinConcat
      ]
    , map (uncurry fq) signalBuiltins
    ]
  where
    fq name impl = (( ModuleName [ "$" ], VarName name ), impl )

biVar :: ExprType a => Text -> Expr a
biVar = Variable SourceLineBuiltin . LocalVarName . VarName

biOpt :: ExprType a => Text -> Expr (Maybe a)
biOpt = OptVariable SourceLineBuiltin . LocalVarName . VarName

biArgs :: [ ( Maybe ArgumentKeyword, a ) ] -> FunctionArguments ( VarName, a )
biArgs = FunctionArguments . M.fromList . map (\( kw, atype ) -> ( kw, ( VarName $ maybe "$0" (\(ArgumentKeyword tkw) -> "$" <> tkw) kw, atype ) ))

builtinSend :: SomeExpr
builtinSend = SomeExpr $ ArgsReq (biArgs atypes) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (Send <$> biVar "$to" <*> biVar "$0")
  where
    atypes =
        [ ( Just "to", SomeArgumentType ContextDefault (ExprTypePrim (Proxy @Process)) )
        , ( Nothing, SomeArgumentType RequiredArgument (ExprTypePrim (Proxy @Text)) )
        ]

builtinFlush :: SomeExpr
builtinFlush = SomeExpr $ ArgsReq (biArgs atypes) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (Flush <$> biVar "$from" <*> biOpt "$matching")
  where
    atypes =
        [ ( Just "from", SomeArgumentType ContextDefault (ExprTypePrim (Proxy @Process)) )
        , ( Just "matching", SomeArgumentType OptionalArgument (ExprTypePrim (Proxy @Regex)) )
        ]

builtinIgnore :: SomeExpr
builtinIgnore = SomeExpr $ ArgsReq (biArgs atypes) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (CreateObject (Proxy @IgnoreProcessOutput) <$> ((,) <$> biVar "$from" <*> biOpt "$matching"))
  where
    atypes =
        [ ( Just "from", SomeArgumentType ContextDefault (ExprTypePrim (Proxy @Process)) )
        , ( Just "matching", SomeArgumentType OptionalArgument (ExprTypePrim (Proxy @Regex)) )
        ]

builtinGuard :: SomeExpr
builtinGuard = SomeExpr $
    ArgsReq (biArgs [ ( Nothing, SomeArgumentType RequiredArgument (ExprTypePrim (Proxy @Bool)) ) ]) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (Guard <$> Variable SourceLineBuiltin callStackFqVarName <*> biVar "$0")

builtinMultiplyTimeout :: SomeExpr
builtinMultiplyTimeout = SomeExpr $ ArgsReq (biArgs $ [ ( Just "by", SomeArgumentType RequiredArgument (ExprTypePrim (Proxy @Scientific)) ) ]) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (CreateObject (Proxy @MultiplyTimeout) <$> biVar "$by")

builtinWait :: SomeExpr
builtinWait = SomeExpr $ Pure $ TestBlockStep EmptyTestBlock Wait

builtinConcat :: SomeExpr
builtinConcat = SomeExpr $ TypeLambda (TypeVar "a")
    (ExprTypeFunction
        (ExprTypeArguments $ FunctionArguments $ M.singleton Nothing $ SomeArgumentType RequiredArgument
            (ExprTypeApp (ExprTypeConstr1 (Proxy @[])) [ ExprTypeApp (ExprTypeConstr1 (Proxy @[])) [ ExprTypeVar (TypeVar "a") ] ] ))
        (ExprTypeApp (ExprTypeConstr1 (Proxy @[])) [ ExprTypeVar (TypeVar "a") ])
    ) $ \case
        ExprTypePrim (pa :: Proxy a) -> HideFunType (FunctionArguments $ M.singleton Nothing $ SomeArgumentType RequiredArgument (ExprTypePrim (Proxy :: Proxy [[ a ]]))) $
            ArgsReq (biArgs [ ( Nothing, SomeArgumentType RequiredArgument (ExprTypePrim pa) ) ]) $ FunctionAbstraction $ (concat :: [[ a ]] -> [ a ]) <$> biVar "$0"
        t -> Undefined ("ambiguous type ‘" <> T.unpack (textSomeExprType t) <> "’ for concat") :: Expr DynamicType
