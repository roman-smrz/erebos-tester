module Test.Builtins (
    builtins,
) where

import Data.Map qualified as M
import Data.Proxy
import Data.Scientific
import Data.Text (Text)

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
        [ ( Just "to", SomeArgumentType (ContextDefault @Process) )
        , ( Nothing, SomeArgumentType (RequiredArgument @Text) )
        ]

builtinFlush :: SomeExpr
builtinFlush = SomeExpr $ ArgsReq (biArgs atypes) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (Flush <$> biVar "$from" <*> biOpt "$matching")
  where
    atypes =
        [ ( Just "from", SomeArgumentType (ContextDefault @Process) )
        , ( Just "matching", SomeArgumentType (OptionalArgument @Regex) )
        ]

builtinIgnore :: SomeExpr
builtinIgnore = SomeExpr $ ArgsReq (biArgs atypes) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (CreateObject (Proxy @IgnoreProcessOutput) <$> ((,) <$> biVar "$from" <*> biOpt "$matching"))
  where
    atypes =
        [ ( Just "from", SomeArgumentType (ContextDefault @Process) )
        , ( Just "matching", SomeArgumentType (OptionalArgument @Regex) )
        ]

builtinGuard :: SomeExpr
builtinGuard = SomeExpr $
    ArgsReq (biArgs [ ( Nothing, SomeArgumentType (RequiredArgument @Bool) ) ]) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (Guard <$> Variable SourceLineBuiltin callStackFqVarName <*> biVar "$0")

builtinMultiplyTimeout :: SomeExpr
builtinMultiplyTimeout = SomeExpr $ ArgsReq (biArgs $ [ ( Just "by", SomeArgumentType (RequiredArgument @Scientific) ) ]) $
    FunctionAbstraction $ TestBlockStep EmptyTestBlock <$> (CreateObject (Proxy @MultiplyTimeout) <$> biVar "$by")

builtinWait :: SomeExpr
builtinWait = SomeExpr $ Pure $ TestBlockStep EmptyTestBlock Wait
