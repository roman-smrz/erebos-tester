module Test.Builtins (
    builtins,
) where

import Data.Map qualified as M
import Data.Maybe
import Data.Proxy
import Data.Scientific
import Data.Text (Text)

import Process
import Script.Expr
import Test

builtins :: GlobalDefs
builtins = M.fromList
    [ fq "send" builtinSend
    , fq "flush" builtinFlush
    , fq "ignore" builtinIgnore
    , fq "guard" builtinGuard
    , fq "multiply_timeout" builtinMultiplyTimeout
    , fq "wait" builtinWait
    ]
  where
    fq name impl = (( ModuleName [ "$" ], VarName name ), impl )

getArg :: ExprType a => FunctionArguments SomeVarValue -> Maybe ArgumentKeyword -> a
getArg args = fromMaybe (error "parameter mismatch") . getArgMb args

getArgMb :: ExprType a => FunctionArguments SomeVarValue -> Maybe ArgumentKeyword -> Maybe a
getArgMb (FunctionArguments args) kw = do
    fromSomeVarValue (CallStack []) (LocalVarName (VarName "")) =<< M.lookup kw args

builtinSend :: SomeVarValue
builtinSend = SomeVarValue $ VarValue [] (FunctionArguments $ M.fromList atypes) $
    \_ args -> TestBlockStep EmptyTestBlock $ Send (getArg args (Just "to")) (getArg args Nothing)
  where
    atypes =
        [ ( Just "to", SomeArgumentType (ContextDefault @Process) )
        , ( Nothing, SomeArgumentType (RequiredArgument @Text) )
        ]

builtinFlush :: SomeVarValue
builtinFlush = SomeVarValue $ VarValue [] (FunctionArguments $ M.fromList atypes) $
    \_ args -> TestBlockStep EmptyTestBlock $ Flush (getArg args (Just "from")) (getArgMb args (Just "matching"))
  where
    atypes =
        [ ( Just "from", SomeArgumentType (ContextDefault @Process) )
        , ( Just "matching", SomeArgumentType (OptionalArgument @Regex) )
        ]

builtinIgnore :: SomeVarValue
builtinIgnore = SomeVarValue $ VarValue [] (FunctionArguments $ M.fromList atypes) $
    \_ args -> TestBlockStep EmptyTestBlock $ CreateObject (Proxy @IgnoreProcessOutput) ( getArg args (Just "from"), getArgMb args (Just "matching") )
  where
    atypes =
        [ ( Just "from", SomeArgumentType (ContextDefault @Process) )
        , ( Just "matching", SomeArgumentType (OptionalArgument @Regex) )
        ]

builtinGuard :: SomeVarValue
builtinGuard = SomeVarValue $ VarValue [] (FunctionArguments $ M.singleton Nothing (SomeArgumentType (RequiredArgument @Bool))) $
    \stack args -> TestBlockStep EmptyTestBlock $ Guard stack (getArg args Nothing)

builtinMultiplyTimeout :: SomeVarValue
builtinMultiplyTimeout = SomeVarValue $ VarValue [] (FunctionArguments $ M.singleton (Just "by") (SomeArgumentType (RequiredArgument @Scientific))) $
    \_ args -> TestBlockStep EmptyTestBlock $ CreateObject (Proxy @MultiplyTimeout) (getArg args (Just "by"))

builtinWait :: SomeVarValue
builtinWait = someConstValue $ TestBlockStep EmptyTestBlock Wait
