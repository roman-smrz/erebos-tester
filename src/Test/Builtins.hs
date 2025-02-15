module Test.Builtins (
    builtins,
) where

import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)

import Process (Process)
import Test

builtins :: GlobalDefs
builtins = M.fromList
    [ fq "send" builtinSend
    , fq "flush" builtinFlush
    , fq "guard" builtinGuard
    , fq "wait" builtinWait
    ]
  where
    fq name impl = (( ModuleName [ "$" ], VarName name ), impl )

getArg :: ExprType a => FunctionArguments SomeVarValue -> Maybe ArgumentKeyword -> a
getArg args = fromMaybe (error "parameter mismatch") . getArgMb args

getArgMb :: ExprType a => FunctionArguments SomeVarValue -> Maybe ArgumentKeyword -> Maybe a
getArgMb (FunctionArguments args) kw = do
    fromSomeVarValue SourceLineBuiltin (LocalVarName (VarName "")) =<< M.lookup kw args

getArgVars :: FunctionArguments SomeVarValue -> Maybe ArgumentKeyword -> [ (( FqVarName, [ Text ] ), SomeVarValue ) ]
getArgVars (FunctionArguments args) kw = do
    maybe [] svvVariables $ M.lookup kw args

builtinSend :: SomeVarValue
builtinSend = SomeVarValue $ VarValue [] (FunctionArguments $ M.fromList atypes) $
    \_ args -> TestBlock [ Send (getArg args (Just "to")) (getArg args Nothing) ]
  where
    atypes =
        [ ( Just "to", SomeArgumentType (ContextDefault @Process) )
        , ( Nothing, SomeArgumentType (RequiredArgument @Text) )
        ]

builtinFlush :: SomeVarValue
builtinFlush = SomeVarValue $ VarValue [] (FunctionArguments $ M.fromList atypes) $
    \_ args -> TestBlock [ Flush (getArg args (Just "from")) (getArgMb args (Just "matching")) ]
  where
    atypes =
        [ ( Just "from", SomeArgumentType (ContextDefault @Process) )
        , ( Just "matching", SomeArgumentType (OptionalArgument @Regex) )
        ]

builtinGuard :: SomeVarValue
builtinGuard = SomeVarValue $ VarValue [] (FunctionArguments $ M.singleton Nothing (SomeArgumentType (RequiredArgument @Bool))) $
    \sline args -> TestBlock [ Guard sline (getArgVars args Nothing) (getArg args Nothing) ]

builtinWait :: SomeVarValue
builtinWait = someConstValue $ TestBlock [ Wait ]
