module Test.Builtins (
    builtins,
) where

import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Typeable

import Process (Process)
import Test

builtins :: [ ( VarName, SomeVarValue ) ]
builtins =
    [ ( VarName "send", builtinSend )
    , ( VarName "flush", builtinFlush )
    , ( VarName "guard", builtinGuard )
    , ( VarName "wait", builtinWait )
    ]

getArg :: Typeable a => FunctionArguments SomeExpr -> Maybe ArgumentKeyword -> (Expr a)
getArg args = fromMaybe (error "parameter mismatch") . getArgMb args

getArgMb :: Typeable a => FunctionArguments SomeExpr -> Maybe ArgumentKeyword -> Maybe (Expr a)
getArgMb (FunctionArguments args) kw = do
    SomeExpr expr <- M.lookup kw args
    cast expr

builtinSend :: SomeVarValue
builtinSend = SomeVarValue (FunctionArguments $ M.fromList atypes) $
    \_ args -> TestBlock [ Send (getArg args (Just "to")) (getArg args Nothing) ]
  where
    atypes =
        [ ( Just "to", SomeArgumentType (ContextDefault @Process) )
        , ( Nothing, SomeArgumentType (RequiredArgument @Text) )
        ]

builtinFlush :: SomeVarValue
builtinFlush = SomeVarValue (FunctionArguments $ M.fromList atypes) $
    \_ args -> TestBlock [ Flush (getArg args (Just "from")) (getArgMb args Nothing) ]
  where
    atypes =
        [ ( Just "from", SomeArgumentType (ContextDefault @Process) )
        , ( Nothing, SomeArgumentType (OptionalArgument @Regex) )
        ]

builtinGuard :: SomeVarValue
builtinGuard = SomeVarValue (FunctionArguments $ M.singleton Nothing (SomeArgumentType (RequiredArgument @Bool))) $
    \sline args -> TestBlock [ Guard sline (getArg args Nothing) ]

builtinWait :: SomeVarValue
builtinWait = SomeVarValue mempty $ const . const $ TestBlock [ Wait ]
