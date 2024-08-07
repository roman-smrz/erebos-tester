module Test.Builtins (
    builtins,
) where

import Test

builtins :: [ ( VarName, SomeVarValue ) ]
builtins =
    [ ( VarName "wait", SomeVarValue builtinWait )
    ]

builtinWait :: TestBlock
builtinWait = TestBlock [ Wait ]
