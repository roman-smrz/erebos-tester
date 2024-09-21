module Test.Builtins (
    builtins,
) where

import Test

builtins :: [ ( VarName, SomeVarValue ) ]
builtins =
    [ ( VarName "wait", builtinWait )
    ]

builtinWait :: SomeVarValue
builtinWait = SomeVarValue mempty $ const $ TestBlock [ Wait ]
