module Script.Module (
    Module(..),
    ModuleName(..), textModuleName,
    moduleExportedDefinitions,
) where

import Script.Expr
import Test

data Module = Module
    { moduleName :: ModuleName
    , moduleTests :: [ Test ]
    , moduleDefinitions :: [ ( VarName, SomeExpr ) ]
    , moduleExports :: [ VarName ]
    }

moduleExportedDefinitions :: Module -> [ ( VarName, ( FqVarName, SomeExpr )) ]
moduleExportedDefinitions Module {..} =
    map (\( var, expr ) -> ( var, ( GlobalVarName moduleName var, expr ))) $
        filter ((`elem` moduleExports) . fst) moduleDefinitions
