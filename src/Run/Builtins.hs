module Run.Builtins (
    module Run,
    loadModules,
) where

import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Void

import Asset (Asset)
import Network (Network, Node)
import Parser (CustomTestError)
import Process (Process)
import Process.Signal (Signal)
import Run
import Script.Expr
import Test (Test, Tag)


builtinTypes :: [ SomePrimType ]
builtinTypes =
    [ SomePrimType @() Proxy
    , SomePrimType @Integer Proxy
    , SomePrimType @Scientific Proxy
    , SomePrimType @Bool Proxy
    , SomePrimType @Text Proxy
    , SomePrimType @Void Proxy
    , SomePrimType @Regex Proxy

    , SomePrimType @Test Proxy
    , SomePrimType @Tag Proxy
    , SomePrimType @Asset Proxy

    , SomePrimType @Network Proxy
    , SomePrimType @Node Proxy

    , SomePrimType @Process Proxy
    , SomePrimType @Signal Proxy
    ]

loadModules :: [ ( FilePath, Maybe Text ) ] -> IO (Either CustomTestError LoadedModules)
loadModules = loadModules' builtinTypes
