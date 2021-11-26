module Test (
    Test(..),
    TestStep(..),

    ProcName(..), textProcName, unpackProcName,
    NodeName(..), textNodeName, unpackNodeName,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA

import Process

data Test = Test
    { testName :: Text
    , testSteps :: [TestStep]
    }

data TestStep = Spawn ProcName NodeName
              | Send ProcName Text
              | Expect ProcName Regex Text
              | Wait

newtype NodeName = NodeName Text
    deriving (Eq, Ord)

textNodeName :: NodeName -> Text
textNodeName (NodeName name) = name

unpackNodeName :: NodeName -> String
unpackNodeName (NodeName tname) = T.unpack tname
