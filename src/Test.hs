module Test (
    Test(..),
    TestStep(..),

    ProcName(..), unpackProcName,
    NodeName(..), unpackNodeName,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA

data Test = Test
    { testName :: Text
    , testSteps :: [TestStep]
    }

data TestStep = Spawn ProcName NodeName
              | Send ProcName Text
              | Expect ProcName Regex
              | Wait

newtype ProcName = ProcName Text
    deriving (Eq, Ord)

unpackProcName :: ProcName -> String
unpackProcName (ProcName tname) = T.unpack tname

newtype NodeName = NodeName Text
    deriving (Eq, Ord)

unpackNodeName :: NodeName -> String
unpackNodeName (NodeName tname) = T.unpack tname
