module Test (
    Test(..),
    TestStep(..),

    ProcName(..), textProcName, unpackProcName,
    NodeName(..), textNodeName, unpackNodeName,
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

textProcName :: ProcName -> Text
textProcName (ProcName name) = name

unpackProcName :: ProcName -> String
unpackProcName (ProcName tname) = T.unpack tname

newtype NodeName = NodeName Text
    deriving (Eq, Ord)

textNodeName :: NodeName -> Text
textNodeName (NodeName name) = name

unpackNodeName :: NodeName -> String
unpackNodeName (NodeName tname) = T.unpack tname
