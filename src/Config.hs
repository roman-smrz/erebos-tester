module Config (
    Config(..),
    ConfigTool(..),
    findConfig,
    parseConfig,
    getConfigTestFiles,
) where

import Control.Monad.Combinators

import Data.ByteString.Lazy qualified as BS
import Data.Scientific
import Data.Text (Text)
import Data.Text qualified as T
import Data.YAML

import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Glob

data Config = Config
    { configDir :: FilePath
    , configTool :: Maybe ConfigTool
    , configTests :: [ Pattern ]
    , configSelect :: Maybe [ Text ]
    , configExclude :: [ Text ]
    , configTimeout :: Maybe Scientific
    }
    deriving (Show)

data ConfigTool
    = ToolPath FilePath
    | ToolCabal Text
    deriving (Show)

instance FromYAML (FilePath -> Config) where
    parseYAML = withMap "Config" $ \m -> do
        configTool <- foldr1 (<|>)
            [ Just . ToolPath . T.unpack <$> m .: "tool" -- tool path as string
            , Just <$> m .: "tool"
            , return Nothing
            ]
        configTests <- (map (compile . T.unpack) <$> foldr1 (<|>)
                [ fmap (:[]) (m .: "tests") -- single pattern
                , m .:? "tests" .!= []      -- list of patterns
                ]
            )
        configSelect <- foldr1 (<|>)
            [ fmap (Just . (: [])) (m .: "select") -- single item
            , m .:? "select"                       -- list of items
            ]
        configExclude <- foldr1 (<|>)
            [ fmap (: []) (m .: "exclude") -- single item
            , m .:? "exclude" .!= []       -- list of items
            ]
        configTimeout <- fmap fromNumber <$> m .:! "timeout"
        return $ \configDir -> Config {..}

newtype Number = Number { fromNumber :: Scientific }

instance FromYAML Number where
    parseYAML = \case
        Scalar _ (SFloat x) -> return $ Number $ realToFrac x
        Scalar _ (SInt x) -> return $ Number $ fromIntegral x
        node -> typeMismatch "int or float" node

instance FromYAML ConfigTool where
    parseYAML = withMap "tool specification" $ \m -> foldr1 (<|>)
        [ ToolPath . T.unpack <$> m .: "path"
        , ToolCabal <$> m .: "cabal"
        ]

findConfig :: Bool -> IO (Maybe FilePath)
findConfig tryParents = go "."
  where
    name = "erebos-tester.yaml"
    go path = do
        doesFileExist (path </> name) >>= \case
            True -> return $ Just $ path </> name
            False | tryParents -> doesDirectoryExist (path </> "..") >>= \case
                True -> do
                    parent <- canonicalizePath $ path </> ".."
                    if parent /= path then go parent
                                      else return Nothing
                False -> return Nothing
            False -> return Nothing

parseConfig :: FilePath -> IO Config
parseConfig path = do
    contents <- BS.readFile path
    case decode1 contents of
        Left (pos, err) -> do
            putStr $ prettyPosWithSource pos contents err
            exitFailure
        Right conf -> return $ conf $ takeDirectory path

getConfigTestFiles :: Config -> IO [ FilePath ]
getConfigTestFiles config = concat <$> mapM (flip globDir1 $ configDir config) (configTests config)
