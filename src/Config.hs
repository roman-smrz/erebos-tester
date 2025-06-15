module Config (
    Config(..),
    findConfig,
    parseConfig,
    getConfigTestFiles,
) where

import Control.Monad.Combinators

import Data.ByteString.Lazy qualified as BS
import Data.Scientific
import Data.Text qualified as T
import Data.YAML

import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Glob

data Config = Config
    { configDir :: FilePath
    , configTool :: Maybe FilePath
    , configTests :: [ Pattern ]
    , configTimeout :: Maybe Scientific
    }
    deriving (Show)

instance FromYAML (FilePath -> Config) where
    parseYAML = withMap "Config" $ \m -> do
        configTool <- (fmap T.unpack <$> m .:? "tool")
        configTests <- (map (compile . T.unpack) <$> foldr1 (<|>)
                [ fmap (:[]) (m .: "tests") -- single pattern
                , m .:? "tests" .!= []      -- list of patterns
                ]
            )
        configTimeout <- fmap fromNumber <$> m .:! "timeout"
        return $ \configDir -> Config {..}

newtype Number = Number { fromNumber :: Scientific }

instance FromYAML Number where
    parseYAML = \case
        Scalar _ (SFloat x) -> return $ Number $ realToFrac x
        Scalar _ (SInt x) -> return $ Number $ fromIntegral x
        node -> typeMismatch "int or float" node

findConfig :: IO (Maybe FilePath)
findConfig = go "."
  where
    name = "erebos-tester.yaml"
    go path = do
        doesFileExist (path </> name) >>= \case
            True -> return $ Just $ path </> name
            False -> doesDirectoryExist (path </> "..") >>= \case
                True -> do
                    parent <- canonicalizePath $ path </> ".."
                    if parent /= path then go parent
                                      else return Nothing
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
