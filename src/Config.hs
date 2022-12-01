{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config(..),
    findConfig,
    parseConfig,
) where

import Control.Monad.Combinators

import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as T
import Data.YAML

import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Glob

data Config = Config
    { configTool :: Maybe FilePath
    , configTests :: [Pattern]
    }
    deriving (Show)

instance Semigroup Config where
    a <> b = Config
        { configTool = maybe (configTool b) Just (configTool a)
        , configTests = configTests a ++ configTests b
        }

instance Monoid Config where
    mempty = Config
        { configTool = Nothing
        , configTests = []
        }

instance FromYAML Config where
    parseYAML = withMap "Config" $ \m -> Config
        <$> (fmap T.unpack <$> m .:? "tool")
        <*> (map (compile . T.unpack) <$> foldr1 (<|>)
                [ fmap (:[]) (m .: "tests") -- single pattern
                , m .:? "tests" .!= []      -- list of patterns
                ]
            )

findConfig :: IO (Maybe FilePath)
findConfig = go "."
  where
    name = "erebos-tester.yaml"
    go path = do
        doesFileExist (path </> name) >>= \case
            True -> return $ Just $ path </> name
            False -> doesDirectoryExist (path </> "..") >>= \case
                True -> go (path </> "..")
                False -> return Nothing

parseConfig :: FilePath -> IO Config
parseConfig path = do
    contents <- BS.readFile path
    case decode1 contents of
        Left (pos, err) -> do
            putStr $ prettyPosWithSource pos contents err
            exitFailure
        Right conf -> return conf
