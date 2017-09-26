{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Yaml as Y
import GHC.Generics
import Network.Slack.Client
import System.Environment

data FredConfig = FredConfig { apiKey :: T.Text
                             , name   :: T.Text
                             }
                deriving (Eq, Show, Generic)

instance FromJSON FredConfig

readFredConfig :: FilePath -> IO (Either Y.ParseException FredConfig)
readFredConfig = Y.decodeFileEither

main :: IO ()
main = do
  (f:_) <- getArgs
  config <- readFredConfig f
  case config of
    Left e -> putStrLn $ "Failed to read config: " ++ (show e)
    Right c -> do
      connect (apiKey c)
