{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Semigroup ((<>))
import Network.Slack.Client
import Options.Applicative

data Flags = Flags { apiKey :: String
                   , name   :: String
                   }
             deriving (Show)

parseFlags :: Parser Flags
parseFlags = Flags
  <$> strOption (long "apiKey"
                 <> metavar "APIKEY"
                 <> help "Slack API Key"
                )
  <*> strOption (long "name"
                 <> metavar "NAME"
                 <> showDefault
                 <> value "fred"
                 <> help "Display name for the bot"
                )

start :: Flags -> IO ()
start (Flags apiKey name) = do
  connect apiKey

main :: IO ()
main = execParser opts >>= start
  where opts = info (parseFlags <**> helper) (fullDesc
                                             <> header "fred - A Slack bot in Haskell"
                                             )
