{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Semigroup ((<>))
import Network.Slack.Client
import Options.Applicative

data Flags = Flags { apiKey :: String
                   }
             deriving (Show)

parseFlags :: Parser Flags
parseFlags = Flags
  <$> strOption (long "apiKey"
                 <> metavar "APIKEY"
                 <> help "Slack API Key"
                )

messageHandler :: SlackEvent -> IO (Maybe String)
messageHandler (Message _ _ _ _ _ _) = return $ Just "Yo"

start :: Flags -> IO ()
start (Flags apiKey) = do
  connect messageHandler apiKey

main :: IO ()
main = execParser opts >>= start
  where opts = info (parseFlags <**> helper) (fullDesc
                                             <> header "fred - A Slack bot in Haskell"
                                             )
