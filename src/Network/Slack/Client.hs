{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.Client where

import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Network.Slack.RTM.Events
import Network.URI
import Network.WebSockets
import Network.Wreq
import Wuss

data SlackAuthResponse = SlackAuthResponse { ok   :: Bool
                                           , url  :: String
                                           , team :: SlackTeam
                                           , self :: SlackUser
                                           }
                         deriving (Eq, Show, Generic)

instance FromJSON SlackAuthResponse

data SlackTeam = SlackTeam { id     :: String
                           , name   :: String
                           , domain :: String
                           }
                 deriving (Eq, Show, Generic)

instance FromJSON SlackTeam

data SlackUser = SlackUser { id   :: String
                           , name :: String
                           }
                 deriving (Eq, Show, Generic)

instance FromJSON SlackUser

client :: ClientApp ()
client connection = do
  message <- receiveData connection
  putStrLn $ show $ (decode message :: Maybe SlackEvent)
  sendClose connection (T.pack "Bye!")

makeWSSClient :: SlackAuthResponse -> IO ()
makeWSSClient SlackAuthResponse { url } = do
  case parseURI url of
    Just uri -> do
      let Just host = uriRegName <$> (uriAuthority uri)
          port = 443
          path = uriPath uri
      runSecureClient host port path client
    Nothing ->
      return ()

authenticate :: T.Text -> IO (Maybe SlackAuthResponse)
authenticate apiKey = do
  let opts = defaults & param "token" .~ [apiKey]
  r <- getWith opts "http://slack.com/api/rtm.connect"
  return $ decode =<< (r ^? responseBody)

connect :: T.Text -> IO ()
connect apiKey = do
  result <- authenticate apiKey
  case result of
    Just a -> makeWSSClient a
    Nothing -> fail $ "Failed to authenticate to Slack."
