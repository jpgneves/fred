{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.Client where

import Control.Lens
import Control.Monad (forever)
import Data.Aeson
import qualified Data.Aeson.Types as AT
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

data Typing = Typing { _tId        :: Int
                     , _tType      :: String
                     , _tChannelId :: SlackId
                     }
              deriving (Eq, Show, Generic)

instance ToJSON Typing where
  toJSON (Typing i t c) = object ["id" AT..= i, "type" AT..= t, "channel" AT..= c]

handleEvent :: SlackEvent -> ClientApp ()
handleEvent Message { _seChannelId = chan } connection = do
  sendTextData connection $ encode typingMsg
  where typingMsg = Typing { _tId = 1, _tType = "typing", _tChannelId = chan }
handleEvent event _ = do
  putStrLn $ show event

client :: ClientApp ()
client connection = forever $ do
  message <- receiveData connection
  case (eitherDecode message :: Either String SlackEvent) of
    Left _  -> return ()
    Right m -> handleEvent m connection

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
