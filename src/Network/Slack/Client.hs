{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.Client where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (forever, void)
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

data SlackUser = SlackUser { id   :: UserId
                           , name :: String
                           }
                 deriving (Eq, Show, Generic)

instance FromJSON SlackUser

data Typing = Typing { _tId        :: Integer
                     , _tType      :: String
                     , _tChannelId :: ChannelId
                     }
              deriving (Eq, Show, Generic)

instance ToJSON Typing where
  toJSON (Typing i t c) = object ["id" AT..= i, "type" AT..= t, "channel" AT..= c]

data Reply = Reply { _rId :: Integer
                   , _rType :: String
                   , _rChannelId :: ChannelId
                   , _rText :: String
                   }
             deriving (Eq, Show, Generic)

instance ToJSON Reply where
  toJSON (Reply i t c x) = object ["id" AT..= i, "type" AT..= t, "channel" AT..= c, "text" AT..= x]

sendTyping :: ChannelId -> Integer -> Connection -> IO ()
sendTyping chan count connection = sendTextData connection $ encode typingMsg
  where typingMsg = Typing { _tId = count, _tType = "typing", _tChannelId = chan }

incCounter :: TVar Integer -> IO Integer
incCounter counter = atomically $ do
  c <- readTVar counter
  writeTVar counter (c + 1)
  return c

sendMessage :: ChannelId -> Integer -> String -> Connection -> IO ()
sendMessage chan count text connection = sendTextData connection $ encode msg
  where msg = Reply { _rId = count, _rType = "message", _rChannelId = chan, _rText = text }

getReply :: String -> String
getReply _ = ":reversed_hand_with_middle_finger_extended:"

reply :: SlackEvent -> TVar Integer -> Connection -> IO ()
reply Message { _seChannelId = chan, _seText = txt } counter connection =
  let reply = getReply txt
  in
    incCounter counter
    >>= \c -> sendTyping chan c connection
              >> threadDelay 500000
              >> incCounter counter
              >>= \c -> sendMessage chan c reply connection

handleEvent :: SlackEvent -> TVar Integer -> Connection -> IO ()
handleEvent event@Message { _seChannelId = chan } counter connection = do
  reply event counter connection
handleEvent event _ _ = do
  putStrLn $ show event

client :: TVar Integer -> Connection -> IO ()
client counter connection = forever $ do
  message <- receiveData connection
  case (eitherDecode message :: Either String SlackEvent) of
    Left _  -> return ()
    Right m -> void $ async (handleEvent m counter connection)

makeWSSClient :: SlackAuthResponse -> IO ()
makeWSSClient SlackAuthResponse { url } = do
  case parseURI url of
    Just uri -> do
      let Just host = uriRegName <$> (uriAuthority uri)
          port = 443
          path = uriPath uri
      counter <- atomically $ newTVar 0
      runSecureClient host port path (client counter)
    Nothing ->
      return ()

authenticate :: String -> IO (Maybe SlackAuthResponse)
authenticate apiKey = do
  let opts = defaults & param "token" .~ [T.pack apiKey]
  r <- getWith opts "http://slack.com/api/rtm.connect"
  return $ decode =<< (r ^? responseBody)

connect :: String -> IO ()
connect apiKey = do
  result <- authenticate apiKey
  case result of
    Just a -> makeWSSClient a
    Nothing -> fail $ "Failed to authenticate to Slack."
