{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Slack.RTM.Events where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import qualified Data.Map as M
import GHC.Generics

dropPrefix :: String -> String -> String
dropPrefix prefix = (drop (length prefix)) . (map toLower) -- For sure there's a better way :)

newtype SlackId = SlackId String deriving (Generic, Show)

instance FromJSON SlackId

data SlackEvent = AccountsChanged
                | BotAdded { _seBot  :: BotInfo
                           }
                | BotChanged { _seBot  :: BotInfo
                             }
                | ChannelArchive { _seChannelId :: SlackId
                                 , _seUser      :: SlackId
                                 }
                | ChannelCreated { _seChannel   :: ChannelInfo }
                | ChannelDeleted { _seChannelId :: SlackId }
                | ChannelHistoryChanged { _seLatest  :: String
                                        , _seTs      :: String
                                        , _seEventTs :: String
                                        }
                | ChannelJoined { _seChannelId :: SlackId }
                | ChannelLeft { _seChannelId :: SlackId }
                | ChannelMarked { _seChannelId :: SlackId
                                , _seTs        :: String
                                }
                | ChannelRename { _seChannelRename :: ChannelRenameInfo }
                | ChannelUnarchive { _seChannelId :: SlackId
                                   , _seUser      :: SlackId
                                   }
                | CommandsChanged { _seEventTs :: String }
                | DndUpdated { _seUser      :: SlackId
                             , _seDndStatus :: DndStatus
                             }
                | DndUpdatedUser { _seUser      :: SlackId
                                 , _seDndStatus :: DndStatus
                                 }
                | EmailDomainChanged { _seEmailDomain :: String
                                     , _seEventTs     :: String
                                     }
                | EmojiAdd { _seName    :: String
                           , _seValue   :: String
                           , _seEventTs :: String
                           }
                | EmojiRemove { _seNames   :: [String]
                              , _seEventTs :: String
                              }
                | FileChange { _seFile :: FileInfo }
                | Goodbye
                | GroupArchive { _seChannelId :: SlackId }
                | GroupClose { _seUser      :: SlackId
                             , _seChannelId :: SlackId
                             }
                | GroupHistoryChanged { _seLatest  :: String
                                      , _seTs      :: String
                                      , _seEventTs :: String
                                      }
                | GroupJoined { _seGroupId :: SlackId }
                | GroupLeft { _seGroupId :: SlackId }
                | GroupMarked { _seGroupId :: SlackId
                              , _seTs      :: String
                              }
                | GroupOpen { _seUser    :: SlackId
                            , _seGroupId :: SlackId }
                | GroupRename { _seGroupRename :: ChannelRenameInfo }
                | GroupUnarchive { _seChannelId :: SlackId
                                 , _seUser      :: SlackId
                                 }
                | Hello
                deriving Show

instance FromJSON SlackEvent where
    parseJSON = withObject "SlackEvent" $ \o -> do
        eventType <- (o .: "type" :: Parser String)
        case eventType of
          "accounts_changed"        -> return AccountsChanged
          "bot_added"               -> BotAdded <$> o .: "bot"
          "bot_changed"             -> BotChanged <$> o .: "bot"
          "channel_archive"         -> ChannelArchive <$> o .: "channel" <*> o .: "user"
          "channel_created"         -> ChannelCreated <$> o .: "channel"
          "channel_deleted"         -> ChannelDeleted <$> o .: "channel"
          "channel_history_changed" -> ChannelHistoryChanged <$> o .: "latest" <*> o .: "ts" <*> o .: "event_ts"
          "channel_joined"          -> ChannelJoined <$> o .: "channel"
          "channel_left"            -> ChannelLeft <$> o .: "channel"
          "channel_marked"          -> ChannelMarked <$> o .: "channel" <*> o .: "ts"
          "channel_rename"          -> ChannelRename <$> o .: "channel"
          "channel_unarchive"       -> ChannelUnarchive <$> o .: "channel" <*> o .: "user"
          "commands_changed"        -> CommandsChanged <$> o .: "event_ts"
          "dnd_updated"             -> DndUpdated <$> o .: "user" <*> o .: "dnd_status"
          "dnd_updated_user"        -> DndUpdatedUser <$> o .: "user" <*> o .: "dnd_status"
          "email_domain_changed"    -> EmailDomainChanged <$> o .: "email_domain" <*> o .: "event_ts"
          "emoji_changed"           -> do
                                        subtype <- (o .: "subtype" :: Parser String)
                                        case subtype of
                                          "add"    -> EmojiAdd <$> o .: "name" <*> o .: "value" <*> o .: "event_ts"
                                          "remove" -> EmojiRemove <$> o .: "names" <*> o .: "event_ts"
                                          _        -> fail $ "Unknown emoji_changed subtype: " ++ subtype
          "file_change"             -> FileChange <$> o .: "file"
          "goodbye"                 -> return Goodbye
          "group_archive"           -> GroupArchive <$> o .: "channel"
          "group_close"             -> GroupClose <$> o .: "user" <*> o .: "channel"
          "group_history_changed"   -> GroupHistoryChanged <$> o .: "latest" <*> o .: "ts" <*> o .: "event_ts"
          "group_joined"            -> GroupJoined <$> o .: "channel"
          "group_left"              -> GroupLeft <$> o .: "channel"
          "group_marked"            -> GroupMarked <$> o .: "channel" <*> o .: "ts"
          "group_open"              -> GroupOpen <$> o .: "user" <*> o .: "channel"
          "group_rename"            -> GroupRename <$> o .: "channel"
          "group_unarchive"         -> GroupUnarchive <$> o .: "channel" <*> o .: "user"
          "hello"                   -> return Hello
          _                         -> fail $ "Unknown event type: " ++ eventType

data BotInfo = BotInfo { _biId    :: SlackId
                       , _biAppId :: SlackId
                       , _biName  :: String
                       , _biIcons :: M.Map String String
                       }
                       deriving (Generic, Show)

instance FromJSON BotInfo where
     parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix "_bi" }

data ChannelInfo = ChannelInfo { _ciId      :: SlackId 
                               , _ciName    :: String
                               , _ciCreated :: Int -- TODO: Use proper datetime type
                               , _ciCreator :: SlackId
                               }
                               deriving (Generic, Show)

instance FromJSON ChannelInfo where
      parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix "_ci" }

data ChannelRenameInfo = ChannelRenameInfo { _criId      :: SlackId
                                           , _criName    :: String
                                           , _criCreated :: Int -- TODO: Use proper datetime type
                                           }
                                           deriving (Generic, Show)

instance FromJSON ChannelRenameInfo where
      parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix "_cri" }

data DndStatus = DndStatus { _dsDnd_enabled       :: Bool
                           , _dsNext_dnd_start_ts :: Int -- TODO: Use proper datetime type
                           , _dsNext_dnd_end_ts   :: Int -- TODO: Use proper datetime type
                           , _dsSnooze_enabled    :: Maybe Bool
                           , _dsSnooze_endtime    :: Maybe Int -- TODO: Use proper datetime type
                           }
                           deriving (Generic, Show)

instance FromJSON DndStatus where
      parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix "_ds" }

data FileInfo = FileInfo { _fiId        :: SlackId
                         , _fiCreated   :: Int -- TODO: Use proper datetime type
                         , _fiTimestamp :: Int -- TODO: Use proper datetime type
                         , _fiName      :: Maybe String
                         , _fiTitle     :: String
                         , _fiMimetype  :: String
                         , _fiFiletype  :: String
                         , _fiPretty_type :: String
                         , _fiUser        :: SlackId
                         , _fiMode        :: String
                         , _fiEditable    :: Bool
                         , _fiIs_external :: Bool
                         , _fiExternal_type :: String
                         , _fiUsername      :: String
                         , _fiSize          :: Int
                         , _fiUrl_private   :: String
                         , _fiUrl_private_download :: String
                         , _fiPermalink            :: String
                         , _fiPermalink_public     :: String
                         , _fiEdit_link            :: String
                         , _fiIs_public            :: Bool
                         , _fiPublic_url_shared    :: Bool
                         , _fiDisplay_as_bot       :: Bool
                         , _fiChannels             :: [SlackId]
                         , _fiGroups               :: [SlackId]
                         , _fiIms                  :: [SlackId]
                         , _fiIs_Starred           :: Bool
                         , _fiPinned_to            :: [SlackId]
                         }
                         deriving (Generic, Show)

instance FromJSON FileInfo where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix "_fi" }