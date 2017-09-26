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

data ItemType = Channel
              | Group
              | File
              | FileComments
                 deriving (Show)

instance FromJSON ItemType where
  parseJSON = withText "ItemType" $ \t -> do
    case t of
      "C"  -> return Channel
      "G"  -> return Group
      "F"  -> return File
      "Fc" -> return FileComments

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
                | SlackError { _seErrorCode    :: Int
                             , _seErrorMessage :: String
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
                | IMClose { _seUser      :: SlackId
                          , _seChannelId :: SlackId
                          }
                | IMCreated { _seUser    :: SlackId
                            , _seChannel :: ChannelInfo
                            }
                | IMHistoryChanged { _seLatest  :: String
                                   , _seTs      :: String
                                   , _seEventTs :: String
                                   }
                | IMMarked { _seChannelId :: SlackId
                           , _seTs :: String
                           }
                | IMOpen { _seUser      :: SlackId
                         , _seChannelId :: SlackId
                         }
                | ManualPresenceChange { _sePresence :: String }
                | MemberJoinedChannel { _seUser        :: SlackId
                                      , _seChannelId   :: SlackId
                                      , _seItemType    :: ItemType
                                      , _seInviter     :: Maybe SlackId
                                      }
                | MemberLeftChannel { _seUser        :: SlackId
                                    , _seChannelId   :: SlackId
                                    , _seChannelType :: ItemType
                                    }
                | Message { _seTs        :: String
                          , _seUser      :: SlackId
                          , _seChannelId :: SlackId
                          , _seText      :: String
                          , _seEdited    :: EditInfo
                          }
                | BotMessage { _seTs       :: String
                             , _seText     :: String
                             , _seBotId    :: SlackId
                             , _seUsername :: String
                             , _seIcons    :: [String]
                             }
                | ChannelArchiveMessage { _seTs   :: String
                                        , _seUser :: SlackId
                                        , _seText :: String
                                        }
                | ChannelJoinMessage { _seTs      :: String
                                     , _seUser    :: SlackId
                                     , _seText    :: String
                                     , _seInviter :: Maybe SlackId
                                     }
                | ChannelLeaveMessage { _seTs   :: String
                                      , _seUser :: SlackId
                                      , _seText :: String
                                      }
                | ChannelNameMessage { _seTs      :: String
                                     , _seUser    :: SlackId
                                     , _seText    :: String
                                     , _seOldName :: String
                                     , _seName    :: String
                                     }
                | ChannelPurposeMessage { _seTs      :: String
                                        , _seUser    :: SlackId
                                        , _seText    :: String
                                        , _sePurpose :: String
                                        }
                | ChannelTopicMessage { _seTs    :: String
                                      , _seUser  :: SlackId
                                      , _seText  :: String
                                      , _seTopic :: String
                                      }
                | ChannelUnarchiveMessage { _seTs   :: String
                                          , _seUser :: SlackId
                                          , _seText :: String
                                          }
                | FileCommentMessage { _seTs      :: String
                                     , _seUser    :: SlackId
                                     , _seText    :: String
                                     , _seFile    :: FileInfo
                                     , _seComment :: Object
                                     }
                | FileMentionMessage { _seTs   :: String
                                     , _seUser :: SlackId
                                     , _seText :: String
                                     , _seFile :: FileInfo
                                     }
                | FileShareMessage { _seTs     :: String
                                   , _seUser   :: SlackId
                                   , _seText   :: String
                                   , _seFile   :: FileInfo
                                   , _seUpload :: Bool
                                   }
                | GroupArchiveMessage { _seTs      :: String
                                      , _seUser    :: SlackId
                                      , _seText    :: String
                                      , _seMembers :: [SlackId]
                                      }
                | GroupJoinMessage { _seTs   :: String
                                   , _seUser :: SlackId
                                   , _seText :: String
                                   }
                | GroupLeaveMessage { _seTs   :: String
                                    , _seUser :: SlackId
                                    , _seText :: String
                                    }
                | GroupNameMessage { _seTs      :: String
                                   , _seUser    :: SlackId
                                   , _seText    :: String
                                   , _seOldName :: String
                                   , _seName    :: String
                                   }
                | GroupPurposeMessage { _seTs      :: String
                                      , _seUser    :: SlackId
                                      , _seText    :: String
                                      , _sePurpose :: String
                                      }
                | GroupTopicMessage { _seTs    :: String
                                    , _seUser  :: SlackId
                                    , _seText  :: String
                                    , _seTopic :: String
                                    }
                | GroupUnarchiveMessage { _seTs   :: String
                                        , _seUser :: SlackId
                                        , _seText :: String
                                        }
                | MeMessage { _seTs        :: String
                            , _seUser      :: SlackId
                            , _seText      :: String
                            , _seChannelId :: SlackId
                            }
                | MessageChanged { _seTs      :: String
                                 , _seUser    :: SlackId
                                 , _seText    :: String
                                 , _seHidden  :: Bool
                                 , _seMessage :: MessageInfo
                                 }
                | MessageDeleted { _seTs        :: String
                                 , _seChannelId :: SlackId
                                 , _seDeletedTs :: String
                                 , _seHidden    :: Bool
                                 }
                | MessageReplied { _seTs        :: String
                                 , _seChannelId :: SlackId
                                 , _seEventTs   :: String
                                 , _seHidden    :: Bool
                                 , _seMessage   :: MessageInfo
                                 }
                | PinnedItemMessage { _seTs        :: String
                                    , _seUser      :: SlackId
                                    , _seText      :: String
                                    , _seChannelId :: SlackId
                                    , _seItemType  :: ItemType
                                    , _seItem      :: String
                                    }
                | ReplyBroadcastMessage { _seTs          :: String
                                        , _seUser        :: SlackId
                                        , _seChannelId   :: SlackId
                                        , _seEventTs     :: String
                                        , _seAttachments :: [Object]
                                        }
                | UnpinnedItemMessage { _seTs        :: String
                                      , _seUser      :: SlackId
                                      , _seText      :: String
                                      , _seChannelId :: SlackId
                                      , _seItemType  :: ItemType
                                      , _seItem      :: String
                                      }
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
          "error"                   -> SlackError <$> o .: "code" <*> o .: "msg"
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
          "im_close"                -> IMClose <$> o .: "user" <*> o .: "channel"
          "im_created"              -> IMCreated <$> o .: "user" <*> o .: "channel"
          "im_history_changed"      -> IMHistoryChanged <$> o .: "latest" <*> o .: "ts" <*> o .: "event_ts"
          "im_marked"               -> IMMarked <$> o .: "channel" <*> o .: "ts"
          "im_open"                 -> IMOpen <$> o .: "user" <*> o .: "channel"
          "manual_presence_change"  -> ManualPresenceChange <$> o .: "presence"
          "member_joined_channel"   -> MemberJoinedChannel
                                       <$> o .: "user"
                                       <*> o .: "channel"
                                       <*> o .: "channel_type"
                                       <*> o .: "inviter"
          "member_left_channel"     -> MemberLeftChannel <$> o .: "user" <*> o .: "channel" <*> o .: "channel_type"
          "message"                 -> parseMessageEvent o
          _                         -> fail $ "Unknown event type: " ++ eventType


parseMessageEvent :: Object -> Parser SlackEvent
parseMessageEvent o = do
  subtype <- (o .: "subtype" :: Parser (Maybe String))
  case subtype of
    Nothing                  -> Message
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "channel"
                                <*> o .: "text"
                                <*> o .: "edited"
    Just "bot_message"       -> BotMessage
                                <$> o .: "ts"
                                <*> o .: "text"
                                <*> o .: "bot_id"
                                <*> o .: "username"
                                <*> o .: "icons"
    Just "channel_archive"   -> ChannelArchiveMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
    Just "channel_join"      -> ChannelJoinMessage
                                <$> o .: "ts"
                                <*> o .: "text"
                                <*> o .: "user"
                                <*> o .: "inviter"
    Just "channel_leave"     -> ChannelLeaveMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
    Just "channel_name"      -> ChannelNameMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
                                <*> o .: "old_name"
                                <*> o .: "name"
    Just "channel_purpose"   -> ChannelPurposeMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
                                <*> o .: "purpose"
    Just "channel_topic"     -> ChannelTopicMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
                                <*> o .: "topic"
    Just "channel_unarchive" -> ChannelUnarchiveMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
    Just "file_comment"      -> FileCommentMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
                                <*> o .: "file"
                                <*> o .: "comment"
    Just "file_mention"      -> FileMentionMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
                                <*> o .: "file"
    Just "file_share"        -> FileShareMessage
                                <$> o .: "ts"
                                <*> o .: "user"
                                <*> o .: "text"
                                <*> o .: "file"
                                <*> o .: "upload"
--  Just "group_archive"     -> undefined
--  Just "group_join"        -> undefined
--  Just "group_leave"       -> undefined
--  Just "group_name"        -> undefined
--  Just "group_purpose"     -> undefined
--  Just "group_topic"       -> undefined
--  Just "group_unarchive"   -> undefined
--  Just "me_message"        -> undefined
--  Just "message_changed"   -> undefined
--  Just "message_deleted"   -> undefined
--  Just "message_replied"   -> undefined
--  Just "pinned_item"       -> undefined
--  Just "reply_broadcast"   -> undefined
--  Just "unpinned_item"     -> undefined
    Just s                   -> fail $ "Unknown message subtype: " ++ s

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

data EditInfo = EditInfo { _eiUser :: SlackId
                         , _eiTs   :: String
                         }
                deriving (Generic, Show)

instance FromJSON EditInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix "_ei" }

data Reaction = Reaction { _rName  :: String
                         , _rCount :: Int
                         , _rUsers :: [SlackId]
                         }
                deriving (Generic, Show)

instance FromJSON Reaction where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix "_r" }

data MessageInfo = MessageInfo { _mChannelId :: SlackId
                               , _mUser      :: SlackId
                               , _mText      :: String
                               , _mTs        :: String
                               , _mEdited    :: Maybe EditInfo
                               , _mIsStarred :: Maybe Bool
                               , _mPinnedTo  :: Maybe [SlackId]
                               , _mReactions :: Maybe [Reaction]
                               }
                 deriving (Generic, Show)

instance FromJSON MessageInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix "_m" }
