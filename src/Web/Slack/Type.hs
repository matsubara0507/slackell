module Web.Slack.Type
    ( Ok (..)
    , ChannelID
    , UserID
    , TeamID
    , TimeStamp
    , ColorCode
    , Conversation
    , ChannelTopic
    , ChannelType (..)
    , Channel
    , toChannel
    , DirectMessage
    , toDirectMessage
    , Message
    , Reply
    , User
    , Profile
    , Presence
    , UserIdentity
    , TeamIdentity
    ) where

import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                (.:))
import qualified Data.Aeson                    as J
import           Data.Extensible
import qualified Data.HashMap.Strict           as HM
import           Data.Int                      (Int64)
import           Data.Text                     (Text, unpack)
import           Lens.Micro                    ((^.))
import           Network.Simple.OptionalParams (ToHttpApiData' (..))

data Ok a = Ok a | Err Text
  deriving (Show, Eq)

instance FromJSON a => FromJSON (Ok a) where
  parseJSON = J.withObject "Result e a" $ \obj -> case HM.lookup "ok" obj of
    Just (J.Bool True)  -> Ok <$> parseJSON (J.Object obj)
    Just (J.Bool False) -> Err <$> obj .: "error"
    _                   -> fail "key `ok:bool` is not found."

instance ToJSON a => ToJSON (Ok a) where
  toJSON (Ok a) = case toJSON a of
    J.Object obj -> J.Object $ HM.insert "ok" (J.Bool True) obj
    value        -> J.Object $ HM.fromList [("ok", J.Bool True), ("value", value)]
  toJSON (Err e)  = J.Object $ HM.fromList [("ok", J.Bool False), ("error", J.String e)]

type ChannelID = Text
type UserID = Text
type TeamID = Text

type TimeStamp = Text
type ColorCode = Text

type Conversation = Record
  '[ "id"                    >: ChannelID
   , "name"                  >: Maybe Text
   , "is_channel"            >: Maybe Bool
   , "is_group"              >: Maybe Bool
   , "is_im"                 >: Maybe Bool
   , "created"               >: Maybe Int64
   , "creator"               >: Maybe UserID
   , "is_archived"           >: Maybe Bool
   , "is_general"            >: Maybe Bool
   , "unlinked"              >: Maybe Int
   , "name_normalized"       >: Maybe Text
   , "is_shared"             >: Maybe Bool
   , "is_ext_shared"         >: Maybe Bool
   , "is_org_shared"         >: Maybe Bool
   , "pending_shared"        >: Maybe [Text]
   , "is_pending_ext_shared" >: Maybe Bool
   , "is_member"             >: Maybe Bool
   , "is_private"            >: Maybe Bool
   , "is_mpim"               >: Maybe Bool
   , "topic"                 >: Maybe ChannelTopic
   , "purpose"               >: Maybe ChannelTopic
   , "previous_names"        >: Maybe [Text]
   , "num_members"           >: Maybe Int
   , "user"                  >: Maybe UserID
   , "is_user_deleted"       >: Maybe Bool
   , "priority"              >: Maybe Int
   , "locale"                >: Maybe Text
   ]

type ChannelTopic = Record
  '[ "value"    >: Text
   , "creator"  >: UserID
   , "last_set" >: Int64
   ]

data ChannelType
    = PublicChannel
    | PrivateChannel
    | Mpim
    | Im
    deriving (Show, Eq)

instance ToHttpApiData' ChannelType where
  toQueryParam' PublicChannel  = Just "public_channel"
  toQueryParam' PrivateChannel = Just "private_channel"
  toQueryParam' Mpim           = Just "mpim"
  toQueryParam' Im             = Just "im"

type Channel = Record
  '[ "id"                    >: ChannelID
   , "name"                  >: Text
   , "created"               >: Int64
   , "creator"               >: UserID
   , "is_archived"           >: Bool
   , "is_general"            >: Bool
   , "unlinked"              >: Int
   , "name_normalized"       >: Text
   , "is_shared"             >: Bool
   , "is_ext_shared"         >: Bool
   , "is_org_shared"         >: Bool
   , "pending_shared"        >: [Text]
   , "is_pending_ext_shared" >: Bool
   , "is_member"             >: Bool
   , "is_private"            >: Bool
   , "topic"                 >: ChannelTopic
   , "purpose"               >: ChannelTopic
   , "previous_names"        >: Maybe [Text]
   , "num_members"           >: Maybe Int
   ]

toChannel :: Conversation -> Maybe Channel
toChannel conv = hsequence
    $ #id                    <@=> Just (conv ^. #id)
   <: #name                  <@=> conv ^. #name
   <: #created               <@=> conv ^. #created
   <: #creator               <@=> conv ^. #creator
   <: #is_archived           <@=> conv ^. #is_archived
   <: #is_general            <@=> conv ^. #is_general
   <: #unlinked              <@=> conv ^. #unlinked
   <: #name_normalized       <@=> conv ^. #name_normalized
   <: #is_shared             <@=> conv ^. #is_shared
   <: #is_ext_shared         <@=> conv ^. #is_ext_shared
   <: #is_org_shared         <@=> conv ^. #is_org_shared
   <: #pending_shared        <@=> conv ^. #pending_shared
   <: #is_pending_ext_shared <@=> conv ^. #is_pending_ext_shared
   <: #is_member             <@=> conv ^. #is_member
   <: #is_private            <@=> conv ^. #is_private
   <: #topic                 <@=> conv ^. #topic
   <: #purpose               <@=> conv ^. #purpose
   <: #previous_names        <@=> Just (conv ^. #previous_names)
   <: #num_members           <@=> Just (conv ^. #num_members)
   <: nil

type DirectMessage = Record
  '[ "id"              >: ChannelID
   , "created"         >: Int64
   , "is_im"           >: Bool
   , "is_org_shared"   >: Bool
   , "user"            >: UserID
   , "is_user_deleted" >: Maybe Bool
   , "priority"        >: Int
   ]

toDirectMessage :: Conversation -> Maybe DirectMessage
toDirectMessage conv = hsequence
    $ #id              <@=> Just (conv ^. #id)
   <: #created         <@=> conv ^. #created
   <: #is_im           <@=> conv ^. #is_im
   <: #is_org_shared   <@=> conv ^. #is_org_shared
   <: #user            <@=> conv ^. #user
   <: #is_user_deleted <@=> Just (conv ^. #is_user_deleted)
   <: #priority        <@=> conv ^. #priority
   <: nil

type Message = Record
  '[ "type"           >: Text
   , "user"           >: UserID
   , "text"           >: Text
   , "thread_ts"      >: Maybe TimeStamp
   , "parent_user_id" >: Maybe UserID
   , "reply_count"    >: Maybe Int
   , "replies"        >: Maybe [Reply]
   , "subscribed"     >: Maybe Bool
   , "last_read"      >: Maybe TimeStamp
   , "unread_count"   >: Maybe Int
   , "ts"             >: TimeStamp
   ]

type Reply = Record
  '[ "user" >: UserID
   , "ts"   >: TimeStamp
   ]

type User = Record
  '[ "id"                  >: UserID
   , "team_id"             >: TeamID
   , "name"                >: Text
   , "deleted"             >: Bool
   , "color"               >: Maybe ColorCode
   , "real_name"           >: Maybe Text
   , "tz"                  >: Maybe Text
   , "tz_label"            >: Maybe Text
   , "tz_offset"           >: Maybe Int
   , "profile"             >: Profile
   , "is_admin"            >: Maybe Bool
   , "is_owner"            >: Maybe Bool
   , "is_primary_owner"    >: Maybe Bool
   , "is_restricted"       >: Maybe Bool
   , "is_ultra_restricted" >: Maybe Bool
   , "is_bot"              >: Bool
   , "updated"             >: Int64
   , "is_app_user"         >: Maybe Bool
   , "has_2fa"             >: Maybe Bool
   ]

type Profile = Record
  '[ "avatar_hash"             >: Text
   , "status_text"             >: Maybe Text
   , "status_emoji"            >: Maybe Text
   , "real_name"               >: Text
   , "display_name"            >: Text
   , "real_name_normalized"    >: Text
   , "display_name_normalized" >: Text
   , "email"                   >: Maybe Text
   , "image_original"          >: Maybe Text
   , "image_24"                >: Text
   , "image_32"                >: Text
   , "image_48"                >: Text
   , "image_72"                >: Text
   , "image_192"               >: Text
   , "image_512"               >: Text
   , "team"                    >: Maybe Text
   ]

data Presence
  = Active
  | Away
  deriving (Show, Eq)

instance FromJSON Presence where
  parseJSON = J.withText "Presence" $ \case
    "active" -> pure Active
    "away"   -> pure Away
    txt      -> fail $ "\"" <> unpack txt <> "\" is not Presence"

instance ToJSON Presence where
  toJSON Active = J.String "active"
  toJSON Away   = J.String "away"

type UserIdentity = Record
  '[ "id"        >: UserID
   , "name"      >: Text
   , "email"     >: Maybe Text
   , "image_24"  >: Maybe Text
   , "image_32"  >: Maybe Text
   , "image_48"  >: Maybe Text
   , "image_72"  >: Maybe Text
   , "image_192" >: Maybe Text
   ]

type TeamIdentity = Record
  '[ "id"   >: TeamID
   , "name" >: Maybe Text
   ]
