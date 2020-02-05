module Web.Slack.WebAPI.Conversations
    ( archive
    , CloseResule
    , close
    , CreateParams
    , create
    , Messages
    , HistoryParams
    , history
    , InfoParams
    , info
    , invite
    , join
    , kick
    , leave
    , Conversations
    , ListParams
    , list
    , Members
    , MembersParams
    , members
    , OpenResult
    , OpenParams
    , open
    , rename
    , Replies
    , RepliesParams
    , replies
    , setPurpose
    , setTopic
    , unarchive
    ) where

import           Data.Extensible
import           Data.Text                 (Text)
import           Network.HTTP.Req
import           Network.Simple            (Client (..), OptionalParams,
                                            buildRequestParams, toQueryParam')
import           Web.Slack.Type            as Slack
import           Web.Slack.WebAPI.Internal

buildUrl :: Client c => c -> Text -> Url (ClientScheme c)
buildUrl c path = baseUrl c /: "api" /: "conversations." <> path

-- | API for conversations

archive
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (WebApiResponse (Record '[ "ok" >: Bool ]))
archive client cid =
  buildPostApi client (buildUrl client "archive") ("channel" =: cid)

type CloseResule = Record
  '[ "no_op"          >: Maybe Bool
   , "already_closed" >: Maybe Bool
   ]

close
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (WebApiResponse CloseResule)
close client cid = buildPostApi client (buildUrl client "close") ("channel" =: cid)

type CreateParams = OptionalParams
  '[ "is_private" >: Bool
   , "user_ids"   >: [Slack.UserID]
   ]

create
  :: (MonadHttp m, Client c)
  => c
  -> Text -- ^ channel name
  -> CreateParams
  -> m (WebApiResponse (Record '[ "channel" >: Slack.Conversation ]))
create client cname =
  buildPostApi client (buildUrl client "create") . ("name" =: cname <>) . buildRequestParams

type Messages = Record
  '[ "messages"          >: [Slack.Message]
   , "has_more"          >: Bool
   , "pin_count"         >: Int
   , "response_metadata" >: Maybe (Record NextCursor)
   ]

type HistoryParams = OptionalParams
  '[ "cursor"    >: Text
   , "inclusive" >: Bool
   , "latest"    >: Slack.TimeStamp
   , "limit"     >: Int
   , "oldest"    >: Slack.TimeStamp
   ]

history
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> HistoryParams
  -> m (WebApiResponse Messages)
history client cid =
  buildGetApi client (buildUrl client "history") . ("channel" =: cid <>) . buildRequestParams

type InfoParams = OptionalParams
  '[ "include_locale"      >: Bool
   , "include_num_members" >: Bool
   ]

info
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> InfoParams
  -> m (WebApiResponse (Record '[ "channel" >: Slack.Conversation ]))
info client cid =
  buildGetApi client (buildUrl client "info") . ("channel" =: cid <>) . buildRequestParams

invite
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> [Slack.UserID]
  -> m (WebApiResponse (Record '[ "channel" >: Slack.Conversation ]))
invite client cid uids = buildPostApi client (buildUrl client "invite") opts
  where
    opts = "channel" =: cid <> "users" =: toQueryParam' uids

join
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (WebApiResponse (Record '[ "channel" >: Slack.Conversation ]))
join client cid =
  buildPostApi client (buildUrl client "join") ("channel" =: cid)

kick
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Slack.UserID
  -> m (WebApiResponse (Record '[ "ok" >: Bool ]))
kick client cid uid =
  buildPostApi client (buildUrl client "kick") ("channel" =: cid <> "user" =: uid)

leave
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (WebApiResponse (Record '[ "not_in_channel" >: Maybe Bool ]))
leave client cid =
  buildPostApi client (buildUrl client "leave") ("channel" =: cid)

type Conversations = Record
  '[ "channels"          >: [Slack.Conversation]
   , "response_metadata" >: Maybe (Record NextCursor)
   ]

type ListParams = OptionalParams
  '[ "cursor"           >: Text
   , "limit"            >: Int
   , "exclude_archived" >: Bool
   , "types"            >: [Slack.ChannelType]
   ]

list
  :: (MonadHttp m, Client c)
  => c -> ListParams -> m (WebApiResponse Conversations)
list client = buildGetApi client (buildUrl client "list") . buildRequestParams

type Members = Record
  '[ "members"           >: [Slack.UserID]
   , "response_metadata" >: Maybe (Record NextCursor)
   ]

type MembersParams = OptionalParams
  '[ "cursor" >: Text
   , "limit"  >: Int
   ]

members
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> MembersParams
  -> m (WebApiResponse Members)
members client cid =
  buildGetApi client (buildUrl client "members") . ("channel" =: cid <>) . buildRequestParams

type OpenResult = Record
  '[ "no_op"        >: Maybe Bool
   , "already_open" >: Maybe Bool
   , "channel"      >: Slack.Conversation
   ]

type OpenParams = OptionalParams
  '[ "channel"   >: Text
   , "return_im" >: Bool
   , "users"     >: [Slack.UserID]
   ]

open
  :: (MonadHttp m, Client c)
  => c
  -> OpenParams
  -> m (WebApiResponse OpenResult)
open client = buildPostApi client (buildUrl client "open") . buildRequestParams

rename
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Text
  -> m (WebApiResponse (Record '[ "channel" >: Slack.Conversation ]))
rename client cid name =
  buildPostApi client (buildUrl client "rename") ("channel" =: cid <> "name" =: name)

type Replies = Record
  '[ "messages"          >: [Slack.Message]
   , "has_more"          >: Bool
   , "response_metadata" >: Maybe (Record NextCursor)
   ]

type RepliesParams = HistoryParams

replies
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Slack.TimeStamp
  -> RepliesParams
  -> m (WebApiResponse Replies)
replies client cid ts =
  buildGetApi client (buildUrl client "replies") .
    ("channel" =: cid <>) . ("ts" =: ts <>) . buildRequestParams

setPurpose
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Text
  -> m (WebApiResponse (Record '[ "purpose" >: Text ]))
setPurpose client cid purpose =
  buildPostApi client (buildUrl client "setPurpose") ("channel" =: cid <> "purpose" =: purpose)

setTopic
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Text
  -> m (WebApiResponse (Record '[ "topic" >: Text ]))
setTopic client cid topic =
  buildPostApi client (buildUrl client "setTopic") ("channel" =: cid <> "topic" =: topic)

unarchive
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (WebApiResponse (Record '[ "ok" >: Bool ]))
unarchive client cid =
  buildPostApi client (buildUrl client "unarchive") ("channel" =: cid)
