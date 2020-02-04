module Web.Slack.WebAPI.Users
    ( ConversationsParams
    , conversations
    , deletePhoto
    , getPresence
    , IdentityResult
    , identity
    , InfoParams
    , info
    , Users
    , ListParams
    , list
    , lookupByEmail
    , setActive
    , setPresenceAway
    , setPresenceAuto
    -- , setPhoto -- ToDo
    ) where

import           Data.Extensible
import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import           Network.HTTP.Req
import           Network.Simple                 (Client (..), OptionalParams,
                                                 buildRequestParams)
import           Web.Slack.Type                 as Slack
import           Web.Slack.WebAPI.Conversations (Conversations)
import           Web.Slack.WebAPI.Internal

buildUrl :: Client c => c -> Text -> Url (ClientScheme c)
buildUrl c path = baseUrl c /: "api" /: "users." <> path


-- | API for users

type ConversationsParams = OptionalParams
  '[ "cursor"           >: Text
   , "limit"            >: Int
   , "exclude_archived" >: Bool
   , "types"            >: [Slack.ChannelType]
   , "user"             >: Slack.UserID
   ]

conversations
  :: (MonadHttp m, Client c)
  => c -> ConversationsParams -> m (SlackApiResponse Conversations)
conversations client =
  buildGetApi client (buildUrl client "conversations") . buildRequestParams

deletePhoto :: (MonadHttp m, Client c) => c -> m (SlackApiResponse (Record '[ "ok" >: Bool ]))
deletePhoto client = buildPostApi client (buildUrl client "deletePhoto") mempty

getPresence
  :: (MonadHttp m, Client c)
  => c -> Slack.UserID -> m (SlackApiResponse (Record '[ "presence" >: Presence ]))
getPresence client uid =
  buildGetApi client (buildUrl client "getPresence") ("user" =: uid)

type IdentityResult = Record
  '[ "user" >: Slack.UserIdentity
   , "team" >: Slack.TeamIdentity
   ]

identity :: (MonadHttp m, Client c) => c -> m (SlackApiResponse IdentityResult)
identity client = buildGetApi client (buildUrl client "identity") mempty

type InfoParams = OptionalParams
  '[ "include_locale" >: Bool
   ]

info
  :: (MonadHttp m, Client c)
  => c -> Slack.UserID -> InfoParams -> m (SlackApiResponse (Record '[ "user" >: Slack.User ]))
info client uid =
  buildGetApi client (buildUrl client "info") . ("user" =: uid <>) . buildRequestParams

type Users = Record
  '[ "members"           >: [Slack.User]
   , "cache_ts"          >: Int64
   , "response_metadata" >: Maybe (Record NextCursor)
   ]

type ListParams = OptionalParams
  '[ "cursor"         >: Text
   , "include_locale" >: Bool
   , "limit"          >: Int
   ]

list
  :: (MonadHttp m, Client c)
  => c -> ListParams -> m (SlackApiResponse Users)
list client =
  buildGetApi client (buildUrl client "list") . buildRequestParams

lookupByEmail
  :: (MonadHttp m, Client c)
  => c -> Text -> m (SlackApiResponse (Record '[ "user" >: Slack.User ]))
lookupByEmail client email =
  buildGetApi client (buildUrl client "lookupByEmail") ("email" =: email)

setActive, setPresenceAway, setPresenceAuto
  :: (MonadHttp m, Client c)
  => c -> m (SlackApiResponse (Record '[ "ok" >: Bool ]))
setActive client =
  buildPostApi client (buildUrl client "setActive") mempty
setPresenceAway client =
  buildPostApi client (buildUrl client "setPresence") ("presence" =: ("away" :: Text))
setPresenceAuto client =
  buildPostApi client (buildUrl client "setPresence") ("presence" =: ("auto" :: Text))
