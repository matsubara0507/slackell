module Web.Slack.Test.WebAPI where

import           Data.Extensible
import           Data.Text                      (Text)
import           Servant
import qualified Web.Slack                      as Slack
import           Web.Slack.Test.Helper          (returnJsonFile)
import           Web.Slack.WebAPI.Conversations (CloseResule, Conversations,
                                                 Members, Messages, OpenResult,
                                                 Replies)
import           Web.Slack.WebAPI.Users         (IdentityResult, Users)

type API = "api" :> (ConversationsAPI :<|> UsersAPI)

server :: Server API
server = conversations :<|> users

type ConversationsAPI
     = "conversations.archive"    :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))
  :<|> "conversations.close"      :> Post '[JSON] (Slack.Ok CloseResule)
  :<|> "conversations.create"     :> Post '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
  :<|> "conversations.history"    :> Get '[JSON] (Slack.Ok Messages)
  :<|> "conversations.info"       :> Get '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
  :<|> "conversations.invite"     :> Post '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
  :<|> "conversations.join"       :> Post '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
  :<|> "conversations.kick"       :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))
  :<|> "conversations.leave"      :> Post '[JSON] (Slack.Ok (Record '[ "not_in_channel" >: Maybe Bool ]))
  :<|> "conversations.list"       :> Get '[JSON] (Slack.Ok Conversations)
  :<|> "conversations.members"    :> Get '[JSON] (Slack.Ok Members)
  :<|> "conversations.open"       :> Post '[JSON] (Slack.Ok OpenResult)
  :<|> "conversations.rename"     :> Post '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
  :<|> "conversations.replies"    :> Get '[JSON] (Slack.Ok Replies)
  :<|> "conversations.setPurpose" :> Post '[JSON] (Slack.Ok (Record '[ "purpose" >: Text ]))
  :<|> "conversations.setTopic"   :> Post '[JSON] (Slack.Ok (Record '[ "topic" >: Text ]))
  :<|> "conversations.unarchive"  :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))

conversations :: Server ConversationsAPI
conversations
     = conversationsArchive
  :<|> conversationsClose
  :<|> conversationsCreate
  :<|> conversationsHistory
  :<|> conversationsInfo
  :<|> conversationsInvite
  :<|> conversationsJoin
  :<|> conversationsKick
  :<|> conversationsLeave
  :<|> conversationsList
  :<|> conversationsMembers
  :<|> conversationsOpen
  :<|> conversationsRename
  :<|> conversationsReplies
  :<|> conversationsSetPurpose
  :<|> conversationsSetTopic
  :<|> conversationsUnarchive
  where
    conversationsArchive = returnJsonFile "test/fixture/webapi/ok.json"
    conversationsClose = returnJsonFile "test/fixture/webapi/ok.json"
    conversationsCreate = returnJsonFile "test/fixture/webapi/conversation.json"
    conversationsHistory = returnJsonFile "test/fixture/webapi/messages.json"
    conversationsInfo = returnJsonFile "test/fixture/webapi/conversation.json"
    conversationsInvite = returnJsonFile "test/fixture/webapi/conversation.json"
    conversationsJoin = returnJsonFile "test/fixture/webapi/conversation.json"
    conversationsKick = returnJsonFile "test/fixture/webapi/ok.json"
    conversationsLeave = returnJsonFile "test/fixture/webapi/ok.json"
    conversationsList = returnJsonFile "test/fixture/webapi/conversations.json"
    conversationsMembers = returnJsonFile "test/fixture/webapi/members.json"
    conversationsOpen = returnJsonFile "test/fixture/webapi/conversation.json"
    conversationsRename = returnJsonFile "test/fixture/webapi/conversation.json"
    conversationsReplies = returnJsonFile "test/fixture/webapi/replies.json"
    conversationsSetPurpose = returnJsonFile "test/fixture/webapi/purpose.json"
    conversationsSetTopic = returnJsonFile "test/fixture/webapi/topic.json"
    conversationsUnarchive = returnJsonFile "test/fixture/webapi/ok.json"

type UsersAPI
     = "users.conversations" :> Get '[JSON] (Slack.Ok Conversations)
  :<|> "users.deletePhoto"   :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))
  :<|> "users.getPresence"   :> Get '[JSON] (Slack.Ok (Record '[ "presence" >: Slack.Presence ]))
  :<|> "users.identity"      :> Get '[JSON] (Slack.Ok IdentityResult)
  :<|> "users.info"          :> Get '[JSON] (Slack.Ok (Record '[ "user" >: Slack.User ]))
  :<|> "users.list"          :> Get '[JSON] (Slack.Ok Users)
  :<|> "users.lookupByEmail" :> Get '[JSON] (Slack.Ok (Record '[ "user" >: Slack.User ]))
  :<|> "users.setActive"     :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))
  :<|> "users.setPresence"   :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))

users :: Server UsersAPI
users
     = usersConversations
  :<|> usersDeletePhoto
  :<|> usersGetPresence
  :<|> usersIdentity
  :<|> usersInfo
  :<|> usersList
  :<|> usersLookupByEmail
  :<|> usersSetActive
  :<|> usersSetPresence
  where
    usersConversations = returnJsonFile "test/fixture/webapi/conversations.json"
    usersDeletePhoto = returnJsonFile "test/fixture/webapi/ok.json"
    usersGetPresence = returnJsonFile "test/fixture/webapi/presence.json"
    usersIdentity = returnJsonFile "test/fixture/webapi/identity.json"
    usersInfo = returnJsonFile "test/fixture/webapi/user.json"
    usersList = returnJsonFile "test/fixture/webapi/users.json"
    usersLookupByEmail = returnJsonFile "test/fixture/webapi/user.json"
    usersSetActive = returnJsonFile "test/fixture/webapi/ok.json"
    usersSetPresence = returnJsonFile "test/fixture/webapi/ok.json"
