module Spec.Web.Slack.WebAPI.Users
    ( specWith
    ) where


import           Data.Extensible
import           Network.Simple         (Client)
import           Test.Tasty
import           Test.Tasty.Hspec
import           Web.Slack.Test.Helper  (shouldResponseAs)
import           Web.Slack.WebAPI.Users as Users

specWith :: Client c => c -> IO TestTree
specWith client = testSpec "Web.Slack.WebAPI.Users" $ do
  describe "conversations" $
    it "should return conversations" $
      Users.conversations client vacancy `shouldResponseAs` "test/fixture/webapi/conversations.json"
  describe "deletePhoto" $
    it "should return ok" $
      Users.deletePhoto client `shouldResponseAs` "test/fixture/webapi/ok.json"
  describe "getPresence" $
    it "should return presence" $
      Users.getPresence client "C1234567890" `shouldResponseAs` "test/fixture/webapi/presence.json"
  describe "identity" $
    it "should return identity" $
      Users.identity client `shouldResponseAs` "test/fixture/webapi/identity.json"
  describe "info" $
    it "should return user" $
      Users.info client "C1234567890" vacancy `shouldResponseAs` "test/fixture/webapi/user.json"
  describe "list" $
    it "should return users" $
      Users.list client vacancy `shouldResponseAs` "test/fixture/webapi/users.json"
  describe "lookupByEmail" $
    it "should return user" $
      Users.lookupByEmail client "spengler@ghostbusters.example.com"  `shouldResponseAs` "test/fixture/webapi/user.json"
  describe "setActive" $
    it "should return ok" $
      Users.setActive client `shouldResponseAs` "test/fixture/webapi/ok.json"
  describe "setPresenceAway" $
    it "should return ok" $
      Users.setPresenceAway client `shouldResponseAs` "test/fixture/webapi/ok.json"
  describe "setPresenceAuto" $
    it "should return ok" $
      Users.setPresenceAuto client `shouldResponseAs` "test/fixture/webapi/ok.json"
