module Web.Slack.Client
    ( Client (..)
    , SlackApiClient
    , newClient
    ) where

import           Data.ByteString       (ByteString)
import           Network.HTTP.Req      (Scheme (..))
import qualified Network.HTTP.Req      as Req
import           Network.Simple.Client (Client (..))

type Token = ByteString

newtype SlackApiClient = SlackApiClient Token

instance Client SlackApiClient where
  type ClientScheme SlackApiClient = 'Https
  baseUrl = const (Req.https "slack.com")
  mkHeader (SlackApiClient token) = Req.oAuth2Bearer token

newClient :: Token -> SlackApiClient
newClient = SlackApiClient
