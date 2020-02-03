module Web.Slack.WebAPI.Internal where

import           Data.Aeson            (FromJSON)
import           Data.Extensible
import           Data.Text             (Text)
import           Network.HTTP.Req
import           Network.Simple.Client (Client (..), buildApi)
import           Web.Slack.Type        as Slack

type SlackApiResponse r = JsonResponse (Slack.Ok r)

type NextCursor = '[ "next_cursor" >: Text ]

buildGetApi ::
  (MonadHttp m, Client c, FromJSON r)
  => c                             -- ^ client
  -> Url (ClientScheme c)          -- ^ Location of resource
  -> Option (ClientScheme c)       -- ^ request params
  -> m (SlackApiResponse r)
buildGetApi c url params = buildApi c GET url NoReqBody params

buildPostApi ::
  (MonadHttp m, Client c, FromJSON r)
  => c                             -- ^ client
  -> Url (ClientScheme c)          -- ^ Location of resource
  -> Option (ClientScheme c)       -- ^ request params
  -> m (SlackApiResponse r)
buildPostApi c url params = buildApi c POST url NoReqBody params
