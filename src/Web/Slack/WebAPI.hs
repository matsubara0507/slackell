module Web.Slack.WebAPI
    ( SlackApiResponse
    , NextCursor
    , run
    ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson                (FromJSON)
import           Data.Text                 (Text)
import           Network.HTTP.Req          (Req, defaultHttpConfig,
                                            responseBody, runReq)
import           Web.Slack.Type            (Ok (..))
import           Web.Slack.WebAPI.Internal (NextCursor, SlackApiResponse)

run :: (MonadIO m, FromJSON r) => Req (SlackApiResponse r) -> m (Either Text r)
run request = runReq defaultHttpConfig $ do
  result <- request
  pure $ case responseBody result of
    Ok r  -> Right r
    Err e -> Left e
