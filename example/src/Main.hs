module Main where

import           Control.Concurrent       (forkIO)
import           Control.Monad.Except     (throwError)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Extensible
import           Data.String              (fromString)
import           Data.Text                (Text)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.API
import           Servant.Server           (Server, err401, serve)
import           System.Environment       (getEnv)
import qualified Web.Slack                as Slack
import           Web.Slack.SlashCommand   (SlashCommand)
import qualified Web.Slack.SlashCommand   as SlashCmd

main :: IO ()
main = do
  secret <- fromString <$> getEnv "SLACK_SIGNING_SECRET"
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve (Proxy @ API) (server secret)

type API
    = "slash"
      :> ReqBody '[SlashCommand] (LBS.ByteString, SlashCmd.RequestData)
      :> Header "X-Slack-Request-Timestamp" Slack.RequestTimestamp
      :> Header "X-Slack-Signature" Slack.SignatureHeader
      :> Post '[JSON] NoContent

server :: Slack.SigningSecret -> Server API
server secret = slashCommand
  where
    slashCommand (lbs, body) (Just ts) (Just sign) =
      let digest = Slack.encodeSignature secret ts (LBS.toStrict lbs) in
      if Just digest == Slack.convertSignatureHeader sign then
        liftIO $ do
          _ <- forkIO $ action body
          pure NoContent
      else
        throwError err401
    slashCommand _ _ _ = throwError err401

    action :: SlashCmd.RequestData -> IO ()
    action body = putStrLn $ "post slash with " <> show (shrink body :: LogData)

type LogData = Record
  '[ "text"            >: Text
   , "user_name"       >: Text
   , "team_domain"       >: Text
   , "channel_name"    >: Text
   , "enterprise_name" >: Maybe Text
   ]
