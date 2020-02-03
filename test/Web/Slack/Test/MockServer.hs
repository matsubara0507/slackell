{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Web.Slack.Test.MockServer
    ( mockServer
    , runMockServer
    ) where

import           Control.Concurrent
import           Network.Wai.Handler.Warp
import           Servant
import qualified Web.Slack.Test.WebAPI    as WebAPI

type API = WebAPI.API

api :: Proxy API
api = Proxy

server :: Server API
server = WebAPI.server

mockServer :: IO ()
mockServer = run 8000 (serve api server)

runMockServer :: IO () -> IO ()
runMockServer action = do
  _ <- forkIO mockServer
  action
