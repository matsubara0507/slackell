{-# LANGUAGE MultiParamTypeClasses #-}

module Web.Slack.SlashCommand
    ( SlashCommand
    , RequestData
    ) where

import           Control.Arrow                  ((+++))
import           Data.ByteString.Lazy           (ByteString)
import           Data.Extensible
import           Data.Extensible.FormUrlEncoded ()
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Network.HTTP.Media             as M
import           Servant.API.ContentTypes
import           Web.FormUrlEncoded             (urlDecodeAsForm)

type RequestData = Record
  '[ "token"           >: Text
   , "command"         >: Text
   , "text"            >: Text
   , "response_url"    >: Text
   , "trigger_id"      >: Text
   , "user_id"         >: Text
   , "user_name"       >: Text
   , "team_id"         >: Text
   , "team_domain"     >: Text
   , "channel_id"      >: Text
   , "channel_name"    >: Text
   , "enterprise_id"   >: Maybe Text
   , "enterprise_name" >: Maybe Text
   ]

data SlashCommand

instance Accept SlashCommand where
  contentType _ = "application" M.// "x-www-form-urlencoded"

instance MimeUnrender SlashCommand (ByteString, RequestData) where
  mimeUnrender _ bs = Text.unpack +++ (bs,) $ urlDecodeAsForm bs
