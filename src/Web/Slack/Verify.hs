-- | ref: https://api.slack.com/authentication/verifying-requests-from-slack

module Web.Slack.Verify
    ( SigningSecret
    , RequestTimestamp
    , SignatureHeader
    , encodeSignature
    , convertSignatureHeader
    ) where

import           Crypto.Hash             (Digest, SHA256, digestFromByteString)
import           Crypto.MAC.HMAC         (HMAC (..), hmac)
import           Data.ByteArray.Encoding (Base (..), convertFromBase)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text

newtype SigningSecret = SigningSecret Text deriving (IsString)

type RequestTimestamp = Text

type SignatureHeader = Text

encodeSignature :: SigningSecret -> RequestTimestamp -> ByteString -> Digest SHA256
encodeSignature (SigningSecret secret) ts body =
  hmacGetDigest $ hmac (Text.encodeUtf8 secret) basestr
  where
    basestr = BS.intercalate ":" [Text.encodeUtf8 version, Text.encodeUtf8 ts, body]

convertSignatureHeader :: SignatureHeader -> Maybe (Digest SHA256)
convertSignatureHeader sign = either (const Nothing) digestFromByteString bs
  where
    (_, sign') = Text.breakOnEnd (version <> "=") sign
    bs = convertFromBase Base16 (Text.encodeUtf8 sign') :: Either String ByteString

version :: Text
version = "v0"
