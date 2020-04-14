module Web.Slack.Verify
    ( SigningSecret
    , encodeSignature
    , convertToDigest
    ) where

import           Crypto.Hash             (Digest, SHA256, digestFromByteString)
import           Crypto.MAC.HMAC         (HMAC (..), hmac)
import           Data.ByteArray.Encoding (Base (..), convertFromBase)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as Text

newtype SigningSecret = SigningSecret Text deriving (IsString)

type RequestTimestamp = ByteString

encodeSignature :: SigningSecret -> RequestTimestamp -> ByteString -> Digest SHA256
encodeSignature (SigningSecret secret) ts body =
  hmacGetDigest $ hmac (Text.encodeUtf8 secret) (BS.intercalate ":" ["v0", ts, body])

convertToDigest :: ByteString -> Maybe (Digest SHA256)
convertToDigest bs =
  either (const Nothing) digestFromByteString (convertFromBase Base16 bs :: Either String ByteString)
