import qualified Spec.Web.Slack.WebAPI.Conversations
import qualified Spec.Web.Slack.WebAPI.Users
import           Test.Tasty
import           Web.Slack.Test.Client
import           Web.Slack.Test.MockServer           (runMockServer)

main :: IO ()
main = runMockServer $ defaultMain =<< spec

spec :: IO TestTree
spec = testGroup "Web.Slack" <$> sequence
  [ Spec.Web.Slack.WebAPI.Conversations.specWith client
  , Spec.Web.Slack.WebAPI.Users.specWith client
  ]
  where
    client = TestClient
