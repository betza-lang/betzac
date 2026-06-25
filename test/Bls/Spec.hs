import qualified HandlersSpec as HandlersSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
    HandlersSpec.spec
