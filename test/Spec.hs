import Domain.ParserSpec qualified
import Domain.TokenSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
    Domain.ParserSpec.spec
    Domain.TokenSpec.spec
