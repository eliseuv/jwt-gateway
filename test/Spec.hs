import Domain.ParserSpec qualified
import Domain.VerifySpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
    Domain.ParserSpec.spec
    Domain.VerifySpec.spec
