module Domain.ParserSpec (spec) where

import Data.Either (isLeft)
import Domain.Parser (parseJwtHeader)
import Domain.Token (Jwt (RawToken))
import Test.Hspec

spec :: Spec
spec = describe "Domain.Parser.parseJwtHeader" $ do
    it "successfully parses a valid 3-part Bearer token" $ do
        let input = "Bearer header123.payload456.sig789"
        case parseJwtHeader input of
            Right (RawToken txt) -> txt `shouldBe` "header123.payload456.sig789"
            Left _ -> expectationFailure "Expected successful parse"

    it "fails if the 'Bearer ' prefix is missing" $ do
        let input = "header123.payload456.sig789"
        parseJwtHeader input `shouldSatisfy` isLeft

    it "fails if there are not exactly 3 parts (e.g., missing signature)" $ do
        let input = "Bearer header123.payload456"
        parseJwtHeader input `shouldSatisfy` isLeft

    it "fails if segments contain invalid Base64Url characters (like spaces)" $ do
        let input = "Bearer head er.payload.sig"
        parseJwtHeader input `shouldSatisfy` isLeft
