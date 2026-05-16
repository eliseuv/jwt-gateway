module Domain.VerifySpec (spec) where

import Data.ByteString.Char8 qualified as B8
import Data.Text qualified as T

import Domain.Token
import Domain.Verify
import Test.Hspec

-- Valid Base64Url components for testing
-- Header:  {"alg":"RS256"}
validHeader :: String
validHeader = "eyJhbGciOiJSUzI1NiJ9"

-- Payload: {"sub":"user_123","exp":1700000000,"iss":"gateway"}
validPayload :: String
validPayload = "eyJzdWIiOiJ1c2VyXzEyMyIsImV4cCI6MTcwMDAwMDAwMCwiaXNzIjoiZ2F0ZXdheSJ9"

validSignature :: String
validSignature = "c2lnbmF0dXJl"

validTokenString :: String
validTokenString = validHeader <> "." <> validPayload <> "." <> validSignature

spec :: Spec
spec = describe "Domain.Verify" $ do
    describe "extractAndDecode" $ do
        it "decodes valid Base64Url token parts successfully" $ do
            let unverified = RawToken (T.pack validTokenString)
            case extractAndDecode unverified of
                Right (_header, _payload, _signature, signed) -> do
                    -- Verify the bytes were extracted
                    signed `shouldBe` B8.pack (validHeader <> "." <> validPayload)
                Left err -> expectationFailure $ "Failed to decode: " <> show err

        it "returns MalformedTokenStructure for tokens not split by two dots" $ do
            let badToken = RawToken "eyJhbGciOiJSUzI1NiJ9"
            extractAndDecode badToken `shouldBe` Left MalformedTokenStructure

    describe "extractClaims" $ do
        it "parses valid JSON bytes into the Claims record" $ do
            let unverified = RawToken (T.pack validTokenString)

            -- Extract the decoded payload bytes
            case extractAndDecode unverified of
                Right (_, payloadBytes, _, _) ->
                    case extractClaims payloadBytes of
                        Right claims -> do
                            _claimsSub claims `shouldBe` "user_123"
                            _claimsExp claims `shouldBe` 1700000000
                            _claimsIss claims `shouldBe` "gateway"
                        Left err -> expectationFailure $ "Failed to parse JSON: " <> show err
                Left err -> expectationFailure $ "Failed to decode token parts: " <> show err

        it "fails with JsonDecodeError if required fields are missing" $ do
            -- Payload: {"sub":"user_123"}  -- Missing 'exp' and 'iss'
            let missingFieldsPayload = "eyJzdWIiOiJ1c2VyXzEyMyJ9"
            let badToken = RawToken (T.pack $ validHeader <> "." <> missingFieldsPayload <> "." <> validSignature)

            case extractAndDecode badToken of
                Right (_, badPayloadBytes, _, _) ->
                    case extractClaims badPayloadBytes of
                        Left (JsonDecodeError _) -> return () -- Success! This is what we expected.
                        Left err -> expectationFailure $ "Failed with wrong error: " <> show err
                        Right _ -> expectationFailure "Expected JSON decoding to fail, but it succeeded"
                Left err -> expectationFailure $ "Failed to decode token parts: " <> show err
