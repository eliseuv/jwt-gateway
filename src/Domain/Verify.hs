module Domain.Verify (
    VerifyError (..),
    extractAndDecode,
    extractClaims,
)
where

import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase)
import Data.ByteString (ByteString)
import Data.Either.Extra (mapLeft)
import Data.Text (Text, splitOn)
import Data.Text.Encoding (encodeUtf8)

import Data.Aeson (eitherDecodeStrict)
import Domain.Token (Claims, Jwt (..), TokenState (..))

data VerifyError
    = MalformedTokenStructure
    | Base64DecodeError String
    | JsonDecodeError String
    | InvalidSignature
    | TokenExpired
    deriving (Show, Eq)

decode64Url :: Text -> Either String ByteString
decode64Url txt =
    convertFromBase Base64URLUnpadded (encodeUtf8 txt)

extractAndDecode :: Jwt 'Unverified -> Either VerifyError (ByteString, ByteString, ByteString, ByteString)
extractAndDecode (RawToken rawText) =
    case splitOn "." rawText of
        [headerTxt, payloadTxt, signatureTxt] -> do
            headerBytes <- mapLeft Base64DecodeError $ decode64Url headerTxt
            payloadBytes <- mapLeft Base64DecodeError $ decode64Url payloadTxt
            signatureBytes <- mapLeft Base64DecodeError $ decode64Url signatureTxt

            let signedContent = encodeUtf8 (headerTxt <> "." <> payloadTxt)

            Right (headerBytes, payloadBytes, signatureBytes, signedContent)
        _ -> Left MalformedTokenStructure

-- | Attempt to parse the raw JSON bytes into Claims record
extractClaims :: ByteString -> Either VerifyError Claims
extractClaims payloadBytes =
    mapLeft JsonDecodeError (eitherDecodeStrict payloadBytes)

-- | Token verification
verifyToken :: Jwt 'Unverified -> Either VerifyError (Jwt 'Verified)
verifyToken unverifiedToken = do
    (_, payloadBytes, _, _) <- extractAndDecode unverifiedToken
    claims <- extractClaims payloadBytes
    -- TODO: Verify signature
    -- TODO: Check expiration date
    Right (ValidToken claims)
