{-# LANGUAGE PatternSynonyms #-}

module Domain.Token (
    TokenState (..),
    Jwt (RawToken),
    pattern ValidToken,
    Claims (..),
    claimsSub,
    claimsExp,
    claimsIss,
    VerifyError (..),
    extractAndDecode,
    extractClaims,
    verifyToken,
) where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), defaultOptions, eitherDecodeStrict, fieldLabelModifier, genericParseJSON)
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Either.Extra (mapLeft)
import Data.Text (Text, splitOn)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)

-- | TokenState Kind with two new types: Unverified and Verified
data TokenState = Unverified | Verified

-- | Payload of the JWT
data Claims = Claims
    { _claimsSub :: Text -- Subject (User ID)
    , _claimsExp :: Integer -- Expiration (Timestamp)
    , _claimsIss :: Text -- Issuer
    }
    deriving (Show, Eq, Generic)

-- | Generate lenses: claimsSub, claimsExp and claimsIss
makeLenses ''Claims

-- | JSON decoding
instance FromJSON Claims where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = map toLower . drop 7
                }

{- | GADT: Type-level State Machine
s : state index
-}
data Jwt (s :: TokenState) where
    -- Unverified Constructor (Header, Payload, Signature)
    RawToken :: !Text -> !Text -> !Text -> Jwt 'Unverified
    -- Internal Verified Constructor
    ValidTokenInternal :: Claims -> Jwt 'Verified

deriving instance Show (Jwt s)

{- | Unidirectional pattern synonym to allow pattern matching on verified tokens
without allowing direct construction.
-}
pattern ValidToken :: Claims -> Jwt 'Verified
pattern ValidToken c <- ValidTokenInternal c

data VerifyError
    = Base64DecodeError String
    | JsonDecodeError String
    | InvalidSignature
    | TokenExpired
    deriving (Show, Eq)

decode64Url :: Text -> Either String ByteString
decode64Url txt =
    convertFromBase Base64URLUnpadded (encodeUtf8 txt)

extractAndDecode :: Jwt 'Unverified -> Either VerifyError (ByteString, ByteString, ByteString, ByteString)
extractAndDecode (RawToken headerTxt payloadTxt signatureTxt) = do
    headerBytes <- mapLeft Base64DecodeError $ decode64Url headerTxt
    payloadBytes <- mapLeft Base64DecodeError $ decode64Url payloadTxt
    signatureBytes <- mapLeft Base64DecodeError $ decode64Url signatureTxt

    let signedContent = encodeUtf8 (headerTxt <> "." <> payloadTxt)

    Right (headerBytes, payloadBytes, signatureBytes, signedContent)

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
    Right (ValidTokenInternal claims)
