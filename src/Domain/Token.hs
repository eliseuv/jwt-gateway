module Domain.Token where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.Char (toLower)
import Data.Text (Text)
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
    -- Unverified Constructor
    RawToken :: Text -> Jwt 'Unverified
    -- Verified Constructor
    ValidToken :: Claims -> Jwt 'Verified

deriving instance Show (Jwt s)
