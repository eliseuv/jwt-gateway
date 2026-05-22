module Domain.Parser (
    parseJwtHeader,
    JwtParseError (..),
)
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Domain.Token (Jwt (..), TokenState (..))

-- | Megaparsec parser
type Parser = Parsec Void Text

-- | Custom error type
newtype JwtParseError = InvalidFormat Text
    deriving (Show, Eq)

-- | Helper to parse Base64
isBase64UrlChar :: Char -> Bool
isBase64UrlChar c =
    isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c == '-'
        || c == '_'

{- Core JWT parser
Validates: "Bearer <header>.<payload>.<signature>"
-}
jwtParser :: Parser (Jwt 'Unverified)
jwtParser = do
    _ <- string "Bearer "

    let part = takeWhile1P (Just "base64url component") isBase64UrlChar

    hdr <- part
    _ <- char '.'
    pay <- part
    _ <- char '.'
    sig <- part

    return (RawToken hdr pay sig)


-- Entry point
parseJwtHeader :: Text -> Either (ParseErrorBundle Text Void) (Jwt 'Unverified)
parseJwtHeader = parse jwtParser ""
