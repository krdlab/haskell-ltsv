{-# LANGUAGE OverloadedStrings #-}
module Data.LTSV.String
    ( ltsv
    , record
    , encode
    , decode
    , decodeLTSV
    , ToRecord(..)
    , FromRecord(..)
    , Field, FieldMap, Record, LTSV
    )
    where

import Control.Monad       (liftM2)
import Control.Applicative ((<*))
import Text.Parsec         ( parse
                           , Parsec
                           , newline, sepBy, tab, char, many1, alphaNum, oneOf, many, noneOf, (<|>), try
                           )
import Data.List (intercalate)
import qualified Data.Map as Map

type Field    = (String, String)
type FieldMap = Map.Map String String
type Record   = [Field]
type LTSV     = [Record]

-- |
-- LTSV format parser.
--
-- >>> import Text.Parsec (parse)
-- >>> parse ltsv "(stdin)" "aaa:111\tbbb:222"
-- Right [[("aaa","111"),("bbb","222")]]
-- >>> parse ltsv "(stdin)" "aaa:111\tbbb:222\nccc:333\tddd:444"
-- Right [[("aaa","111"),("bbb","222")],[("ccc","333"),("ddd","444")]]
-- >>> parse ltsv "(stdin)" "aaa:111\tbbb:222\nccc:333\tddd:444\n"
-- Right [[("aaa","111"),("bbb","222")],[("ccc","333"),("ddd","444")]]
--
ltsv :: Parsec String () LTSV
ltsv = do
    rs <- many $ try recordNL
    r  <- record
    return $ if null r
                 then rs
                 else rs ++ [r]

recordNL :: Parsec String () Record
recordNL = record <* newline

record :: Parsec String () Record
record = sepBy field tab
    where
        field = liftM2 (,) (label <* colon) value
        label = many1 lbyte
        lbyte = alphaNum <|> oneOf "_.-"
        value = many $ noneOf "\t\n\r"
        colon = char ':'

class ToRecord a where
    toRecord :: a -> Record

class FromRecord a where
    fromRecord :: FieldMap -> Maybe a

-- |
-- Serialize a record value as a String.
--
-- >>> data Person = Person { name :: String, age :: Int } deriving Show
-- >>> instance ToRecord Person where toRecord p = [("name", name p), ("age", show . age $ p)]
-- >>> encode $ Person "krdlab" 128
-- "name:krdlab\tage:128"
--
encode :: (ToRecord a) => a -> String
encode = serialize . toRecord

serialize :: Record -> String
serialize [] = ""
serialize ts = intercalate "\t" $ map s ts
    where
        s (l, v) = l ++ ":" ++ v

-- |
-- deserialize a record value from a String.
--
-- >>> import Control.Applicative ((<$>))
-- >>> data Person = Person { name :: String, age :: Int } deriving Show
-- >>> instance FromRecord Person where fromRecord m = liftM2 Person (Map.lookup "name" m) (read <$> Map.lookup "age" m)
-- >>> decode "name:krdlab\tage:128" :: Maybe Person
-- Just (Person {name = "krdlab", age = 128})
--
decode :: (FromRecord a) => String -> Maybe a
decode = decodeWith record fromRecord

decodeWith :: Parsec String () Record -> (FieldMap -> Maybe a) -> String -> Maybe a
decodeWith p conv s = 
    case parse p "(decode)" s of
        Right r -> conv $ Map.fromList r
        _       -> Nothing

decodeLTSV :: (FromRecord a) => LTSV -> Maybe [a]
decodeLTSV = mapM (fromRecord . Map.fromList)

