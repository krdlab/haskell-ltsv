{-# LANGUAGE OverloadedStrings #-}
module Data.LTSV
    ( ltsv
    , recordNL
    , record
    , Field, Record, LTSV
    )
    where

import Control.Applicative ((<*), (<|>))
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString, pack)
import Data.Word (Word8)

type Field  = (ByteString, ByteString)
type Record = [Field]
type LTSV   = [Record]

tab, cr, lf, colon :: Parser Word8
tab   = word8 9
cr    = word8 13
lf    = word8 10
colon = word8 58

-- |
-- LTSV format parser.
--
-- >>> parseOnly ltsv "aaa:111\tbbb:222"
-- Right [[("aaa","111"),("bbb","222")]]
-- >>> parseOnly ltsv "aaa:111\tbbb:222\nccc:333\tddd:444"
-- Right [[("aaa","111"),("bbb","222")],[("ccc","333"),("ddd","444")]]
-- >>> parseOnly ltsv "aaa:111\tbbb:222\nccc:333\tddd:444\n"
-- Right [[("aaa","111"),("bbb","222")],[("ccc","333"),("ddd","444")]]
--
ltsv :: Parser LTSV
ltsv = do
    rs <- many' recordNL
    r  <- record
    return $ if null r
                 then rs
                 else rs ++ [r]

-- |
-- >>> parseOnly recordNL "aaa:111\tbbb:222"
-- Left "not enough bytes"
-- >>> parseOnly recordNL "aaa:111\tbbb:222\n"
-- Right [("aaa","111"),("bbb","222")]
--
recordNL :: Parser Record
recordNL = record <* nl
    where
        nl = (cr >> lf) <|> lf

-- |
-- >>> parseOnly record "aaa:111\tbbb:222"
-- Right [("aaa","111"),("bbb","222")]
-- >>> parseOnly record "aaa:111\tbbb:222\n"
-- Right [("aaa","111"),("bbb","222")]
record :: Parser Record
record = sepBy field tab
    where
        field = do
            l <- label <* colon
            v <- value
            return (pack l, pack v)
        label = many1 $ satisfy $ inClass "0-9A-Za-z_.-"
        value = many' $ satisfy $ \w -> w `notElem` [9, 10, 13]

