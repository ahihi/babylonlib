module PersonID
    ( LegalGender (..)
    , ParseError ()
    , PersonID ()
    , computeChecksum
    , idBirthDate
    , idIndividualNumber
    , isTemporary
    , legalGender
    , parseID
    , unparseID
    ) where

import Control.Applicative
    ( (<$>)
    , (<*>)
    )
import Control.Monad
    ( guard
    )
import Data.Array.Unboxed
    ( UArray ()
    , (!)
    , listArray
    )
import Data.Char
    ( digitToInt
    )
import Data.List
    ( foldl'
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromJust
    )
import Data.Time.Calendar
    ( Day
    , fromGregorianValid
    , toGregorian
    )
import Text.ParserCombinators.Parsec
    ( Parser ()
    , ParseError ()
    , char
    , choice
    , count
    , digit
    , eof
    , oneOf
    , parse
    )
import Text.Printf
    ( printf
    )

data PersonID = PersonID
    { idBirthDate :: Day
    , idIndividualNumber :: Int
    } deriving (Eq, Show)

isTemporary :: PersonID -> Bool
isTemporary = (>= 900) . idIndividualNumber

parseID :: String -> Either ParseError PersonID
parseID = parse (personID >>= \ pid -> eof >> return pid) ""

unparseID :: PersonID -> String
unparseID pid@(PersonID date num)
    = printf "%02d%02d%02d%c%03d%c" day month year2 centurySymbol num csum
    where
        (year, month, day) = toGregorian date
        (century, year2) = year `divMod` 100
        centurySymbol = fromJust $ rlookup (100*century) centurySymbols
        csum = computeChecksum pid

computeChecksum :: PersonID -> Char
computeChecksum (PersonID date num) = checksumsArray ! index
    where
        index = (dayTerm + monthTerm + yearTerm + num) `mod` 31
        dayTerm = 10^7 * day
        monthTerm = 10^5 * month
        yearTerm = 10^3 * (fromIntegral year `mod` 100)
        (year, month, day) = toGregorian date

data LegalGender = Female | Male
    deriving (Enum, Eq, Show)
    
legalGender :: PersonID -> LegalGender
legalGender pid
    | even $ idIndividualNumber pid = Female
    | otherwise                     = Male

centurySymbols :: (Integral a) => [(Char, a)]
centurySymbols = [('+', 1800), ('-', 1900), ('A', 2000)]

rlookup :: (Eq b) => b -> [(a, b)] -> Maybe a
rlookup _ [] = Nothing
rlookup z ((a, b):xs)
    | b == z    = Just a
    | otherwise = rlookup z xs

checksums :: [Char]
checksums = "0123456789ABCDEFHJKLMNPRSTUVWXY"

checksumsArray :: UArray Int Char
checksumsArray = listArray (0, 30) checksums

intOfLength :: (Integral a) => Int -> Parser a
intOfLength n = foldl' (\ s x -> 10*s + fromIntegral (digitToInt x)) 0 <$> count n digit

dateOfBirth :: Parser Day
dateOfBirth = do
    day <- intOfLength 2
    month <- intOfLength 2
    year <- (+) <$> intOfLength 2 <*> century
    case fromGregorianValid year month day of
        Just date -> return date
        Nothing   -> fail "invalid date"

century :: (Integral a) => Parser a
century = choice [ char c >> return result | (c, result) <- centurySymbols ]

individualNumber :: Parser Int
individualNumber = check =<< intOfLength 3
    where
        check n
            | 2 <= n && n <= 999 = return n
            | otherwise          = fail "invalid individual number"

checksum :: Parser Char
checksum = oneOf checksums

personID :: Parser PersonID
personID = do
    date <- dateOfBirth
    num <- individualNumber
    csum <- checksum
    let pid = PersonID date num
    let csum' = computeChecksum pid
    if csum == csum'
        then return pid
        else fail $ "invalid checksum, expected " ++ show csum'
