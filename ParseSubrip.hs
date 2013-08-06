module ParseSubrip where

import Prelude hiding (iterate, map, take, or)
import Data.List.Utils
import Text.Printf

import Nagari

-------------------
-- Grammar types --
-------------------

type SubIndex = Int
data SubTime = SubTime { hour :: Int
                       , minute :: Int
                       , second :: Float
                       }
type SubText = String
data SubEntry = SubEntry { index :: SubIndex
                         , startTime :: SubTime
                         , endTime :: SubTime
                         , text :: SubText
                         }

instance Show SubTime where
    show (SubTime stHour stMinute stSecond) =
        fHour   ++ ":" ++
        fMinute ++ ":" ++
        replace "." "," fSecond
        where fHour   = printf "%02d" stHour
              fMinute = printf "%02d" stMinute
              fSecond = printf "%06.3f" stSecond

instance Show SubEntry where
    show (SubEntry seIndex seStartTime seEndTime seText) =
        fIndex ++ "\n" ++
        fTime  ++ "\n" ++
        seText ++ "\n"
        where fIndex = show seIndex
              fTime  = show seStartTime ++ " --> " ++ show seEndTime

---------------------
-- Grammar parsers --
---------------------

newline :: Parser Char
newline = lit '\n'

colon :: Parser Char
colon = lit ':'

comma :: Parser Char
comma = lit ','

twoDigitInt :: Parser Int
twoDigitInt = map read $ take 2 digit

seconds :: Parser Float
seconds = do
    whole <- take 2 digit
    comma
    fractional <- take 3 digit
    return $ read (whole ++ "." ++ fractional)

subNumber :: Parser SubIndex
subNumber = do
    x <- number
    newline
    return x

subTime :: Parser SubTime
subTime = (do
    h <- twoDigitInt
    colon
    m <- twoDigitInt
    colon
    s <- seconds
    return $ SubTime h m s) `or` err "Illegal sub rip time entry"

subRange :: Parser (SubTime, SubTime)
subRange = (do
    start <- subTime
    accept " --> "
    end <- subTime
    return (start, end)) `or` err "Illegal sub rip time range"

subText :: Parser SubText
subText = (do
    str <- takeUntil "\n\n"
    take 2 $ lit '\n'
    return $ str ++ "\n") `or` err "Illegal sub rip text"

subEntry :: Parser SubEntry
subEntry = do
    seIndex <- subNumber
    (seStartTime, seEndTime) <- subRange
    seText <- subText
    return $ SubEntry seIndex seStartTime seEndTime seText
