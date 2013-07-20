module ParseSubrip where

import Prelude hiding (return, iterate)
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
twoDigitInt = iterate digit 2 >>> read

-- | Doesn't really need err message since only used in function which has err
-- message.
seconds :: Parser Float
seconds = iterate digit 2 #- comma # iterate digit 3
    >>> (\(whole, fractional) -> read $ whole ++ "." ++ fractional)

subNumber :: Parser SubIndex
subNumber = number #- newline

subTime :: Parser SubTime
subTime = twoDigitInt #- colon # twoDigitInt #- colon # seconds
    >>> (\((h, m), s) -> SubTime h m s)
    ! err "Illegal sub rip time entry"

subRange :: Parser (SubTime, SubTime)
subRange = subTime #- accept " --> " # subTime #- newline
         ! err "Illegal sub rip time range"

subText :: Parser SubText
subText = (iterateUntil "\n\n" >>> (++"\n")) #- double
        ! err "Illegal sub rip text"

subEntry :: Parser SubEntry
subEntry = subNumber # subRange # subText
    >>> (\((seIndex, (seStartTime, seEndTime)), seText) ->
            SubEntry seIndex seStartTime seEndTime seText)
