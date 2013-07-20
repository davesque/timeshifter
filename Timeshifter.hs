import Nagari
import ParseSubrip hiding (index, startTime, endTime, text)
import Control.Exception
import Data.Fixed
import System.Directory
import System.Environment
import System.IO

{-
 - Data Building/Muting Functions
 -}
getTotalSeconds :: SubTime -> Float
getTotalSeconds (SubTime h m s) = fromIntegral h * 3600 + fromIntegral m * 60 + s

shiftSubTime :: Float -> SubTime -> SubTime
shiftSubTime interval entry =
    SubTime newHours newMinutes newSeconds
    where newTotalSeconds = max 0 (getTotalSeconds entry + interval)
          newHours        = floor $ newTotalSeconds / 3600
          newMinutes      = floor $ (newTotalSeconds `mod'` 3600) / 60
          newSeconds      = newTotalSeconds `mod'` 60

shiftSubEntry :: Float -> SubEntry -> SubEntry
shiftSubEntry interval (SubEntry index startTime endTime text) =
    SubEntry index newStartTime newEndTime text
    where newStartTime = shiftSubTime interval startTime
          newEndTime   = shiftSubTime interval endTime

cleanEntries :: Maybe ([SubEntry], String) -> [SubEntry]
cleanEntries Nothing = error "Illegal subrip entry found!"
cleanEntries (Just (es, _)) = es

{-
 - IO/Main
 -}
shiftEntries :: Float -> String -> IO ()
shiftEntries interval filename = do
    contents <- readFile filename

    let maybeEntries = iterateWhile subEntry contents
        entries      = cleanEntries maybeEntries
        newEntries   = map (shiftSubEntry interval) entries

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            mapM_ (hPutStr tempHandle . show) newEntries
            hClose tempHandle
            removeFile filename
            renameFile tempName filename)

    putStrLn "Done!"

printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "timeshifter interval filename"

dispatch :: [String] -> IO ()
dispatch (interval:filename:[]) = shiftEntries (read interval) filename
dispatch _ = printUsage

main :: IO ()
main = do
    args <- getArgs
    dispatch args
