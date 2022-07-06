module Schedule where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Data.List.Split
import Misc (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)


data Schedule
    = Schedule
        { roomIdSched :: Int
        , hourID :: Int
        , hour :: String
        , status :: String
        , bookedBy :: String
        , agenda :: String
        }
    | UnknownSchedule
    deriving (Show, Eq)


-- Read list of schedule from file
parseSchedule :: String -> [Schedule]
parseSchedule rawContent = map parseScheduleRoom (lines rawContent)

parseScheduleRoom :: String -> Schedule
parseScheduleRoom str = 
    case splitOn "|" str of
    (roomIdSched : hourId : hourDesc : status : bookedBy : agenda) -> makeScheduleRoom roomIdSched hourId hourDesc status bookedBy agenda
    _ -> UnknownSchedule  

makeScheduleRoom :: String -> String -> String -> String -> String -> [String] -> Schedule
makeScheduleRoom roomId hourId hourDesc status bookedBy agenda =
    Schedule
        { roomIdSched = read roomId
        , hourID = read hourId
        , hour = hourDesc
        , status = status
        , bookedBy = bookedBy
        , agenda = unwords agenda
        }

-- Write updated schedule to file
parseLogSchedule :: [Schedule] -> IO ()
parseLogSchedule newSchedule = do
    let convertToLog :: [Schedule] -> String
        convertToLog [] = ""
        convertToLog (schedule : rest) =
            show (roomIdSched schedule)
                ++ "|"
                ++ show (hourID schedule)
                ++ "|"
                ++ hour schedule
                ++ "|"
                ++ status schedule
                ++ "|"
                ++ bookedBy schedule
                ++ "|"
                ++ agenda schedule
                ++ "\n"
                ++ convertToLog rest
    let parsedLogSchedule = init $ convertToLog newSchedule -- using init to remove the last \n at the end of the .log
    writeFile "model/reservation.txt" parsedLogSchedule

{-
bookRoom :: [Schedule] -> Int -> Int -> IO [Schedule]
bookRoom oldSchedule roomNo hourSlot = do
    let scheduleExist = find (\schedule -> (roomIdSched schedule) == roomNo && (hourID schedule) == hourSlot) oldSchedule
        extractSchedule :: Maybe Schedule -> Schedule
        extractSchedule (Just a) = a
        extractSchedule Nothing = UnknownSchedule
            
        
        bookSchedule :: [Schedule] -> Schedule -> Int -> Int -> [Schedule]
        bookSchedule [] choosenSched roomNo hourSlot = []
        bookSchedule (schedule : rest) choosenSched roomNo hourSlot
            | schedule == choosenSched = [schedule{status = "Not Available"}] ++ bookSchedule rest choosenSched roomNo hourSlot
        --  | ((schedule == roomNo) && (hourID schedule == hourSlot)) = [schedule{status = "Not Available"}] ++ bookSchedule rest roomNo hourSlot
            | otherwise = [schedule] ++ bookSchedule rest choosenSched roomNo hourSlot

    let newSchedule =
            if (extractSchedule scheduleExist) == UnknownSchedule
                then oldSchedule
                else bookSchedule oldSchedule (extractSchedule scheduleExist) roomNo hourSlot
    
    if (extractItem itemExist) == UnknownItem
        then putStrLn "Item not found. Please check your ItemID"
        else
            if amount == 0
                then putStrLn "Amount inserted is zero. Are you sure you've input it correctly?"
                else putStrLn "Successfully restocked item!"
    
    return newSchedule
-}

bookRoomKu :: [Schedule] -> Int -> Int -> String -> String -> [Schedule]
bookRoomKu [] _ _ _ _ = []
bookRoomKu (schedule : rest) roomNo hourSlot bookedBy agenda
        -- ((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot) = [makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) [("Not Available")]] ++ bookRoomKu rest roomNo hourSlot
        -- (roomIdSched schedule) == roomNo = [makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) [("Not Available")]] ++ bookRoomKu rest roomNo hourSlot
        -- (roomIdSched schedule) == roomNo = [(makeScheduleRoom (show (roomIdSched schedule)) (show (hourID schedule)) (hour schedule) ["Not Available"])] ++ bookRoomKu rest roomNo hourSlot
        --(hourID schedule) == slot = [(makeScheduleRoom (show (roomIdSched schedule)) (show (hourID schedule)) (hour schedule) ["Not Available"])] ++ bookRoomKu rest roomnumber slot
        | ((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot) = [(makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) "Not Available" (bookedBy) [(agenda)])] ++ bookRoomKu rest roomNo hourSlot bookedBy agenda
        | otherwise = [schedule] ++ bookRoomKu rest roomNo hourSlot bookedBy agenda

cancelRoomKu :: [Schedule] -> Int -> Int -> [Schedule]
cancelRoomKu [] _ _ = []
cancelRoomKu (schedule : rest) roomNo hourSlot
        | ((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot) = [(makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) "Available" "NA" ["NA"])] ++ cancelRoomKu rest roomNo hourSlot
        | otherwise = [schedule] ++ cancelRoomKu rest roomNo hourSlot

addNewSchedule :: [Schedule] -> Int -> Int ->String -> String -> String -> String -> IO [Schedule]
addNewSchedule exstSchedule roomId hourId hour status bookedBy agenda = do
    let lastId =
            if null exstSchedule
                then 0
                else hourID $ last exstSchedule
        newId = lastId + 1
        newSchedule =
            Schedule 
                { roomIdSched = roomId
                , hourID = newId
                , hour = hour
                , status = status
                , bookedBy = bookedBy
                , agenda = agenda
                }
    let newScheduleList = exstSchedule ++ [newSchedule]
    return newScheduleList 

resetRoomKu :: [Schedule] -> [Schedule]
resetRoomKu [] = []
resetRoomKu (schedule : rest) = [(makeScheduleRoom (show (roomIdSched schedule)) (show (hourID schedule)) (hour schedule) "Available" "NA" ["NA"])] ++ resetRoomKu rest

deleteSchedule :: [Schedule] -> Int -> [Schedule]
deleteSchedule [] _ = []
deleteSchedule (schedule : rest) roomIdChoosen
        | (roomIdSched schedule) /= roomIdChoosen = [schedule] ++ deleteSchedule rest roomIdChoosen
        | otherwise = deleteSchedule rest roomIdChoosen
        

convertSchedule :: [Schedule] -> Int -> [Schedule]      
convertSchedule [] _ = []
convertSchedule (tempSchedule : rest) roomNum = [(makeScheduleRoom (show (roomNum)) (show (hourID tempSchedule)) (hour tempSchedule) "Available" "NA" ["NA"])] ++ convertSchedule rest roomNum
        
makeTempRoomKu :: [Schedule] -> [Schedule] -> Int -> IO [Schedule]
makeTempRoomKu schedule tempschedule roomNum = do
    let newSchedule = schedule ++ convertSchedule tempschedule roomNum
    return newSchedule
