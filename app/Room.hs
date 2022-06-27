module Room where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Data.List.Split
import Misc (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)
--import Data.Text

data Room
    = Room
        { roomId :: Int
        , roomName :: String
        , roomLocation :: String
        , roomCapacity :: String
        , roomPIC :: String
        , roomDesc :: String
        }
    | UnknownRoom
    deriving (Show, Eq)

data Schedule
    = Schedule
        { roomIdSched :: Int
        , hourID :: Int
        , hour :: String
        , status :: String
        }
    | UnknownSchedule
    deriving (Show, Eq)


-- Read list of room from file
parseRoom :: String -> [Room]
parseRoom rawContent = map parseSingleRoom (lines rawContent)

parseSingleRoom :: String -> Room
parseSingleRoom str = 
    case splitOn "|" str of
    (id : name : loc : cap : pic : desc) -> makeRoom id name loc cap pic desc
    _ -> UnknownRoom

makeRoom :: String -> String -> String -> String -> String -> [String] -> Room
makeRoom roomId roomName roomLocation roomCapacity roomPIC roomDesc =
    Room
        { roomId = read roomId
        , roomName = roomName
        , roomLocation = roomLocation
        , roomCapacity = roomCapacity
        , roomPIC = roomPIC
        , roomDesc = unwords roomDesc
        }


-- Read list of schedule from file
parseSchedule :: String -> [Schedule]
parseSchedule rawContent = map parseScheduleRoom (lines rawContent)

parseScheduleRoom :: String -> Schedule
parseScheduleRoom str = 
    case splitOn "|" str of
    (roomIdSched : hourId : hourDesc : status) -> makeScheduleRoom roomIdSched hourId hourDesc status
    _ -> UnknownSchedule  

makeScheduleRoom :: String -> String -> String -> [String] -> Schedule
makeScheduleRoom roomId hourId hourDesc status =
    Schedule
        { roomIdSched = read roomId
        , hourID = read hourId
        , hour = hourDesc
        , status = unwords status
        }
    
-- Add new room to the existing list of room
addNewRoom :: [Room] -> String -> String -> String -> String -> String -> IO [Room]
addNewRoom exstRoom name location capacity pic description = do
    let lastId =
            if null exstRoom
                then 0
                else roomId $ last exstRoom
        newId = lastId + 1
        newRoom =
            Room
                { roomId = newId
                , roomName = name
                , roomLocation = location
                , roomCapacity = capacity
                , roomPIC = pic
                , roomDesc = description
                }
    let newRoomList = exstRoom ++ [newRoom]
    return newRoomList

{-
bookRoom :: [Schedule] -> Int -> Int -> IO [Schedule]
bookRoom schedule roomNo hourSlot = 
    case (((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot)) of
        (roomIdSched : hourId : hourDesc : status) -> makeScheduleRoom roomIdSched hourId hourDesc status
        _ -> UnknownSchedule  
    -- (((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot)) = makeScheduleRoom roomIdSched schedule hourID schedule : hour schedule : ["Not Available"]
    -- otherwise = bookRoom rest roomNo hourSlot
-}      


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
    {-
    if (extractItem itemExist) == UnknownItem
        then putStrLn "Item not found. Please check your ItemID"
        else
            if amount == 0
                then putStrLn "Amount inserted is zero. Are you sure you've input it correctly?"
                else putStrLn "Successfully restocked item!"
    -}
    return newSchedule


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
                ++ "\n"
                ++ convertToLog rest
    let parsedLogSchedule = init $ convertToLog newSchedule -- using init to remove the last \n at the end of the .log
    writeFile "model/reservation.txt" parsedLogSchedule
{-
takeItem :: [LogItem] -> Int -> Int -> IO [LogItem]
takeItem oldLogItemList choice amount = do
    let itemExist = find (\item -> (itemId item) == choice) oldLogItemList

        extractItem :: Maybe LogItem -> LogItem
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [LogItem] -> LogItem -> Int -> [LogItem]
        replaceItem [] chosenItem amount = []
        replaceItem (item : rest) chosenItem amount
            | item == chosenItem = [item{storage = storage item - amount}] ++ replaceItem rest chosenItem amount
            | otherwise = [item] ++ replaceItem rest chosenItem amount

    let updatedLogItemList =
            if (extractItem itemExist) == UnknownItem
                then oldLogItemList
                else
                    if amount > (storage $ extractItem itemExist)
                        then oldLogItemList
                        else replaceItem oldLogItemList (extractItem itemExist) amount

    if (extractItem itemExist) == UnknownItem
        then putStrLn "Item not found. Please check your ItemID"
        else
            if amount > (storage $ extractItem itemExist)
                then putStrLn "Not enough storage quantity to take items."
                else
                    if amount == 0
                        then putStrLn "Amount inserted is zero. Are you've sure you input it correctly?"
                        else putStrLn "Successfully restocked item!"

    return updatedLogItemList

parseLogItem :: [LogItem] -> IO ()
parseLogItem logItemList = do
    let convertToLog :: [LogItem] -> String
        convertToLog [] = ""
        convertToLog (logItem : rest) =
            show (itemId logItem)
                ++ " "
                ++ itemName logItem
                ++ " "
                ++ show (storage logItem)
                ++ " "
                ++ description logItem
                ++ "\n"
                ++ convertToLog rest
    let parsedLogItem = init $ convertToLog logItemList -- using init to remove the last \n at the end of the .log
    writeFile "log/items.log" parsedLogItem

parseItem :: String -> [LogItem]
parseItem rawContent = map parseSingleItem (lines rawContent)

parseSingleItem :: String -> LogItem
parseSingleItem str = case words str of
    (i : n : s : d) -> makeItem i n s d
    _ -> UnknownItem

makeItem :: String -> String -> String -> [String] -> LogItem
makeItem itemId itemName storage description =
    LogItem
        { itemId = read itemId
        , itemName = itemName
        , storage = read storage
        , description = unwords description
        }
    -}