module Room where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Data.List.Split
import Misc (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)
import Schedule (Schedule (UnknownSchedule), makeTempRoomKu, resetRoomKu, resetRoomKu, bookRoomKu, cancelRoomKu, parseSchedule, parseLogSchedule, makeScheduleRoom, roomIdSched, hourID, hour, status, bookedBy, agenda)
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

-- Add new room to the existing list of room
addNewRoom :: [Room] -> String -> String -> String -> String -> String -> [Schedule] -> [Schedule] -> IO [Room]
addNewRoom exstRoom name location capacity pic description schedule tempSchedule = do
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
    newSchedule <- makeTempRoomKu schedule tempSchedule newId
    --parseLogSchedule newSchedule
    return newRoomList 

deleteRoom :: [Room] -> Int -> [Room]
deleteRoom [] _ = []
deleteRoom (room : rest) roomIdChoosen
        | (roomId room) /= roomIdChoosen = [room] ++ deleteRoom rest roomIdChoosen
        | otherwise = deleteRoom rest roomIdChoosen

parseLogRoom :: [Room] -> IO ()
parseLogRoom newRoom = do
    let convertToLog :: [Room] -> String
        convertToLog [] = ""
        convertToLog (room : rest) =
            show (roomId room)
                ++ "|"
                ++ roomName room
                ++ "|"
                ++ roomLocation room
                ++ "|"
                ++ roomCapacity room
                ++ "|"
                ++ roomPIC room
                ++ "|"
                ++ roomDesc room
                ++ "\n"
                ++ convertToLog rest
    let parsedLogRoom = init $ convertToLog newRoom -- using init to remove the last \n at the end of the .log
    writeFile "model/room.txt" parsedLogRoom

