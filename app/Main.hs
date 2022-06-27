module Main where

import System.IO (hFlush, stdout)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Data.List.Split
import Misc (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)

import Test (addMe)
import Message (LogMessage)
--import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import Room (Room (UnknownRoom), Schedule (UnknownSchedule), parseLogSchedule, makeScheduleRoom, addNewRoom, bookRoom, roomId, roomName, roomLocation, roomCapacity, roomPIC, roomDesc, parseRoom, parseSchedule, roomIdSched, hourID, hour, status)

runReservation :: [Room] -> [Schedule] -> [LogMessage] -> IO ()
runReservation rooms schedule messages = do
    putStrLn "\n"     
    putStrLn "|++++++++++++++++++++++++++++++++++++++++++++++++++|"
    putStrLn "|++++++++++++++ List of Meeting Room ++++++++++++++|"
    putStrLn "|++++++++++++++++++++++++++++++++++++++++++++++++++|"
    putStrLn "\n" 
    putStrLn $ showAll rooms
    putStrLn "(a) Show all\n(b) Book Room\n(c) Cancel Meeting\n(e) Exit program"
    choice <- prompt "Input choice: "
    --user choice
    case choice of
        "a" -> do
                putStrLn $ showAll rooms
                empty <- prompt "Press enter to go back"
                runReservation rooms schedule messages
        "b" -> do
                putStr "Please choose meeting room ID: "
                hFlush stdout
                roomNo <- do
                        result <- runMaybeT maybeReadInt
                        case result of
                                (Just a) -> return a                                      
                                Nothing -> return 0

                putStrLn "\n++++++++++++++++++++++++++"
                putStrLn "Meeting Room Availability"
                putStrLn "++++++++++++++++++++++++++\n"
                putStrLn $ showRoomSchedule schedule roomNo

                putStr "Please choose 1 slot to book the room. Slot No: "
                hFlush stdout
                hourSlot <- do
                        resultb <- runMaybeT maybeReadInt
                        case resultb of
                                (Just b) -> return b
                                Nothing -> return 0

                --putStrLn $ showRoomSlotSchedule schedule hourSlot                            
                --emptyPrompt <- prompt "Press enter to continue."
                --showScheduleRoom roomNo
                --putStrLn $ showAllSchedule schedule
        
                newSchedule <- return (bookRoomKu schedule roomNo hourSlot)
                parseLogSchedule newSchedule        
                
                --putStrLn $ showRoomSchedule newSchedule roomNo
                emptyPrompt <- prompt "Press enter to continue."
                runReservation rooms newSchedule messages
        "c" -> do
                putStr "Please choose meeting room ID: "
                hFlush stdout
                roomNo <- do
                        result <- runMaybeT maybeReadInt
                        case result of
                                (Just a) -> return a                                      
                                Nothing -> return 0

                putStrLn "\n++++++++++++++++++++++++++"
                putStrLn "Meeting Room Availability"
                putStrLn "++++++++++++++++++++++++++\n"
                putStrLn $ showRoomSchedule schedule roomNo

                putStr "Please choose 1 slot to cancel reservation. Slot No: "
                hFlush stdout
                hourSlot <- do
                        resultb <- runMaybeT maybeReadInt
                        case resultb of
                                (Just b) -> return b
                                Nothing -> return 0

                --putStrLn $ showRoomSlotSchedule schedule hourSlot                            
                --emptyPrompt <- prompt "Press enter to continue."
                --showScheduleRoom roomNo
                --putStrLn $ showAllSchedule schedule
        
                newSchedule <- return (cancelRoomKu schedule roomNo hourSlot)
                parseLogSchedule newSchedule        
                
                --putStrLn $ showRoomSchedule newSchedule roomNo
                emptyPrompt <- prompt "Press enter to continue."
                runReservation rooms newSchedule messages

        "e" -> do
                putStrLn "Exiting program..."
                putStrLn "Goodbye!"

        _ -> do
                empty <- prompt "Wrong input! Press enter to try again."
                runReservation rooms schedule messages


showAll :: [Room] -> String
showAll [] = replicate 52 '='
showAll (room : rest) =
        "ID: " 
        ++ show (roomId room)
        ++ "\nName: "
        ++ roomName room
        ++ "\nLocation: "
        ++ roomLocation room
        ++ "\nCapacity: "
        ++ roomCapacity room
        ++ "\nPIC: "
        ++ roomPIC room
        ++ "\nDescription: "
        ++ roomDesc room
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAll rest

showAllSchedule :: [Schedule] -> String
showAllSchedule [] = replicate 52 '='
showAllSchedule (schedule : rest) =
        "No." ++ show (hourID schedule) ++ "\tHour: " ++ hour schedule ++ "\tStatus: " ++ status schedule
        ++ "\n"
        ++ showAllSchedule rest

showRoomSchedule :: [Schedule] -> Int -> String
showRoomSchedule [] _ = replicate 52 '='
showRoomSchedule (schedule : rest) roomIdChoosen
        | (roomIdSched schedule) == roomIdChoosen = "No." ++ show (hourID schedule) ++ "\tHour: " ++ hour schedule ++ "\tStatus: " ++ status schedule ++ "\n" ++ showRoomSchedule rest roomIdChoosen
        | otherwise = showRoomSchedule rest roomIdChoosen

showRoomSlotSchedule :: [Schedule] -> Int -> String
showRoomSlotSchedule [] _ = replicate 52 '='
showRoomSlotSchedule (schedule : rest) hourIdChoosen
        | (hourID schedule) == hourIdChoosen = "No." ++ show (hourID schedule) ++ "\tHour: " ++ hour schedule ++ "\tStatus: " ++ status schedule ++ "\n" ++ showRoomSlotSchedule rest hourIdChoosen
        | otherwise = showRoomSlotSchedule rest hourIdChoosen

bookRoomKu :: [Schedule] -> Int -> Int -> [Schedule]
bookRoomKu [] _ _ = []
bookRoomKu (schedule : rest) roomNo hourSlot
        -- ((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot) = [makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) [("Not Available")]] ++ bookRoomKu rest roomNo hourSlot
        -- (roomIdSched schedule) == roomNo = [makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) [("Not Available")]] ++ bookRoomKu rest roomNo hourSlot
        -- (roomIdSched schedule) == roomNo = [(makeScheduleRoom (show (roomIdSched schedule)) (show (hourID schedule)) (hour schedule) ["Not Available"])] ++ bookRoomKu rest roomNo hourSlot
        --(hourID schedule) == slot = [(makeScheduleRoom (show (roomIdSched schedule)) (show (hourID schedule)) (hour schedule) ["Not Available"])] ++ bookRoomKu rest roomnumber slot
        | ((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot) = [(makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) ["Not Available"])] ++ bookRoomKu rest roomNo hourSlot
        | otherwise = [schedule] ++ bookRoomKu rest roomNo hourSlot

cancelRoomKu :: [Schedule] -> Int -> Int -> [Schedule]
cancelRoomKu [] _ _ = []
cancelRoomKu (schedule : rest) roomNo hourSlot
        -- ((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot) = [makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) [("Not Available")]] ++ bookRoomKu rest roomNo hourSlot
        -- (roomIdSched schedule) == roomNo = [makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) [("Not Available")]] ++ bookRoomKu rest roomNo hourSlot
        -- (roomIdSched schedule) == roomNo = [(makeScheduleRoom (show (roomIdSched schedule)) (show (hourID schedule)) (hour schedule) ["Not Available"])] ++ bookRoomKu rest roomNo hourSlot
        --(hourID schedule) == slot = [(makeScheduleRoom (show (roomIdSched schedule)) (show (hourID schedule)) (hour schedule) ["Not Available"])] ++ bookRoomKu rest roomnumber slot
        | ((roomIdSched schedule) == roomNo) && ((hourID schedule) == hourSlot) = [(makeScheduleRoom (show (roomNo)) (show (hourSlot)) (hour schedule) ["Available"])] ++ cancelRoomKu rest roomNo hourSlot
        | otherwise = [schedule] ++ cancelRoomKu rest roomNo hourSlot



main :: IO ()
main = do
        rooms <- fmap parseRoom (readFile "model/room.txt")
        schedule <- fmap parseSchedule (readFile "model/reservation.txt")
        runReservation rooms schedule []
        {-
        f <- readFile "model/reservation.txt"
        let resultku = lines f
        case length resultku of
                0 -> putStrLn "Empty"
                1 -> putStrLn "Cuma 1"
                _ -> print resultku
       
        let me = splitOn "|" (head resultku)
        print me
        
        runReservation rooms []
        
                let result = addMe 6 5
                print result
        -}

