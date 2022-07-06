module Main where

import System.IO (hFlush, stdout)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Data.List.Split
import Data.Time
import Misc (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)

import Message (LogMessage)
--import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import Room (Room (UnknownRoom), parseRoom, parseLogRoom, addNewRoom, deleteRoom, roomId, roomName, roomLocation, roomCapacity, roomPIC, roomDesc)
import Schedule (Schedule (UnknownSchedule), deleteSchedule, resetRoomKu, resetRoomKu, bookRoomKu, cancelRoomKu, parseSchedule, parseLogSchedule, makeScheduleRoom, roomIdSched, hourID, hour, status, bookedBy, agenda)

runReservation :: [Room] -> [Schedule] -> [Schedule] -> [LogMessage] -> IO ()
runReservation rooms schedule tempSchedule messages = do
    datetoday <- fmap (formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S") getZonedTime

    putStrLn "\n"     
    putStrLn "|++++++++++++++++++++++++++++++++++++++++++++++++++|"
    putStrLn "|++++++++++++++ List of Meeting Room ++++++++++++++|"
    putStr   "|++++++++++++++ "
    putStr . show $ datetoday
    putStrLn " +++++++++++++|"
    putStrLn "|++++++++++++++++++++++++++++++++++++++++++++++++++|"
    putStrLn "\n" 
    putStrLn $ showAll rooms
    
    
    putStrLn "(a) Show all room (b) Show meeting room schedule (c) Book Room (d) Cancel Booking\n(e) Add Room (f) Delete Room (r) Reset Status (x) Exit program"
    choice <- prompt "Input choice: "
    --user choice
    case choice of
        "a" -> do
                --show all meeting room
                putStrLn $ showAll rooms

                empty <- prompt "Press enter to go back"
                runReservation rooms schedule tempSchedule messages

        "b" -> do
                --show all schedule of the choosen meeting room
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

                empty <- prompt "Press enter to go back"
                runReservation rooms schedule tempSchedule messages

        "c" -> do
                --booking the slot of the meeting room
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
                bookedBy <- prompt "PIC of agenda: "
                agenda <- prompt "Agenda: "
        
                newSchedule <- return (bookRoomKu schedule roomNo hourSlot bookedBy agenda)
                parseLogSchedule newSchedule        
                
                emptyPrompt <- prompt "Press enter to continue."
                runReservation rooms newSchedule tempSchedule messages
        
        "d" -> do
                --cancel booked slot of the meeting room
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
        
                newSchedule <- return (cancelRoomKu schedule roomNo hourSlot)
                parseLogSchedule newSchedule        
                
                emptyPrompt <- prompt "Press enter to continue."
                runReservation rooms newSchedule tempSchedule messages
        
        "e" -> do
                -- Add new meeting room
                putStrLn "Please enter the following information to add new meeting room: "

                roomName <- prompt "Meeting Room's name: "
                roomLoc <- prompt "Location: "
                roomCap <- prompt "Capacity: "
                roomPIC <- prompt "PIC: "
                roomDesc <- prompt "Description: "

                newRoom <- addNewRoom rooms roomName roomLoc roomCap roomPIC roomDesc schedule tempSchedule
                parseLogRoom newRoom
                emptyPrompt <- prompt "Press enter to continue."
                runReservation newRoom schedule tempSchedule messages
        
        "f" -> do
                -- Delete meeting room
                putStrLn "You're about to delete the Meeting Room"
                
                putStr "Please enter Id Room to be deleted: "
                hFlush stdout
                roomId <- do
                        result <- runMaybeT maybeReadInt
                        case result of
                                (Just b) -> return b
                                Nothing -> return 0

                newRoom <- return (deleteRoom rooms roomId)
                parseLogRoom newRoom
                emptyPrompt <- prompt "Press enter to continue."
                runReservation newRoom schedule tempSchedule messages
        
        "r" -> do
                --let status = "Not Available"
                newSchedule <- return (resetRoomKu schedule)
                parseLogSchedule newSchedule
     
                emptyPrompt <- prompt "Press enter to continue."
                runReservation rooms newSchedule tempSchedule messages
        
        "x" -> do
                putStrLn "Exiting program..."
                putStrLn "Goodbye!"

        _ -> do
                empty <- prompt "Wrong input! Press enter to try again."
                runReservation rooms schedule tempSchedule messages


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
        "No." ++ show (hourID schedule) ++ "\tHour: " ++ hour schedule ++ "\tStatus: " ++ status schedule ++ "\tBooked by: " ++ bookedBy schedule ++ "\tAgenda: " ++ agenda schedule
        ++ "\n"
        ++ showAllSchedule rest

showRoomSchedule :: [Schedule] -> Int -> String
showRoomSchedule [] _ = replicate 52 '='
showRoomSchedule (schedule : rest) roomIdChoosen
        | (roomIdSched schedule) == roomIdChoosen = "No." ++ show (hourID schedule) ++ "\tHour: " ++ hour schedule ++ "\tStatus: " ++ status schedule ++ "\tBooked by: " ++ bookedBy schedule ++ "\tAgenda: " ++ agenda schedule ++ "\n" ++ showRoomSchedule rest roomIdChoosen
        | otherwise = showRoomSchedule rest roomIdChoosen

main :: IO ()
main = do
        
        rooms <- fmap parseRoom (readFile "model/room.txt")
        schedule <- fmap parseSchedule (readFile "model/reservation.txt")
        tempSchedule <- fmap parseSchedule (readFile "model/temp_reservation.txt")
        
        currTime <- utctDayTime <$> zonedTimeToUTC <$> getZonedTime
        if currTime > 50400 --50400 adalah jam 9 malam utc
                then do
                        newSchedule <- return (resetRoomKu schedule)
                        --parseLogSchedule newSchedule
                        --putStrLn "\n"
                        --emptyPrompt <- prompt "Press enter to continue."
                        runReservation rooms newSchedule tempSchedule []
                else 
                        runReservation rooms schedule tempSchedule []
        {-
        f <- readFile "model/reservation.txt"
        let resultku = lines f
        case length resultku of
                0 -> putStrLn "Empty"
                1 -> putStrLn "Cuma 1"
                _ -> print resultku
        let indexku = indexOf roomIdChoosen rooms
        let me = splitOn "|" (head resultku)
        print me
        
        runReservation rooms []
        
                let result = addMe 6 5
                print result
        -}

