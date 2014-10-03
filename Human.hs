module Main(readHuman, readMove, main, repl) where

import Common
import Command
import CBoard
import Color
import Data.Bits
import Data.List
import Data.Maybe
import System.IO
import AI

readHuman :: CBoard -> Color -> IO Places

readHuman board@(CBoard bl wh) color = do
    print board;
    let bc = popCount bl
        wc = popCount wh
        valid = legalMoves board
    putStrLn $ "black:" ++ show bc ++ " white:" ++ show wc
    putStrLn $ "You: " ++ showColor color
    if valid == 0 then do
        putStrLn "You have to pass."
        return 0
    else
        readMove valid



readMove :: Places -> IO Places
readMove valid = do
    putStr "> "
    hFlush stdout
    line <- getLine :: IO String
    if length line /= 2 then do
        putStrLn "invalid format (input must be of form \"a4\" (quotes for clarity)"
        readMove valid
     else do
        let [col,row] = line
        if not (elem col "abcd") then do
            putStrLn $ "invalid column: " ++ [col]
            readMove valid
         else do
            if not (elem row "1234") then do
                putStrLn $ "invalid row: " ++ [row]
                readMove valid
             else do
                let (i,j) = (fromMaybe (error "???") (findIndex (== col) "abcd") + 1, fromMaybe (error "???") (findIndex (== row) "1234") + 1)
　　　　　　　　　　　　　　　　　　　　mask = 0xf <<< (4 * i + 16 * j - 20)
                if valid &&& mask /= 0 then
                   let tt = valid &&& mask in return (tt &&& (-tt))
                else do
                  putStrLn $ "invalid move: " ++ show (M i j)
                  readMove valid


repl :: CBoard -> Color -> IO ()
repl board@(CBoard bl wh) color = do
  if gameEnd bl wh then
    putStrLn "***** game end *****"
  else do
    mv <- readHuman board color
    let newb = doMoveC board mv color
    aiMv <- myPlay newb (oppositeColor color) 1 500000 False
    repl (doMoveC newb aiMv (oppositeColor color)) color

main :: IO ()
main = repl (CBoard 0 0) black
