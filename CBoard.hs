{-# LANGUAGE BangPatterns #-}
module CBoard where

import Common
import Data.Bits
import Data.List

import Color 
import Command

(<<<) :: Places -> Int -> Places
(<<<) = shiftL
infixl 8 <<<

(>>>) :: Places -> Int -> Places
(>>>) = shiftR
infixl 8 >>>

(|||) :: Places -> Places -> Places
(|||) = (.|.)
infixl 5 |||

(&&&) :: Places -> Places -> Places
(&&&) = (.&.)
infixl 7 &&&

{- utility functions for Places -}
placesToPositions :: Places -> [(Int,Int)]
placesToPositions !pl =
  map (\x -> (x `mod` 8 + 1, x `div` 8 + 1)) $ sub pl where
  sub 0 = []
  sub x = 
      popCount (x &&& (-x) - 1) : sub (x &&& (x-1))
positionsToPlaces :: [(Int,Int)] -> Places
positionsToPlaces !pl =
  foldr (|||) 0 $ map ( \(i,j) -> 1 <<< (i + 8 * j - 9)) pl 

maskLeft :: Places -> Int -> Places -> Places
maskLeft !mask !l !x = (x &&& mask) `shiftL` l

maskRight :: Places -> Int -> Places -> Places
maskRight !mask !l !x = (x &&& mask) `shiftR` l

-- | Converts a set to a list of places.
-- | Every element in returned list is a power of 2.
setToDisks :: Places -> [Places]
setToDisks 0 = []
setToDisks !set = 
      (set &&& (-set)) : setToDisks (set &&& (set-1))

{- functions for CBoard -}

initCBoard :: CBoard
initCBoard = CBoard 0 0

-- | readCBoard board 2 3 0 returns the bottom of B4.
readCBoard :: CBoard -> Int -> Int -> Int -> Color
readCBoard !board !i !j !h
  | i < 0 || i >= 4 || j < 0 || j >= 4 || h < 0 || h >= 4 = sentinel
  | otherwise                            = readCBoardUnsafe board i j h

readCBoardUnsafe :: CBoard -> Int -> Int -> Int -> Color
readCBoardUnsafe (CBoard !bl !wh) !i !j !h =
  let ind = 16 * j + 4 * i + h
      mask= 1 <<< ind :: Places
      bbit = bl .&. mask
      wbit = wh .&. mask
    in
     if bbit /= 0 then black else if wbit /= 0 then white else none
  
countC :: CBoard -> Color -> Int 
countC (CBoard bl wh) color 
  | color == black = popCount bl
  | color == white = popCount wh
  | otherwise      = error $ "invalid color:" ++ show color
showCBoard :: CBoard -> String
showCBoard board = 
    " | a     b     c     d     \n" ++
      "-+----------------------\n" ++
       concatMap putBoardLine [0..3] ++
        "  (X: Black,  O: White)"
    where
      putC c | c == none  = " " 
             | c == white = "O"
             | c == black = "X"
             | otherwise  = undefined
      putBoardLine j =
          show (j+1) ++ "|" ++
           concatMap (\i -> putBoardFiber i j) [0..3]
             ++ "\n"
      putBoardFiber i j = 
          "[" ++ concatMap (\h -> let e = readCBoardUnsafe board i j h in 
                             putC e) [0..3] ++ "]"
instance Show CBoard where
  show = showCBoard

