{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module AI where



import Data.Bits
import Data.Maybe
import Control.Monad 

import System.Timeout
import Control.Exception
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import Data.Function
import Data.List
import Data.IORef
import Data.Typeable
import Text.Printf

import Color 
import Command
import Common
import CBoard

type Heuristics = Int

winValue :: Int
loseValue :: Int
drawValue :: Int
maxValue :: Int
minValue :: Int

winValue  =  10000000
loseValue = -10000000
drawValue = - 5000000
maxValue  =  100000000
minValue  = -100000000

data StopSearch = StopSearch String deriving (Show, Typeable)

instance Exception StopSearch 

-- | These functions prefer the second value if two equal.
maxBy :: (a -> a -> Ordering) -> a -> a -> a
minBy :: (a -> a -> Ordering) -> a -> a -> a

maxBy comp x y = case comp x y of
   GT -> x
   _  -> y

minBy comp x y = case comp x y of
   LT -> x
   _  -> y

weightPlayPop :: Places -> Int
weightPlayPop !pl = popCount (pl &&& 0x9009000000009009) * 2 + popCount pl -- corner is 3-point.

gameEnd :: Places -> Places -> Bool
gameEnd my opp = isVictory my || isVictory opp || (my ||| opp == -1)

-- | timeout : timeout in microseconds (us)
myPlay :: CBoard -> Color -> Heuristics -> Int -> Bool ->IO Places
myPlay board color mode time losing = do
    -- let ms = validMovesC board color in
    let ms = setToDisks $ legalMoves board in 
       case ms of 
         [] -> return 0 -- pass (game end)
         _  -> 
             do
                boards <- newIORef $ map (\mv -> (mv, doMoveC board mv color)) ms
                opt <- newIORef (Nothing :: Maybe ([Places], Int))
                numBoards <- newIORef 0 :: IO (IORef Int)
                _ <- timeout time $ catch (
                  forM_ [0..] (nextMoveDepth board boards color mode opt numBoards (if losing then -1 else 1))
                 ) (\(StopSearch str) -> putStrLn str) -- this always fails and opt is modified with the result
                optmv <- readIORef opt
                numBoardsVal <- readIORef numBoards
                printf "summary:\n depth : %d\n total #boards: %d\n" (fromMaybe (-1) (fmap (length . fst) optmv)) numBoardsVal :: IO ()
                let mv = case optmv of { Nothing -> head ms; Just (o:_, _) -> o; _ -> undefined; }
                return mv;

-- overwrites opt and stort the optimal value
-- boards is rearranged after operation
nextMoveDepth :: CBoard -> IORef [(Places, CBoard)] -> Color -> Heuristics -> IORef (Maybe ([Places], Int)) -> IORef Int -> Int -> Int -> IO ()
nextMoveDepth _board boardsRef color mode opt numBoards factor depth = do
       curopt <- readIORef opt
       boards <- readIORef boardsRef
       oldNum <- readIORef numBoards
       beta  <- newIORef maxValue -- passed to alphaBeta as `beta'
       case curopt of
         Nothing -> return ()
         Just (_, curoptval) -> do
           when (curoptval >= winValue) $ throwIO (StopSearch $ printf "Path to the victory was detected. (depth = %d)" (depth - 1))
           when (curoptval <= loseValue) $ throwIO (StopSearch $ printf "Path to the defeat was detected. (depth = %d)" (depth - 1))
           when (curoptval <= drawValue) $ throwIO (StopSearch $ printf "Path to the draw was detected. (depth = %d)" (depth - 1))
       vals <- forM boards $ \(mv, CBoard bdbl bdwh) -> do
         let (!my, !opp) = if color == black then (bdbl,bdwh) else (bdwh, bdbl)
         cbeta <- readIORef beta
         valPath <- alphaBeta mode opp my depth minValue (cbeta + 1) numBoards factor -- minvalue <= val <= beta + 1
         modifyIORef beta (min (fst valPath)) -- pruning
         return (mv, valPath)
       let valsSort = sortBy (compare `on` (fst . snd)) vals -- sort by value, smallest(best) is first
       writeIORef boardsRef $ map (\(mv, _) -> (mv, fromMaybe (error "(>_<)") $ lookup mv boards)) valsSort
       let (ij, (optval, path)) = if null vals then undefined else head valsSort
       let wholePath = ij : (fromMaybe [] path) -- path is Just _
       let (i, j) = conv ij
       printf "depth = %d, move = (%d, %d), value = %d\n" depth i j (-optval) :: IO ()
       putStrLn $ "path = " ++ show (map showMove wholePath)
       newNum <- readIORef numBoards
       printf "number of boards in depth %d: %d\n" depth (newNum - oldNum) :: IO ()
       writeIORef opt $ Just (wholePath, -optval)
        where
          conv x = let t = popCount (x-1) in (t `mod` 4 + 1, t `div` 4 + 1)

alphaBeta :: Heuristics -> Places -> Places -> Int -> Int -> Int -> IORef Int -> Int -> IO (Int, Maybe [Places])
alphaBeta mode my opp depth alpha beta numBoards factor = do
  let isGameEnd = gameEnd my opp
  if isGameEnd || depth == 0 then do
     let result = staticEval my opp mode isGameEnd * factor
     modifyIORef' numBoards (+1)
     return (min result beta, Just [])
  else do
    aref <- newIORef (alpha, Nothing)
    let ms = legalMovesMO my opp
    if ms == 0 then do
        (result, path) <- alphaBeta mode opp my (depth - 1) (-beta) (-alpha) numBoards factor
        modifyIORef' aref (maxBy (compare `on` fst) (-result, fmap (0 :) path))
    else
     forM_ (setToDisks ms) $ \disk -> do
      (calpha, _) <- readIORef aref
      if calpha >= beta then do
         writeIORef aref (beta, Nothing)
      else do 
        let (nxtopp, nxtmy) = (my ||| disk, opp)
        (result, path) <- alphaBeta mode nxtmy nxtopp (depth - 1) (-beta) (-calpha) numBoards factor
        modifyIORef' aref (maxBy (compare `on` fst) (-result, fmap (disk :) path))
    readIORef aref

staticEval :: Places -> Places -> Heuristics -> Bool -> Int
staticEval my opp mode isGameEnd = 
  if isGameEnd then
    if isVictory my then
      winValue
    else
      loseValue
  else
    ([eval1] !! (mode - 1)) my opp

eval1 :: Places -> Places -> Int
eval1 my opp =
  let !myc = popCount my
      !oppc = popCount opp
      !msmy = legalMovesMO my opp
      !msopp = legalMovesMO opp my
    in
  if myc + oppc >= 50 then -oppc else 4 * (weightPlayPop my - weightPlayPop opp) + weightPlayPop msmy - weightPlayPop msopp

