module Common where

import Data.Word

-- | A set of places in 3d4moku board.

type Places = Word64

-- | A efficient data structure for states of 3d4moku board.
-- | each bit corresponds to a cell in the board:
-- |   a      b      c       d
-- | 1 [0..3] [4..7] [8..11] [12..15]
-- |   ...
-- | 4 [48..51] [52..55] [56..59] [60..63]
-- | (i,j) <====> 8*(j-1)+(i-1)
-- | the first Places represents the positions of black balls.
-- | the second Places represents the positions of white balls.
data CBoard = CBoard !Places !Places


