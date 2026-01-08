module Sudoku where

import Data.List
import Data.Maybe (fromJust, mapMaybe)
import ExactCover
import Text.Read

data Mark = Mark {row :: Int, col :: Int, number :: Int} deriving (Show, Eq, Ord)

replaceAt :: a -> Int -> [a] -> [a]
replaceAt x n xs =
  case splitAt n xs of
    (ys, _ : zs) -> ys ++ (x : zs)
    _ -> error "This should never happen"

-- There are 4 different types of constraints:
-- 1. Cell constraints: Every cell must have a number.
-- 2. Row constraints: Each row must have all numbers from 1-9 with no repeats.
-- 3. Column constraints: Each column must have all numbers from 1-9 with no
--    repeats.
-- 4. Block constraints: Each of the 9 9-cell blocks must have all numbers from
--    1-9 with no repeats.
--
-- Given a Mark, we can generate a bitfield representing which constraints each
-- mark meets and can conflict with.

-- There are 81 cell constraints, one for each cell in the grid. We can number
-- them left to right and top to bottom, as follows:
--
-- 0, 1, 2, ... , 7, 8, 9,
-- 10, 11, 12, ... , 16, 17, 18,
-- ...
-- 72, 73, 74, ... , 78, 79, 80
--
-- The actual number is, of course, irrelevant for the cell constraint, since the only
-- thing that matters is that the cell is filled.

genCellConstraint :: Mark -> [Bool]
genCellConstraint (Mark {row = r, col = c}) =
  let idx = 9 * (r - 1) + (c - 1)
   in replaceAt True idx $ replicate 81 False

-- There are 81 cell constraints, one for each number in each row. The first 9 are Row 1's
-- constraints, the second 9 are Row 2's constraints, etc.
-- This time the number and row matter, but the column doesn't!

genRowConstraint :: Mark -> [Bool]
genRowConstraint (Mark {row = r, number = n}) =
  let idx = 9 * (r - 1) + (n - 1)
   in replaceAt True idx $ replicate 81 False

-- There are 81 column constraints, one for each number in each column. The first 9 are
-- Column 1's constraints, the second 9 are Column 2's constraints, etc.
-- This time the column and number matter, but the row doesn't!

genColConstraint :: Mark -> [Bool]
genColConstraint (Mark {col = c, number = n}) =
  let idx = 9 * (c - 1) + (n - 1)
   in replaceAt True idx $ replicate 81 False

-- There are 81 block constraints, one for each number in each block. As with the cells, we
-- number each block from left to right and top to bottom. Thus
--
-- 0, 1, 2
-- 3, 4, 5
-- 6, 7, 8
--
-- This requires the most math to determine the indices, since we're doing a bunch of modulo
-- fun to figure out which block each row/column we're in, and then adding the number.

genBlockConstraint :: Mark -> [Bool]
genBlockConstraint (Mark {row = r, col = c, number = n}) =
  replaceAt True idx $ replicate 81 False
  where
    (rOffset, cOffset) = ((r - 1) `div` 3, (c - 1) `div` 3)
    blockNumber = rOffset * 3 + cOffset
    idx = blockNumber * 9 + (n - 1)

genConstraint :: Mark -> [Bool]
genConstraint m =
  concatMap
    ($ m)
    [ genCellConstraint,
      genRowConstraint,
      genColConstraint,
      genBlockConstraint
    ]

product3 :: [a] -> [b] -> [c] -> [(a, b, c)]
product3 xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

genAllConstraints :: [Row Mark]
genAllConstraints =
  let marks = map (uncurry3 Mark) $ product3 [1 .. 9] [1 .. 9] [1 .. 9]
   in zipWith Row marks (map genConstraint marks)

parseMark :: Int -> Int -> Char -> Maybe Mark
parseMark r c nChar =
  Mark r c <$> readMaybe (return nChar)

parseMarks :: [String] -> [Mark]
parseMarks xss =
  let grid = concat (zipWith go [1 ..] xss)
   in mapMaybe (uncurry3 parseMark) grid
  where
    go r = zipWith (\c x -> (r, c, x)) [1 ..]

applyMark :: [Row Mark] -> Mark -> [Row Mark]
applyMark field m =
  let r = fromJust . find ((== m) . label) $ field
   in pareRows r field

applyMarks :: [Row Mark] -> [Mark] -> [Row Mark]
applyMarks =
  foldl applyMark

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
  take n xs : chunk n (drop n xs)

showSolvedPuzzle :: [Mark] -> String
showSolvedPuzzle =
  unlines
    . chunk 9
    . concatMap (show . number)

solvePuzzle :: [String] -> [[Mark]]
solvePuzzle strings =
  let initialMarks = parseMarks strings
   in map (sort . (++ initialMarks))
        . algorithmX
        . applyMarks genAllConstraints
        $ initialMarks
