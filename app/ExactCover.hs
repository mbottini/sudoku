module ExactCover where

import Data.List (transpose)
import Data.Tuple (swap)

data Row = Row {label :: String, vals :: [Bool]} deriving (Show)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

createBoolList :: Int -> [Int] -> [Bool]
createBoolList limit =
  go 1
  where
    go n []
      | n <= limit = False : go (n + 1) []
      | otherwise = []
    go n (y : ys)
      | n < y = False : go (n + 1) (y : ys)
      | otherwise = (n == y) : go (n + 1) ys

createBitfield :: [[Int]] -> [[Bool]]
createBitfield xss =
  map (createBoolList limit) xss
  where
    limit = maximum . concat $ xss

-- Step 2: Find the column with the fewest Trues in it.
-- None means that said column has _no_ Trues in it,
-- which means that the computation has failed.
chooseColumn :: [Row] -> Maybe Int
chooseColumn [] = Nothing
chooseColumn rs =
  case go rs of
    (0, _) -> Nothing
    (_, n) -> Just n
  where
    go =
      minimum
        . map swap
        . enumerate
        . map (length . filter id)
        . transpose
        . map vals

-- Step 3: Find the rows that have a True in that column.
findRows :: Int -> [Row] -> [Row]
findRows n =
  filter ((!! n) . vals)

commonElement :: Row -> Row -> Bool
commonElement (Row _ v1) (Row _ v2) =
  or $ zipWith (&&) v1 v2

dropCols :: Row -> Row -> Row
dropCols (Row _ v1) (Row l v2) =
  Row l . map snd . filter (not . fst) $ zip v1 v2

-- Step 4: row index, remove all rows that share a True with that row,
-- plus all columns from all rows where that row's column is True.
pareRows :: Row -> [Row] -> [Row]
pareRows r =
  filter (not . null . vals)
    . map (dropCols r)
    . filter (not . commonElement r)

algorithmX :: [Row] -> [[String]]
algorithmX = map (map label) . go
  where
    go [] = [[]] -- Step 1: if the matrix has no columns, terminate successfully (singleton empty list)
    go rs =
      case chooseColumn rs of
        Just col ->
          let selected = findRows col rs
           in concatMap (\r -> map (r :) (go (pareRows r rs))) selected
        Nothing -> []
