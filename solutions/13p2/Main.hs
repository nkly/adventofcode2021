{-
Finish folding the transparent paper according to the instructions. The manual
says the code is always eight capital letters.

What code do you use to activate the infrared thermal imaging camera system?
-}
module Main where

import Data.Bifunctor
import Data.Bitraversable
import qualified Data.Matrix as M

main :: IO ()
main =
  getContents >>= print . solve . bimap parseCoords (parseFolds . tail) . break null . lines
  where
    solve (coords, folds) =
      M.transpose $ foldAll folds $ toMatrix coords
    foldAll [] m =
      m
    foldAll ((dir, coord) : fs) m
      | dir == "y" =
        foldAll fs $ M.elementwise max yfst (reverseCols ysnd)
      | dir == "x" =
        foldAll fs $ M.elementwise max xfst (reverseRows xsnd)
      | otherwise =
        error $ "invalid direction: '" <> dir <> "'"
      where
        yfst =
          M.submatrix (0, 0) (M.nrows m - 1, coord - 1) m
        ysnd =
          M.submatrix (0, coord + 1) (M.nrows m - 1, M.ncols m - 1) m
        xfst =
          M.submatrix (0, 0) (coord - 1, M.ncols m - 1) m
        xsnd =
          M.submatrix (coord + 1, 0) (M.nrows m - 1, M.ncols m - 1) m
    toMatrix coords =
      (M.imap (\p _ -> if p `elem` coords then 1 else 0) . uncurry M.zero) $
        bisequence (nextOdd . maximum . fmap fst, nextOdd . maximum . fmap snd) coords
    reverseRows m =
      foldl (\m' r -> M.swapRows r (M.nrows m' - r - 1) m') m [0 .. M.nrows m `div` 2]
    reverseCols m =
      foldl (\m' c -> M.swapCols c (M.ncols m' - c - 1) m') m [0 .. M.ncols m `div` 2]
    parseCoords =
      fmap (bimap (read :: String -> Int) ((read . tail) :: String -> Int) . break (== ','))
    parseFolds =
      fmap (second ((read . tail) :: String -> Int) . break (== '=') . last . words)
    nextOdd x
      | odd x = x + 2
      | otherwise = x + 1
