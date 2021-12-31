{-
On the other hand, it might be wise to try a different strategy: let the giant
squid win.

You aren't sure how many bingo boards a giant squid could play at once, so
rather than waste time counting its arms, the safe thing to do is to figure out
which board will win last and choose that one. That way, no matter which boards
it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after
13 is eventually called and its middle column is completely marked. If you were
to keep playing until this point, the second board would have a sum of unmarked
numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score
be?
-}
module Main where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.List

main :: IO ()
main =
  getContents
    >>= print
      . solve [] undefined
      . bisequence
        ( fmap (read :: String -> Int) . splitOn ',' . head,
          fmap (fmap (fmap ((,False) . (read :: String -> Int)) . words)) . splitOn "" . drop 2
        )
      . lines
  where
    solve winners prevDraw (numbers, boards)
      | null numbers =
        fst (head winners) * foldl (\acc (n, s) -> if s then acc else n + acc) 0 (concat $ snd $ head winners)
      | otherwise =
        case find isWinning boards of
          Just winner ->
            solve ((prevDraw, winner) : winners) (head numbers) (tail numbers, boards')
          Nothing ->
            solve winners (head numbers) (tail numbers, boards')
      where
        isWinning =
          bior . bisequence (any (all snd), any (all snd) . transpose)
        boards' =
          fmap (fmap (\(n, s) -> if n == head numbers then (n, True) else (n, s))) <$> filter (not . isWinning) boards
    splitOn n =
      uncurry (:) . second (maybe [] (splitOn n . snd) . uncons) . break (== n)
