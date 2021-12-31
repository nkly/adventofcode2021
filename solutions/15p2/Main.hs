{-
Now that you know how to find low-risk paths in the cave, you can try to find
your way out.

The entire cave is actually five times larger in both dimensions than you
thought; the area you originally scanned is just one tile in a 5x5 tile area
that forms the full map. Your original map tile repeats to the right and
downward; each time the tile repeats to the right or downward, all of its risk
levels are 1 higher than the tile immediately up or left of it. However, risk
levels above 9 wrap back around to 1. So, if your original map had some
position with a risk level of 8, then that same position on each of the 25
total tiles would be as follows:

8 9 1 2 3
9 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7

Each single digit above corresponds to the example position with a value of 8
on the top-left tile. Because the full map is actually five times larger in
both dimensions, that position appears a total of 25 times, once in each
duplicated tile, with the values shown above.

Using the full map, what is the lowest total risk of any path from the top left
to the bottom right?
-}
module Main where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S
import Data.Vector ((!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

main :: IO ()
main =
  getContents >>= solve . extend . parse >>= print
  where
    parse =
      V.fromList . fmap (V.fromList . fmap digitToInt) . lines
    extend =
      join . V.unfoldrExactN 5 (\g -> (g, incRow <$> g)) . fmap extendRow
      where
        extendRow =
          join . V.unfoldrExactN 5 (\r -> (r, incRow r))
        incRow =
          fmap (\x -> max 1 ((x + 1) `mod` 10))
    solve graph = do
      dist <- M.generateM glen (\x -> M.generate glen (\y -> if (x, y) == (0, 0) then 0 else maxBound))
      visited <- M.replicateM glen (M.replicate glen False)
      dijkstra dist queue visited graph
      M.read dist (glen - 1) >>= \r -> M.read r (glen - 1)
      where
        queue =
          S.fromList
            [ if (x, y) == (0, 0)
                then (0, (x, y))
                else (maxBound, (x, y))
              | x <- [0 .. glen - 1],
                y <- [0 .. glen - 1]
            ]
        glen =
          V.length graph
    dijkstra !dist !queue !visited !graph
      | S.null queue =
        pure ()
      | otherwise =
        isVisited u >>= \case
          True ->
            dijkstra dist q' visited graph
          False -> do
            setVisited u
            let processNeighbors q (v, w) = do
                  d' <- distOf v
                  if d + w < d'
                    then do
                      setDistOf v (d + w)
                      pure $ S.insert (d + w, v) q
                    else pure q
            queue' <- foldlM processNeighbors q' =<< neighborsOf u
            dijkstra dist queue' visited graph
      where
        ((d, u), q') =
          S.deleteFindMin queue
        neighborsOf (vx, vy) =
          mapMaybe (\v -> (v,) <$> weightOf v)
            <$> filterM
              (fmap not . isVisited)
              [(vx -1, vy), (vx + 1, vy), (vx, vy -1), (vx, vy + 1)]
        distOf (vx, vy) =
          M.read dist vx >>= \r -> M.read r vy
        setDistOf (vx, vy) val =
          M.read dist vx >>= \r -> M.write r vy val
        isVisited (vx, vy)
          | vx < 0 || vx >= V.length graph || vy < 0 || vy >= V.length graph =
            pure True
          | otherwise =
            M.read visited vx >>= \r -> M.read r vy
        setVisited (vx, vy) =
          M.read visited vx >>= \r -> M.write r vy True
        weightOf (vx, vy) =
          graph !? vx >>= (!? vy)
