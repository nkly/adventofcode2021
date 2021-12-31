{-
You've almost reached the exit of the cave, but the walls are getting closer
together. Your submarine can barely still fit, though; the main problem is that
the walls of the cave are covered in chitons, and it would be best not to bump
any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two
dimensions. The shape of the cavern resembles a square; a quick scan of chiton
density produces a map of risk level throughout the cave (your puzzle input).
For example:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

You start in the top left position, your destination is the bottom right
position, and you cannot move diagonally. The number at each position is its
risk level; to determine the total risk of an entire path, add up the risk
levels of each position you enter (that is, don't count the risk level of your
starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the lowest total risk. In this example, a path
with the lowest total risk is highlighted here:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

The total risk of this path is 40 (the starting position is never entered, so
its risk is not counted).

What is the lowest total risk of any path from the top left to the bottom
right?
-}
module Main where

import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Vector ((!?))
import qualified Data.Vector as V

main :: IO ()
main =
  getContents >>= print . solve . parse
  where
    parse =
      V.fromList . fmap (V.fromList . fmap digitToInt) . lines
    solve graph =
      M.findWithDefault (-1) end dist
      where
        dist =
          dijkstra (M.singleton (0, 0) 0) allVertices graph
        allVertices =
          S.fromList [(x, y) | x <- [0 .. (V.length graph - 1)], y <- [0 .. (V.length graph - 1)]]
        end =
          (V.length graph - 1, V.length graph - 1)
    dijkstra dist queue graph
      | null queue =
        dist
      | otherwise =
        dijkstra dist' (S.delete u queue) graph
      where
        dist' =
          foldl (\ds (v, w) -> let d' = d + w in if d' < distOf v then M.insert v d' ds else ds) dist $ filter (\(v, _) -> S.member v queue) $ neighborsOf u
        (u, d) =
          S.foldl (\(cv, cd) v' -> let d' = distOf v' in if d' < cd then (v', d') else (cv, cd)) ((-1, -1), maxBound) queue
        neighborsOf (vx, vy) =
          mapMaybe (\v -> (v,) <$> weightOf v) [(vx -1, vy), (vx + 1, vy), (vx, vy -1), (vx, vy + 1)]
        distOf v =
          M.findWithDefault maxBound v dist
        weightOf (vx, vy) =
          graph !? vx >>= (!? vy)
