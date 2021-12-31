{-
Unfortunately, considering only horizontal and vertical lines doesn't give you
the full picture; you need to also consider diagonal lines.

Because of the limits of the hydrothermal vent mapping system, the lines in
your list will only ever be horizontal, vertical, or a diagonal line at exactly
45 degrees. In other words:

    An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
    An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.

Considering all lines from the above example would now produce the following
diagram:

1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111....

You still need to determine the number of points where at least two lines
overlap. In the above example, this is still anywhere in the diagram with a 2
or larger - now a total of 12 points.

Consider all of the lines. At how many points do at least two lines overlap?
-}
module Main where

import Data.Bifunctor
import Data.Bitraversable
import Data.Char
import Data.List
import qualified Data.Vector as V

main :: IO ()
main =
  getContents
    >>= print
      . V.length
      . V.filter (>= 2)
      . V.accum (+) (V.replicate (fieldSize * fieldSize) 0)
      . concatMap createLine
      . fmap
        ( bisequence
            ( readPos . takeWhile (not . isSpace),
              readPos . drop 4 . dropWhile (not . isSpace)
            )
        )
      . lines
  where
    createLine ((x, y), (x', y'))
      | x == x' =
        fmap ((,1) . posToIdx . (x,)) [min y y' .. max y y']
      | y == y' =
        fmap ((,1) . posToIdx . (,y)) [min x x' .. max x x']
      | otherwise =
        (,1) . posToIdx <$> ((x, y) : unfoldr nextPoint (x, y))
      where
        nextPoint (x'', y'')
          | x'' == x' || y'' == y' = Nothing
          | otherwise = Just ((dx x'', dy y''), (dx x'', dy y''))
        dx =
          if x < x' then (+ 1) else flip (-) 1
        dy =
          if y < y' then (+ 1) else flip (-) 1
    fieldSize =
      1000
    posToIdx (x, y) =
      fieldSize * x + y
    readPos =
      bimap (read :: String -> Int) ((read :: String -> Int) . tail) . break (== ',')
