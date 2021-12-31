{-
It seems like the individual flashes aren't bright enough to navigate. However,
you might have a better option: the flashes seem to be synchronizing!

In the example above, the first time all octopuses flash simultaneously is step
195:

After step 193:
5877777777
8877777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777

After step 194:
6988888888
9988888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888

After step 195:
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000

If you can calculate the exact moments when the octopuses will all flash
simultaneously, you should be able to navigate through the cavern. What is the
first step during which all octopuses flash?
-}
module Main where

import Data.Char
import Data.Foldable
import Data.Traversable
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

main :: IO ()
main =
  getContents >>= (V.thaw . V.fromList . fmap digitToInt . concat . lines) >>= solve 1000 >>= print
  where
    solve maxSteps world =
      succ . length . takeWhile not <$> for [1 .. maxSteps] simulate
      where
        simulate _ = do
          forM_ [0 .. size - 1] (M.modify world (+ 1))
          flashAll
          forM_ [0 .. size - 1] (M.modify world (max 0))
          M.foldl (\acc x -> acc && (x == 0)) True world
        flashAll = do
          n <- M.ifoldM' flashOnce 0 world
          if n > 0 then flashAll else pure ()
        flashOnce n pos lvl
          | lvl <= 9 =
            pure n
          | otherwise = do
            forM_ (neighbors pos) (M.modify world (+ 1))
            M.write world pos minBound
            pure $ succ n
        neighbors pos =
          toPos <$> filter validPos [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1]]
          where
            (r, c) =
              pos `divMod` ncols
            toPos (r', c') =
              r' * ncols + c'
            validPos (r', c') =
              r' >= 0 && r' < nrows && c' >= 0 && c' < ncols && (r', c') /= (r, c)
        ncols =
          10
        nrows =
          10
        size =
          ncols * nrows
