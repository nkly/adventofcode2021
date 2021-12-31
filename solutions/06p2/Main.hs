{-
Suppose the lanternfish live forever and have unlimited food and space. Would
they take over the entire ocean?

After 256 days in the example above, there would be a total of 26984457539
lanternfish!

How many lanternfish would there be after 256 days?
-}
module Main where

import Data.Bifunctor
import Data.Bitraversable
import Data.List
import qualified Data.Vector as V

main :: IO ()
main =
  getContents >>= print . sum . model 0 . V.accum (+) (V.replicate 9 0) . fmap ((,1) . (read :: String -> Int)) . splitOn ','
  where
    model day population
      | day == 256 =
        population
      | otherwise =
        model (succ day) $ V.accum (+) (V.snoc t h) [(6, h)]
      where
        (h, t) =
          bisequence (V.head, V.tail) population
    splitOn n =
      uncurry (:) . second (maybe [] (splitOn n . snd) . uncons) . break (== n)
