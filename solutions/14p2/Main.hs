{-
The resulting polymer isn't nearly strong enough to reinforce the submarine.
You'll need to run more steps of the pair insertion process; a total of 40
steps should do it.

In the above example, the most common element is B (occurring 2192039569602
times) and the least common element is H (occurring 3849876073 times);
subtracting these produces 2188189693529.

Apply 40 steps of pair insertion to the polymer template and find the most and
least common elements in the result. What do you get if you take the quantity
of the most common element and subtract the quantity of the least common
element?
-}
module Main where

import Data.Bifoldable
import Data.Bitraversable
import qualified Data.Map.Strict as M

main :: IO ()
main =
  getContents >>= print . count . uncurry (iter 40) . parse
  where
    count =
      bisum . bisequence (maximum, negate . minimum) . M.foldlWithKey (\acc p c -> if length p > 1 then acc else c : acc) []
    iter step polymer rules
      | step == 0 =
        polymer
      | otherwise =
        iter (step - 1) polymer' rules
      where
        polymer' =
          M.foldlWithKey (\m p c -> matchAndInsert p c m) M.empty polymer
        matchAndInsert p c =
          case M.lookup p rules of
            Just x ->
              M.insertWith (+) [head p, x] c
                . M.insertWith (+) [x, last p] c
                . M.insertWith (+) [x] c
            Nothing ->
              M.insertWith (+) p c
    parse =
      bisequence (toPairMap . head, M.fromList . fmap (bisequence (take 2, last)) . drop 2) . lines
    toPairMap str =
      foldl (\m p -> M.insertWith (+) p 1 $ M.insertWith (+) (init p) 1 m) (M.singleton [last str] 1) $ biList <$> zip str (tail str)
