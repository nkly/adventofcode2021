{-
Through a little deduction, you should now be able to determine the remaining
digits. Consider again the first example above:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf

After some careful analysis, the mapping between signal wires and segments only
make sense in the following configuration:

 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc

So, the unique signal patterns would correspond to the following digits:

    acedgfb: 8
    cdfbe: 5
    gcdfa: 2
    fbcad: 3
    dab: 7
    cefabd: 9
    cdfgeb: 6
    eafb: 4
    cagedb: 0
    ab: 1

Then, the four digits of the output value can be decoded:

    cdfeb: 5
    fcadb: 3
    cdfeb: 5
    cdbaf: 3

Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example above,
the output value of each entry can be determined:

    fdgacbe cefdb cefbgd gcbe: 8394
    fcgedb cgb dgebacf gc: 9781
    cg cg fdcagb cbg: 1197
    efabcd cedba gadfec cb: 9361
    gecf egdcabf bgf bfgea: 4873
    gebdcfa ecba ca fadegcb: 8418
    cefg dcbef fcge gbcadfe: 4548
    ed bcgafe cdgba cbgef: 1625
    gbdfcae bgc cg cgb: 8717
    fgae cfgab fg bagce: 4315

Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode the
four-digit output values. What do you get if you add up all of the output
values?
-}
module Main where

import Data.Bifunctor
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Tuple

main :: IO ()
main =
  getContents
    >>= print
      . sum
      . fmap
        ( uncurry (decode [])
            . bimap
              (findNums [] . sortOn S.size . fmap S.fromList . words . init)
              (fmap S.fromList . words . drop 2)
            . break (== '|')
        )
      . lines
  where
    decode acc _ [] =
      sum $ zipWith (*) acc $ unfoldr (\x -> Just (x, x * 10)) 1
    decode acc m (x : xs) =
      decode (fromJust (lookup x m) : acc) m xs
    findNums acc [] =
      fmap swap acc
    findNums acc (x : xs)
      | s == 2 =
        findNums ((1, x) : acc) xs
      | s == 3 =
        findNums ((7, x) : acc) xs
      | s == 4 =
        findNums ((4, x) : acc) xs
      | s == 5 && matches seven =
        findNums ((3, x) : acc) xs
      | s == 5 && S.size (S.intersection x four) == 3 =
        findNums ((5, x) : acc) xs
      | s == 5 =
        findNums ((2, x) : acc) xs
      | s == 6 && matches four =
        findNums ((9, x) : acc) xs
      | s == 6 && matches seven =
        findNums ((0, x) : acc) xs
      | s == 6 =
        findNums ((6, x) : acc) xs
      | s == 7 =
        findNums ((8, x) : acc) xs
      | otherwise =
        error "shouldn't happen"
      where
        s =
          S.size x
        four =
          fromJust $ lookup 4 acc
        seven =
          fromJust $ lookup 7 acc
        matches n =
          S.size (S.intersection x n) == S.size n
