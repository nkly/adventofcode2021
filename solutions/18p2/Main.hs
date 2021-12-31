{-
You notice a second question on the back of the homework assignment:

What is the largest magnitude you can get from adding only two of the snailfish
numbers?

Note that snailfish addition is not commutative - that is, x + y and y + x can
produce different results.

Again considering the last example homework assignment above:

[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]

The largest magnitude of the sum of any two snailfish numbers in this list is
3993. This is the magnitude of [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]] +
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]], which reduces to
[[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]].

What is the largest magnitude of any sum of two different snailfish numbers
from the homework assignment?
-}
module Main where

import Data.Char
import Data.Maybe

data Pair
  = PLeaf !Int
  | PNode !Pair !Pair
  deriving (Eq)

data ExplodeAction
  = ESearch !Pair
  | ESkip !Pair
  | EAddBoth !Int !Int
  | EAddLeft !Int !Pair
  | EAddRight !Pair !Int
  deriving (Eq)

main :: IO ()
main =
  getContents >>= print . solve . fmap (fst . parsePair) . lines
  where
    solve ps =
      maximum $ fmap (mag . reducePair . uncurry PNode) [(a, b) | a <- ps, b <- ps]
    mag (PLeaf x) =
      x
    mag (PNode l r) =
      3 * mag l + 2 * mag r
    reducePair p
      | p' == p = p'
      | otherwise = reducePair p'
      where
        p' = split $ explode p

    explode pair
      | pair' == pair = pair'
      | otherwise = explode pair'
      where
        pair' = case explode' 0 pair of
          ESearch p' -> p'
          ESkip p' -> p'
          EAddLeft _ p' -> p'
          EAddRight p' _ -> p'
          _ -> error "shouldn't happen"
        explode' depth p@(PNode (PLeaf l) (PLeaf r))
          | depth >= 4 =
            EAddBoth l r
          | otherwise =
            ESearch p
        explode' depth (PNode lp rp) =
          case explode' (depth + 1) lp of
            ESearch lp' ->
              case explode' (depth + 1) rp of
                ESearch rp' ->
                  ESearch $ PNode lp' rp'
                ESkip rp' ->
                  ESkip $ PNode lp' rp'
                EAddBoth l r ->
                  EAddRight (PNode (addRight lp' l) (PLeaf 0)) r
                EAddRight rp' r ->
                  EAddRight (PNode lp' rp') r
                EAddLeft l rp' ->
                  ESkip (PNode (addRight lp' l) rp')
            ESkip lp' ->
              ESkip (PNode lp' rp)
            EAddBoth l r ->
              EAddLeft l (PNode (PLeaf 0) (addLeft rp r))
            EAddLeft l lp' ->
              EAddLeft l (PNode lp' rp)
            EAddRight lp' r ->
              ESkip (PNode lp' (addLeft rp r))
        explode' _ p =
          ESearch p
        addRight (PNode lp rp) v =
          PNode lp $ addRight rp v
        addRight (PLeaf x) v =
          PLeaf $ x + v
        addLeft (PNode lp rp) v =
          PNode (addLeft lp v) rp
        addLeft (PLeaf x) v =
          PLeaf $ x + v

    split p =
      fromMaybe p $ split' p
      where
        split' (PLeaf x)
          | x > 9 =
            Just $ PNode (PLeaf $ floor $ fromIntegral x / 2) (PLeaf $ ceiling $ fromIntegral x / 2)
          | otherwise =
            Nothing
        split' (PNode lp rp) =
          case (split' lp, split' rp) of
            (Nothing, Nothing) -> Nothing
            (Just lp', _) -> Just $ PNode lp' rp
            (Nothing, Just rp') -> Just $ PNode lp rp'

    parsePair ('[' : s) =
      (PNode left right, tail rs)
      where
        (left, ls) = parsePair s
        (right, rs) = parsePair $ tail ls
    parsePair (x : s) =
      (PLeaf $ digitToInt x, s)
    parsePair _ =
      error "shouldn't happen"
