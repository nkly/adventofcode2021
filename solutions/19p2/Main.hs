{-
Sometimes, it's a good idea to appreciate just how big the ocean is. Using the
Manhattan distance, how far apart do the scanners get?

In the above example, scanners 2 (1105,-1205,1229) and 3 (-92,-2380,-20) are
the largest Manhattan distance apart. In total, they are 1197 + 1175 + 1249 =
3621 units apart.

What is the largest Manhattan distance between any two scanners?
-}
module Main where

import Data.Bifunctor
import Data.List
import Data.Maybe
import qualified Data.Set as S

main :: IO ()
main =
  getContents >>= print . solve . parse
  where
    solve scanners =
      maximum $ [mdist sp1 sp2 | sp1 <- sps, sp2 <- sps]
      where
        sps =
          ($ (0, 0, 0)) <$> snd (solve' [head scanners] [id] $ tail scanners)
        mdist (x1, y1, z1) (x2, y2, z2) =
          abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
    solve' norm ts [] =
      (norm, ts)
    solve' norm ts (s : ss) =
      maybe (solve' norm ts (ss <> [s])) (\(n, t) -> solve' (n : norm) (t : ts) ss) (find (match s) norm >>= normalize s)
    normalize s pivot =
      (\t -> (t <$> s, t)) <$> listToMaybe (mapMaybe unifyWith rotations)
      where
        unifyWith r =
          fmap (\x -> addp x . rotate r) $ listToMaybe $ fmap head $ sortOn (negate . length) $ filter ((>= 12) . length) $ group $ sort $ mapMaybe (findTransformation r) matchingPairs
        findTransformation r ((pp1, pp2), (sp1, sp2)) =
          find (== subp pp2 (rotate r sp2)) $ Just (subp pp1 (rotate r sp1))
        matchingPairs =
          filter (\(pp, sp) -> dist pp == dist sp) [(pp, sp) | pp <- pairs pivot, sp <- pairs s]
    match s1 s2 =
      length (distances s1 `S.intersection` distances s2) >= comb 12 2
    distances pts =
      S.fromList $ dist <$> pairs pts
    pairs pts =
      filter (uncurry (/=)) [(a, b) | a <- pts, b <- pts]
    addp (x1, y1, z1) (x2, y2, z2) =
      (x1 + x2, y1 + y2, z1 + z2)
    subp (x1, y1, z1) (x2, y2, z2) =
      (x1 - x2, y1 - y2, z1 - z2)
    dist ((x1, y1, z1), (x2, y2, z2)) =
      (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)
    rotate (xi, yi, zi, xr, yr, zr) (x, y, z) =
      (([x, y, z] !! xi) * xr, ([x, y, z] !! yi) * yr, ([x, y, z] !! zi) * zr)
    rotations =
      nub [(xi, yi, zi, xr, yr, zr) | [xi, yi, zi] <- permutations [0, 1, 2], xr <- [-1, 1], yr <- [-1, 1], zr <- [-1, 1]]
    comb n k =
      product [(n - k + 1) .. n] `div` product [1 .. k]
    parse =
      fmap (fmap parsePoint . tail) . splitOn "" . lines
      where
        parsePoint inp =
          let [x, y, z] = splitOn ',' inp
           in (read x, read y, read z) :: (Int, Int, Int)
    splitOn n =
      uncurry (:) . second (maybe [] (splitOn n . snd) . uncons) . break (== n)
