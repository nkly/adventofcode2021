{-
As you prepare to give the amphipods your solution, you notice that the diagram
they handed you was actually folded up. As you unfold it, you discover an extra
part of the diagram.

Between the first and second lines of text that contain amphipod starting
positions, insert the following lines:

  #D#C#B#A#
  #D#B#A#C#

So, the above example now becomes:

#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########

The amphipods still want to be organized into rooms similar to before:

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########

In this updated example, the least energy required to organize these amphipods
is 44169:

#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########

#############
#..........D#
###B#C#B#.###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########

#############
#A.........D#
###B#C#B#.###
  #D#C#B#.#
  #D#B#A#C#
  #A#D#C#A#
  #########

#############
#A........BD#
###B#C#.#.###
  #D#C#B#.#
  #D#B#A#C#
  #A#D#C#A#
  #########

#############
#A......B.BD#
###B#C#.#.###
  #D#C#.#.#
  #D#B#A#C#
  #A#D#C#A#
  #########

#############
#AA.....B.BD#
###B#C#.#.###
  #D#C#.#.#
  #D#B#.#C#
  #A#D#C#A#
  #########

#############
#AA.....B.BD#
###B#.#.#.###
  #D#C#.#.#
  #D#B#C#C#
  #A#D#C#A#
  #########

#############
#AA.....B.BD#
###B#.#.#.###
  #D#.#C#.#
  #D#B#C#C#
  #A#D#C#A#
  #########

#############
#AA...B.B.BD#
###B#.#.#.###
  #D#.#C#.#
  #D#.#C#C#
  #A#D#C#A#
  #########

#############
#AA.D.B.B.BD#
###B#.#.#.###
  #D#.#C#.#
  #D#.#C#C#
  #A#.#C#A#
  #########

#############
#AA.D...B.BD#
###B#.#.#.###
  #D#.#C#.#
  #D#.#C#C#
  #A#B#C#A#
  #########

#############
#AA.D.....BD#
###B#.#.#.###
  #D#.#C#.#
  #D#B#C#C#
  #A#B#C#A#
  #########

#############
#AA.D......D#
###B#.#.#.###
  #D#B#C#.#
  #D#B#C#C#
  #A#B#C#A#
  #########

#############
#AA.D......D#
###B#.#C#.###
  #D#B#C#.#
  #D#B#C#.#
  #A#B#C#A#
  #########

#############
#AA.D.....AD#
###B#.#C#.###
  #D#B#C#.#
  #D#B#C#.#
  #A#B#C#.#
  #########

#############
#AA.......AD#
###B#.#C#.###
  #D#B#C#.#
  #D#B#C#.#
  #A#B#C#D#
  #########

#############
#AA.......AD#
###.#B#C#.###
  #D#B#C#.#
  #D#B#C#.#
  #A#B#C#D#
  #########

#############
#AA.......AD#
###.#B#C#.###
  #.#B#C#.#
  #D#B#C#D#
  #A#B#C#D#
  #########

#############
#AA.D.....AD#
###.#B#C#.###
  #.#B#C#.#
  #.#B#C#D#
  #A#B#C#D#
  #########

#############
#A..D.....AD#
###.#B#C#.###
  #.#B#C#.#
  #A#B#C#D#
  #A#B#C#D#
  #########

#############
#...D.....AD#
###.#B#C#.###
  #A#B#C#.#
  #A#B#C#D#
  #A#B#C#D#
  #########

#############
#.........AD#
###.#B#C#.###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########

#############
#..........D#
###A#B#C#.###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########

Using the initial configuration from the full diagram, what is the least energy
required to organize the amphipods?
-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Parallel.Strategies
import Data.Bits
import Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as S
import Data.Word
import GHC.Generics (Generic)

data S = S !Word64 !Word64 deriving (Eq, Ord, Generic)

instance Hashable S

sIsBlocked :: Int -> S -> Bool
sIsBlocked i (S hi lo)
  | i < 0 || i > 40 =
    error $ "sIsBlocked: invalid index: " <> show i
  | i <= 20 =
    testBit lo (i * 3 + 2)
  | otherwise =
    testBit hi ((i - 21) * 3 + 2)

sGet :: Int -> S -> Word64
sGet i (S hi lo)
  | i < 0 || i > 40 =
    error $ "sGet: invalid index: " <> show i
  | i <= 20 =
    shiftR lo (i * 3) .&. 7
  | otherwise =
    shiftR hi ((i - 21) * 3) .&. 7

sSet :: Int -> Word64 -> S -> S
sSet i x (S hi lo)
  | i < 0 || i > 40 =
    error $ "sSet: invalid index: " <> show i
  | i <= 20 =
    S hi ((lo .&. rotateL clear (i * 3)) .|. shiftL x' (i * 3))
  | otherwise =
    S ((hi .&. rotateL clear ((i - 21) * 3)) .|. shiftL x' ((i - 21) * 3)) lo
  where
    x' = x .&. 7
    clear = complement 7

sFromString :: String -> S
sFromString =
  encode' (S 0 0) 0
  where
    encode' !s _ [] =
      s
    encode' !s i (x : xs)
      | x == '-' = encode' (sSet i 0 s) (i + 1) xs
      | x == 'A' = encode' (sSet i 4 s) (i + 1) xs
      | x == 'B' = encode' (sSet i 5 s) (i + 1) xs
      | x == 'C' = encode' (sSet i 6 s) (i + 1) xs
      | x == 'D' = encode' (sSet i 7 s) (i + 1) xs
      | otherwise = error "sFromString: invalid input"

allPaths :: M.HashMap (Int, Int) [Int]
allPaths =
  M.fromList $ catMaybes [((f, t),) <$> bfs' [[f]] [f] t | f <- [0 .. maximum hallway], t <- [0 .. maximum hallway]]
  where
    bfs' [] _ _ =
      Nothing
    bfs' (p : ps) visited t
      | head p == t =
        Just $ reverse p
      | otherwise =
        bfs' (ps <> fmap (: p) toVisit) (visited <> toVisit) t
      where
        toVisit =
          maybe [] (filter (not . (`elem` visited))) $ lookup (head p) burrowMap

hallway :: [Int]
hallway =
  [0, 1, 7, 13, 19, 25, 26] -- 6, 12, 18, 24 are directly above the rooms

roomA :: [Int]
roomA =
  [2, 3, 4, 5]

roomB :: [Int]
roomB =
  [8, 9, 10, 11]

roomC :: [Int]
roomC =
  [14, 15, 16, 17]

roomD :: [Int]
roomD =
  [20, 21, 22, 23]

burrowMap :: [(Int, [Int])]
burrowMap =
  [ (0, [1]),
    (1, [0, 6]),
    (2, [3]),
    (3, [2, 4]),
    (4, [3, 5]),
    (5, [4, 6]),
    (6, [1, 5, 7]),
    (7, [6, 12]),
    (8, [9]),
    (9, [8, 10]),
    (10, [9, 11]),
    (11, [10, 12]),
    (12, [7, 11, 13]),
    (13, [12, 18]),
    (14, [15]),
    (15, [14, 16]),
    (16, [15, 17]),
    (17, [16, 18]),
    (18, [13, 17, 19]),
    (19, [18, 24]),
    (20, [21]),
    (21, [20, 22]),
    (22, [21, 23]),
    (23, [22, 24]),
    (24, [19, 23, 25]),
    (25, [24, 26]),
    (26, [25])
  ]

main :: IO ()
main =
  getContents >>= \s -> solve 0 HS.empty (S.singleton (0, sFromString $ flattenInput s)) >>= print
  where
    flattenInput s =
      let [t, b] = fmap (filter (`elem` "ABCD")) $ take 2 $ drop 2 $ lines s
          [bt, bb] = ["DCBA", "DBAC"]
       in "--" <> L.intercalate "--" (L.zipWith4 (\w x y z -> [w, x, y, z]) b bb bt t) <> "---"
    endState =
      sFromString "--AAAA--BBBB--CCCC--DDDD---"
    solve !count !visited !queue
      | state == endState =
        pure cost
      | otherwise = do
        if count `mod` 10000 == 0 then print (count, cost) else pure ()
        if HS.member state visited
          then solve (count + 1) visited q
          else solve (count + 1) (HS.insert state visited) (foldl' (flip S.insert) q nextMoves)
      where
        ((cost, state), q) =
          S.deleteFindMin queue
        nextMoves =
          catMaybes $ parMap rpar processMove [(f, t) | f <- occupied, t <- empty]
          where
            occupied =
              filter isBlocked (hallway <> allRooms)
            empty =
              filter (not . isBlocked) (hallway <> allRooms)
            processMove (f, t) =
              moveCost f t >>= \c ->
                let s' = sSet f (sGet t state) $ sSet t (sGet f state) state
                 in if HS.member s' visited then Nothing else Just (cost + c, s')
        isBlocked p =
          sIsBlocked p state
        allRooms =
          concat [roomA, roomB, roomC, roomD]
        moveCost f t
          | (f `elem` allRooms && (t `elem` hallway || (t `elem` dstRoom && dstRoomHasOnlyValidElems)))
              || (f `elem` hallway && t `elem` dstRoom && dstRoomHasOnlyValidElems) =
            (* multiplier) . flip (-) 1 . length <$> path
          | otherwise =
            Nothing
          where
            multiplier
              | fValue == 4 = 1
              | fValue == 5 = 10
              | fValue == 6 = 100
              | fValue == 7 = 1000
              | otherwise = undefined
            dstRoom
              | fValue == 4 = roomA
              | fValue == 5 = roomB
              | fValue == 6 = roomC
              | fValue == 7 = roomD
              | otherwise = undefined
            fValue =
              sGet f state
            dstRoomHasOnlyValidElems =
              all (\p -> sGet p state == fValue || not (isBlocked p)) dstRoom
            path =
              M.lookup (f, t) allPaths >>= \p -> if any isBlocked (tail p) then Nothing else Just p
