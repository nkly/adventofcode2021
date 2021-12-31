{-
A group of amphipods notice your fancy submarine and flag you down. "With such
an impressive shell," one amphipod says, "surely you can help us with a
question that has stumped our best scientists."

They go on to explain that a group of timid, stubborn amphipods live in a
nearby burrow. Four types of amphipods live there: Amber (A), Bronze (B),
Copper (C), and Desert (D). They live in a burrow that consists of a hallway
and four side rooms. The side rooms are initially full of amphipods, and the
hallway is initially empty.

They give you a diagram of the situation (your puzzle input), including
locations of each amphipod (A, B, C, or D, each of which is occupying an
otherwise open space), walls (#), and open space (.).

For example:

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

The amphipods would like a method to organize every amphipod into side rooms so
that each side room contains one type of amphipod and the types are sorted A-D
going left to right, like this:

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########

Amphipods can move up, down, left, or right so long as they are moving into an
unoccupied open space. Each type of amphipod requires a different amount of
energy to move one step: Amber amphipods require 1 energy per step, Bronze
amphipods require 10 energy, Copper amphipods require 100, and Desert ones
require 1000. The amphipods would like you to find a way to organize the
amphipods that requires the least total energy.

However, because they are timid and stubborn, the amphipods have some extra
rules:

    Amphipods will never stop on the space immediately outside any room. They
    can move into that space so long as they immediately continue moving.
    (Specifically, this refers to the four open spaces in the hallway that are
    directly above an amphipod starting position.)

    Amphipods will never move from the hallway into a room unless that room is
    their destination room and that room contains no amphipods which do not
    also have that room as their own destination. If an amphipod's starting
    room is not its destination room, it can stay in that room until it leaves
    the room. (For example, an Amber amphipod will not move from the hallway
    into the right three rooms, and will only move into the leftmost room if
    that room is empty or if it only contains other Amber amphipods.)

    Once an amphipod stops moving in the hallway, it will stay in that spot
    until it can move into a room. (That is, once any amphipod starts moving,
    any other amphipods currently in the hallway are locked in place and will
    not move again until they can move fully into a room.)

In the above example, the amphipods can be organized using a minimum of 12521
energy. One way to do this is shown below.

Starting configuration:

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

One Bronze amphipod moves into the hallway, taking 4 steps and using 40 energy:

#############
#...B.......#
###B#C#.#D###
  #A#D#C#A#
  #########

The only Copper amphipod not in its side room moves there, taking 4 steps and
using 400 energy:

#############
#...B.......#
###B#.#C#D###
  #A#D#C#A#
  #########

A Desert amphipod moves out of the way, taking 3 steps and using 3000 energy,
and then the Bronze amphipod takes its place, taking 3 steps and using 30
energy:

#############
#.....D.....#
###B#.#C#D###
  #A#B#C#A#
  #########

The leftmost Bronze amphipod moves to its room using 40 energy:

#############
#.....D.....#
###.#B#C#D###
  #A#B#C#A#
  #########

Both amphipods in the rightmost room move into the hallway, using 2003 energy
in total:

#############
#.....D.D.A.#
###.#B#C#.###
  #A#B#C#.#
  #########

Both Desert amphipods move into the rightmost room using 7000 energy:

#############
#.........A.#
###.#B#C#D###
  #A#B#C#D#
  #########

Finally, the last Amber amphipod moves into its room, using 8 energy:

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########

What is the least energy required to organize the amphipods?
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
  [0, 1, 5, 9, 13, 17, 18] -- 4, 8, 12, 16 are directly above the rooms

roomA :: [Int]
roomA =
  [2, 3]

roomB :: [Int]
roomB =
  [6, 7]

roomC :: [Int]
roomC =
  [10, 11]

roomD :: [Int]
roomD =
  [14, 15]

burrowMap :: [(Int, [Int])]
burrowMap =
  [ (0, [1]),
    (1, [0, 4]),
    (2, [3]),
    (3, [2, 4]),
    (4, [1, 3, 5]),
    (5, [4, 8]),
    (6, [7]),
    (7, [6, 8]),
    (8, [5, 7, 9]),
    (9, [8, 12]),
    (10, [11]),
    (11, [10, 12]),
    (12, [9, 11, 13]),
    (13, [12, 16]),
    (14, [15]),
    (15, [14, 16]),
    (16, [13, 15, 17]),
    (17, [16, 18]),
    (18, [17])
  ]

main :: IO ()
main =
  getContents >>= \s -> print $ solve HS.empty (S.singleton (0, sFromString $ flattenInput s))
  where
    flattenInput s =
      let [t, b] = fmap (filter (`elem` "ABCD")) $ take 2 $ drop 2 $ lines s
       in "--" <> L.intercalate "--" (zipWith (:) b $ fmap (: []) t) <> "---"
    endState =
      sFromString "--AA--BB--CC--DD---"
    solve visited !queue
      | state == endState =
        cost
      | otherwise =
        if HS.member state visited
          then solve visited q
          else solve (HS.insert state visited) (foldl' (flip S.insert) q nextMoves)
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
