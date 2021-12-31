{-
Now that you're warmed up, it's time to play the real game.

A second compartment opens, this time labeled Dirac dice. Out of it falls a
single three-sided die.

As you experiment with the die, you feel a little strange. An informational
brochure in the compartment explains that this is a quantum die: when you roll
it, the universe splits into multiple copies, one copy for each possible
outcome of the die. In this case, rolling the die always splits the universe
into three copies: one where the outcome of the roll was 1, one where it was 2,
and one where it was 3.

The game is played the same as before, although to prevent things from getting
too far out of hand, the game now ends when either player's score reaches at
least 21.

Using the same starting positions as in the example above, player 1 wins in
444356092776315 universes, while player 2 merely wins in 341960390180808
universes.

Using your given starting positions, determine every possible outcome. Find the
player that wins in more universes; in how many universes does that player win?
-}
module Main where

import Control.Monad.ST
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import qualified Data.Map.Strict as M
import Data.STRef

main :: IO ()
main =
  getContents >>= print . solve . parse
  where
    solve pos = runST $ do
      cacheRef <- newSTRef M.empty
      bimaximum <$> play cacheRef 0 (0, 0) pos
    play cacheRef turn (s1, s2) (p1, p2)
      | s1 >= 21 =
        pure (1, 0)
      | s2 >= 21 =
        pure (0, 1)
      | otherwise =
        lookup' (firstPlayerTurn, s1, s2, p1, p2) >>= \case
          Just x ->
            pure x
          Nothing -> do
            val <- if firstPlayerTurn then sumUnzip <$> mapM playFirst diceRollsCount else sumUnzip <$> mapM playSecond diceRollsCount
            modifySTRef' cacheRef (M.insert (firstPlayerTurn, s1, s2, p1, p2) val)
            pure val
      where
        playFirst (r, c) =
          bimap (* c) (* c) <$> play cacheRef (turn + 1) (s1 + clampPos (p1 + r), s2) (clampPos (p1 + r), p2)
        playSecond (r, c) =
          bimap (* c) (* c) <$> play cacheRef (turn + 1) (s1, s2 + clampPos (p2 + r)) (p1, clampPos (p2 + r))
        sumUnzip =
          bimap sum sum . unzip
        firstPlayerTurn =
          even turn
        clampPos p =
          if p `mod` 10 == 0 then 10 else p `mod` 10
        lookup' k =
          M.lookup k <$> readSTRef cacheRef
        diceRollsCount =
          [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]
    parse =
      bisequence
        ( (read :: String -> Int) . last . words . (!! 0),
          (read :: String -> Int) . last . words . (!! 1)
        )
        . lines
