{-
You still can't quite make out the details in the image. Maybe you just didn't
enhance it enough.

If you enhance the starting input image in the above example a total of 50
times, 3351 pixels are lit in the final output image.

Start again with the original input image and apply the image enhancement
algorithm 50 times. How many pixels are lit in the resulting image?
-}
module Main where

import Data.Bitraversable
import qualified Data.Matrix as M
import Data.Maybe

main :: IO ()
main =
  getContents >>= print . uncurry (solve 0 0) . parse
  where
    solve step inf alg m
      | step == 50 =
        sum m
      | otherwise =
        solve (step + 1) inf' alg $ M.ifor m' (const . getValue)
      where
        inf' =
          if inf == 0 then head alg else last alg
        m' =
          M.create (M.nrows m + 2) (M.ncols m + 2) $ \case
            (r, c) | r >= 1 && c >= 1 && r < M.nrows m + 1 && c < M.ncols m + 1 -> M.unsafeGet (r - 1, c - 1) m
            _ -> inf
        getValue (r, c) =
          alg !! idx
          where
            idx =
              btoi $ fromMaybe inf . flip M.get m' <$> [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1), (r, c - 1), (r, c), (r, c + 1), (r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]
            btoi =
              foldl (\acc x -> acc * 2 + x) 0
    parse =
      bisequence (fmap toBit . head, M.fromLists . fmap (fmap toBit) . drop 2) . lines
      where
        toBit x
          | x == '#' = 1
          | x == '.' = 0
          | otherwise = error "invalid input"
