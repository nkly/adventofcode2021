{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Matrix
  ( Matrix (nrows, ncols),
    nelems,
    zero,
    create,
    fromLists,
    toList,
    imap,
    ifor,
    get,
    unsafeGet,
    submatrix,
    elementwise,
    transpose,
    swapRows,
    swapCols,
    swap,
  )
where

import Data.Bifunctor (bimap)
import qualified Data.Tuple as Tuple
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data Matrix a = M
  { nrows :: !Int,
    ncols :: !Int,
    vect :: !(V.Vector a)
  }
  deriving (Eq, Functor, Traversable, Foldable)

instance Show a => Show (Matrix a) where
  show (M _ nc v) =
    init $ concat $ V.toList $ V.imap (\i s -> if (i + 1) `mod` nc == 0 then fill s <> "\n" else fill s <> " ") strs
    where
      strs = fmap show v
      width = maximum $ fmap length strs
      fill s = replicate (width - length s) ' ' <> s

nelems :: Matrix a -> Int
nelems (M nr nc _) =
  nr * nc

zero :: Int -> Int -> Matrix Int
zero rows cols =
  M rows cols $ V.replicate (rows * cols) 0

create :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
create rows cols gen =
  M rows cols $ V.generate (rows * cols) (gen . toRowCol (rows, cols))

fromLists :: [[a]] -> Matrix a
fromLists [] =
  error "fromLists: empty list"
fromLists l@(x : xs)
  | not (all (== length x) $ length <$> xs) =
    error "fromLists: rows have different lengths"
  | otherwise =
    M (length l) (length x) $ V.fromList $ concat l

toList :: Matrix a -> [a]
toList =
  V.toList . vect

imap :: ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
imap fn (M nr nc v) =
  M nr nc $ V.imap (fn . toRowCol (nr, nc)) v

ifor :: Matrix a -> ((Int, Int) -> a -> b) -> Matrix b
ifor = flip imap

get :: (Int, Int) -> Matrix a -> Maybe a
get pos@(r, c) (M nr nc v)
  | 0 <= r && r < nr && 0 <= c && c < nc =
    (V.!?) v idx
  | otherwise =
    Nothing
  where
    idx = toIdx nc pos

unsafeGet :: (Int, Int) -> Matrix a -> a
{-# INLINE unsafeGet #-}
unsafeGet (r, c) (M _ nc v) =
  V.unsafeIndex v (r * nc + c)

submatrix :: (Int, Int) -> (Int, Int) -> Matrix a -> Matrix a
submatrix (sr, sc) (er, ec) m@(M nr nc _)
  | sr < 0 || sc < 0 || sr >= nr || sc >= nc =
    error $ "submatrix: invalid start position: " <> show (sr, sc)
  | er < sr || ec < sc || er >= nr || ec >= nc =
    error $ "submatrix: invalid end position: " <> show (er, ec)
  | otherwise =
    create (er - sr + 1) (ec - sc + 1) (flip unsafeGet m . bimap (+ sr) (+ sc))

elementwise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
elementwise fn (M nr1 nc1 v1) (M nr2 nc2 v2)
  | nr1 /= nr2 || nc1 /= nc2 =
    error "elementwise: matrices have different size"
  | otherwise =
    M nr1 nc1 $ V.zipWith fn v1 v2

transpose :: Matrix a -> Matrix a
transpose m@(M nr nc _) =
  create nc nr (flip unsafeGet m . Tuple.swap)

swap :: (Int, Int) -> (Int, Int) -> Matrix a -> Matrix a
swap p1@(r1, c1) p2@(r2, c2) m@(M nr nc v)
  | r1 < 0 || r1 >= nr || c1 < 0 || c1 >= nc =
    error "swap: invalid first position"
  | r2 < 0 || r2 >= nr || c2 < 0 || c2 >= nc =
    error "swap: invalid second position"
  | r1 == r2 && c1 == c2 =
    m
  | otherwise =
    M nr nc $ V.modify (\mv -> MV.swap mv (toIdx nc p1) (toIdx nc p2)) v

swapRows :: Int -> Int -> Matrix a -> Matrix a
swapRows r1 r2 (M nr nc v)
  | r1 < 0 || r1 >= nr =
    error "swapRows: invalid first row"
  | r2 < 0 || r2 >= nr =
    error "swapRows: invalid second row"
  | otherwise =
    M nr nc $ V.modify (rswap nc) v
  where
    rswap 0 _ =
      pure ()
    rswap c mv = do
      MV.swap mv (r1 * nc + c - 1) (r2 * nc + c - 1)
      rswap (c - 1) mv

swapCols :: Int -> Int -> Matrix a -> Matrix a
swapCols c1 c2 (M nr nc v)
  | c1 < 0 || c1 >= nc =
    error "swapCols: invalid first column"
  | c2 < 0 || c2 >= nc =
    error "swapCols: invalid second column"
  | otherwise =
    M nr nc $ V.modify (cswap nr) v
  where
    cswap 0 _ =
      pure ()
    cswap r mv = do
      MV.swap mv ((r - 1) * nc + c1) ((r - 1) * nc + c2)
      cswap (r - 1) mv

toRowCol :: (Int, Int) -> Int -> (Int, Int)
{-# INLINE toRowCol #-}
toRowCol (_, nc) i =
  i `divMod` nc

toIdx :: Int -> (Int, Int) -> Int
{-# INLINE toIdx #-}
toIdx nc (r, c) =
  r * nc + c
