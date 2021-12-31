{-
As the submarine starts booting up things like the Retro Encabulator, you
realize that maybe you don't need all these submarine features after all.

What is the smallest model number accepted by MONAD?
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async
import Data.Foldable
import Data.List hiding (sort)
import Data.Maybe
import qualified Data.Set as S
import Data.Vector.Algorithms.Merge (sort)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Conc (numCapabilities)

data A
  = AVal !Int
  | AReg !Int
  deriving (Show)

data I
  = IInp !A
  | IAdd !A !A
  | IMul !A !A
  | IDiv !A !A
  | IMod !A !A
  | IEql !A !A
  deriving (Show)

main :: IO ()
main =
  getContents >>= solve . parse >>= print
  where
    solve :: [I] -> IO Int
    solve !insn = do
      states <- V.thaw $ V.singleton ((0, 0, 0, 0), 0)
      solve' states insn
    solve' !states [] =
      V.minimum . V.map snd . V.filter ((== 0) . (\(_, _, _, z) -> z) . fst) <$> V.freeze states
    solve' !states (ins@(IInp _) : insn) = do
      if MV.length states < 1000000 then sort states else psort states
      lastIdx <- MV.foldM' merge 0 $ MV.tail states
      let newSize = (lastIdx + 1) * 9
      putStrLn $ "N. states: " <> show newSize
      states' <- MV.unsafeNew newSize
      forConcurrently_ [1 .. 9] $ \j ->
        MV.iforM_ (MV.take (lastIdx + 1) states) $ \i (s, x) ->
          MV.write states' (9 * i + (j - 1)) $! (,x * 10 + j) $! foldl' (\s' ins' -> step ins' j s') s (ins : toApply)
      solve' states' insn'
      where
        (toApply, insn') =
          break isInpIns insn
        isInpIns (IInp _) =
          True
        isInpIns _ =
          False
        merge i (js, jx) = do
          (is, ix) <- MV.read states i
          if is == js
            then do
              MV.write states i (is, min jx ix)
              pure i
            else do
              MV.write states (i + 1) (js, jx)
              pure (i + 1)
    solve' _ _ =
      error "shouldn't happen"
    step !ins !inp (!w, !x, !y, !z) =
      case ins of
        IInp (AReg r) ->
          modr r (const inp)
        IAdd (AReg r) v ->
          modr r (+ resolve v)
        IMul (AReg r) v ->
          modr r (* resolve v)
        IDiv (AReg r) v ->
          modr r (`div` resolve v)
        IMod (AReg r) v ->
          modr r (`mod` resolve v)
        IEql a@(AReg r) b ->
          modr r (const (if resolve a == resolve b then 1 else 0))
        _ ->
          error $ "invalid instruction: " <> show ins
      where
        modr r fn
          | r == 0 = seq (fn w) (fn w, x, y, z)
          | r == 1 = seq (fn x) (w, fn x, y, z)
          | r == 2 = seq (fn y) (w, x, fn y, z)
          | r == 3 = seq (fn z) (w, x, y, fn z)
          | otherwise = error $ "invalid register: " <> show r
        resolve (AVal v) =
          v
        resolve (AReg r)
          | r == 0 = w
          | r == 1 = x
          | r == 2 = y
          | r == 3 = z
          | otherwise = error $ "invalid register: " <> show r
    parse =
      fmap (toInsn . words) . lines
      where
        varToReg =
          flip elemIndex ["w", "x", "y", "z"]
        regOrVal v =
          maybe (AVal $ read v) AReg $ varToReg v
        reg v =
          AReg $ fromMaybe (error $ "invalid variable: " <> v) $ varToReg v
        toInsn ["inp", v] =
          IInp (reg v)
        toInsn ["add", a1, a2] =
          IAdd (reg a1) (regOrVal a2)
        toInsn ["mul", a1, a2] =
          IMul (reg a1) (regOrVal a2)
        toInsn ["div", a1, a2] =
          IDiv (reg a1) (regOrVal a2)
        toInsn ["mod", a1, a2] =
          IMod (reg a1) (regOrVal a2)
        toInsn ["eql", a1, a2] =
          IEql (reg a1) (regOrVal a2)
        toInsn x =
          error $ "invalid instruction: " <> unwords x

psort :: forall a. (Ord a, MV.Unbox a) => MV.IOVector a -> IO ()
psort v = do
  tmpV <- MV.new (MV.length v)
  MV.copy tmpV v
  forConcurrently_ slices $ \(start, end) -> sort $ MV.slice start (end - start) tmpV
  queue <- foldlM (\q (s, _) -> MV.read tmpV s >>= \x -> pure $ S.insert (x, s) q) S.empty slices
  merge 0 queue tmpV
  where
    slices =
      fmap (\x -> if x == numCapabilities then (sliceSize * (x - 1), MV.length v) else (sliceSize * (x - 1), sliceSize * x)) [1 .. numCapabilities]
    sliceSize =
      MV.length v `div` numCapabilities
    merge :: Int -> S.Set (a, Int) -> MV.IOVector a -> IO ()
    merge idx queue tmpV
      | S.null queue =
        pure ()
      | otherwise = do
        MV.write v idx x
        queue' <-
          if s + 1 >= MV.length v
            then pure q'
            else MV.read tmpV (s + 1) >>= \x' -> if x' >= x then pure (S.insert (x', s + 1) q') else pure q'
        merge (idx + 1) queue' tmpV
      where
        ((x, s), q') = S.deleteFindMin queue
