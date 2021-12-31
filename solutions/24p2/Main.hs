{-
As the submarine starts booting up things like the Retro Encabulator, you
realize that maybe you don't need all these submarine features after all.

What is the smallest model number accepted by MONAD?
-}
module Main where

import Control.Concurrent.Async
import Data.Foldable
import Data.List hiding (sort)
import Data.List.NonEmpty hiding (sort)
import Data.Maybe
import Data.Semigroup
import Data.Vector.Algorithms.Merge (sort)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

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
      sort states
      lastIdx <- MV.foldM' merge 0 $ MV.tail states
      let newSize = (lastIdx + 1) * 9
      putStrLn $ "N. states: " <> show newSize
      states' <- MV.unsafeNew newSize
      MV.imapM_ (\i (s, x) -> forM_ [1 .. 9] (\j -> MV.write states' (9 * i + (j - 1)) $! (,x * 10 + j) $! step ins j s)) $ MV.take (lastIdx + 1) states
      solve' states' insn
      where
        merge i (js, jx) = do
          (is, ix) <- MV.read states i
          if is == js
            then do
              MV.write states i (is, min jx ix)
              pure i
            else do
              MV.write states (i + 1) (js, jx)
              pure (i + 1)
    solve' !states (ins : insn) = do
      if MV.length states < 10000
        then MV.imapM_ (stepUpd 0) states
        else do
          let sliceSize = MV.length states `div` 8
          runConcurrently $
            sconcat $
              Concurrently (MV.imapM_ (stepUpd (7 * sliceSize)) (MV.drop (7 * sliceSize) states))
                :| fmap (\k -> Concurrently (MV.imapM_ (stepUpd (k * sliceSize)) (MV.take sliceSize $ MV.drop (k * sliceSize) states))) [0 .. 6]
      solve' states insn
      where
        stepUpd offset i (s, x) =
          MV.write states (offset + i) $! (,x) $! step ins 0 s
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
