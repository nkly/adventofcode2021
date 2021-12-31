{-
Magic smoke starts leaking from the submarine's arithmetic logic unit (ALU).
Without the ability to perform basic arithmetic and logic functions, the
submarine can't produce cool patterns with its Christmas lights!

It also can't navigate. Or run the oxygen system.

Don't worry, though - you probably have enough oxygen left to give you enough
time to build a new ALU.

The ALU is a four-dimensional processing unit: it has integer variables w, x,
y, and z. These variables all start with the value 0. The ALU also supports six
instructions:

    inp a - Read an input value and write it to variable a.

    add a b - Add the value of a to the value of b, then store the result in
    variable a.

    mul a b - Multiply the value of a by the value of b, then store the result
    in variable a.

    div a b - Divide the value of a by the value of b, truncate the result to
    an integer, then store the result in variable a. (Here, "truncate" means to
    round the value toward zero.)

    mod a b - Divide the value of a by the value of b, then store the remainder
    in variable a. (This is also called the modulo operation.)

    eql a b - If the value of a and b are equal, then store the value 1 in
    variable a. Otherwise, store the value 0 in variable a.

In all of these instructions, a and b are placeholders; a will always be the
variable where the result of the operation is stored (one of w, x, y, or z),
while b can be either a variable or a number. Numbers can be positive or
negative, but will always be integers.

The ALU has no jump instructions; in an ALU program, every instruction is run
exactly once in order from top to bottom. The program halts after the last
instruction has finished executing.

(Program authors should be especially cautious; attempting to execute div with
b=0 or attempting to execute mod with a<0 or b<=0 will cause the program to
crash and might even damage the ALU. These operations are never intended in any
serious ALU program.)

For example, here is an ALU program which takes an input number, negates it,
and stores it in x:

inp x
mul x -1

Here is an ALU program which takes two input numbers, then sets z to 1 if the
second input number is three times larger than the first input number, or sets
z to 0 otherwise:

inp z
inp x
mul z 3
eql z x

Here is an ALU program which takes a non-negative integer as input, converts it
into binary, and stores the lowest (1's) bit in z, the second-lowest (2's) bit
in y, the third-lowest (4's) bit in x, and the fourth-lowest (8's) bit in w:

inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2

Once you have built a replacement ALU, you can install it in the submarine,
which will immediately resume what it was doing when the ALU failed: validating
the submarine's model number. To do this, the ALU will run the MOdel Number
Automatic Detector program (MONAD, your puzzle input).

Submarine model numbers are always fourteen-digit numbers consisting only of
digits 1 through 9. The digit 0 cannot appear in a model number.

When MONAD checks a hypothetical fourteen-digit model number, it uses fourteen
separate inp instructions, each expecting a single digit of the model number in
order of most to least significant. (So, to check the model number
13579246899999, you would give 1 to the first inp instruction, 3 to the second
inp instruction, 5 to the third inp instruction, and so on.) This means that
when operating MONAD, each input instruction should only ever be given an
integer value of at least 1 and at most 9.

Then, after MONAD has finished running all of its instructions, it will
indicate that the model number was valid by leaving a 0 in variable z. However,
if the model number was invalid, it will leave some other non-zero value in z.

MONAD imposes additional, mysterious restrictions on model numbers, and legend
says the last copy of the MONAD documentation was eaten by a tanuki. You'll
need to figure out what MONAD does some other way.

To enable as many submarine features as possible, find the largest valid
fourteen-digit model number that contains no 0 digits. What is the largest
model number accepted by MONAD?
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
      V.maximum . V.map snd . V.filter ((== 0) . (\(_, _, _, z) -> z) . fst) <$> V.freeze states
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
              MV.write states i (is, max jx ix)
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
