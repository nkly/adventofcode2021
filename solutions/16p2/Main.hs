{-
Now that you have the structure of your transmission decoded, you can calculate
the value of the expression it represents.

Literal values (type ID 4) represent a single number as described above. The
remaining type IDs are more interesting:

    Packets with type ID 0 are sum packets - their value is the sum of the
    values of their sub-packets. If they only have a single sub-packet, their
    value is the value of the sub-packet.

    Packets with type ID 1 are product packets - their value is the result of
    multiplying together the values of their sub-packets. If they only have a
    single sub-packet, their value is the value of the sub-packet.

    Packets with type ID 2 are minimum packets - their value is the minimum of
    the values of their sub-packets.

    Packets with type ID 3 are maximum packets - their value is the maximum of
    the values of their sub-packets.

    Packets with type ID 5 are greater than packets - their value is 1 if the
    value of the first sub-packet is greater than the value of the second
    sub-packet; otherwise, their value is 0. These packets always have exactly
    two sub-packets.

    Packets with type ID 6 are less than packets - their value is 1 if the
    value of the first sub-packet is less than the value of the second
    sub-packet; otherwise, their value is 0. These packets always have exactly
    two sub-packets.

    Packets with type ID 7 are equal to packets - their value is 1 if the value
    of the first sub-packet is equal to the value of the second sub-packet;
    otherwise, their value is 0. These packets always have exactly two
    sub-packets.

Using these rules, you can now work out the value of the outermost packet in
your BITS transmission.

For example:

    C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
    04005AC33890 finds the product of 6 and 9, resulting in the value 54.
    880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
    CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
    D8005AC2A8F0 produces 1, because 5 is less than 15.
    F600BC2D8F produces 0, because 5 is not greater than 15.
    9C005AC2F8F0 produces 0, because 5 is not equal to 15.
    9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.

What do you get if you evaluate the expression represented by your
hexadecimal-encoded BITS transmission?
-}
module Main where

import Data.List
import Data.Maybe

data P
  = PLit Int Int
  | POp Int Int [P]
  deriving (Show)

main :: IO ()
main =
  getContents >>= print . eval . fst . readPacket . parse
  where
    parse =
      concatMap htob
    eval (PLit _ v) =
      v
    eval (POp _ t ps)
      | t == 0 =
        sum $ fmap eval ps
      | t == 1 =
        product $ fmap eval ps
      | t == 2 =
        minimum $ fmap eval ps
      | t == 3 =
        maximum $ fmap eval ps
      | t == 5 =
        if eval (head ps) > eval (last ps) then 1 else 0
      | t == 6 =
        if eval (head ps) < eval (last ps) then 1 else 0
      | t == 7 =
        if eval (head ps) == eval (last ps) then 1 else 0
      | otherwise =
        error $ "invalid operation '" <> show t <> "'"
    readPacket input
      | t == 4 =
        let (value, rest) = readVarInt $ drop 6 input
         in (PLit v value, rest)
      | lt == 0 =
        let subp = unfoldr (\x -> if null x then Nothing else Just (readPacket x)) (take subl $ drop 22 input)
         in (POp v t subp, drop (22 + subl) input)
      | otherwise =
        let (subp, rest) = readNPackets subn [] $ drop 18 input
         in (POp v t subp, rest)
      where
        v = btoi $ take 3 input
        t = btoi $ take 3 $ drop 3 input
        lt = input !! 6
        subl = btoi $ take 15 $ drop 7 input
        subn = btoi $ take 11 $ drop 7 input
        readNPackets 0 acc rest =
          (reverse acc, rest)
        readNPackets n acc rest =
          let (p, r) = readPacket rest
           in readNPackets (n - 1) (p : acc) r
    readVarInt input =
      (btoi x, rest)
      where
        x =
          readFrom 0
        rest =
          drop (length x + (length x `div` 4)) input
        readFrom s
          | (input !! s) == 0 =
            take 4 $ drop (s + 1) input
          | otherwise =
            take 4 (drop (s + 1) input) <> readFrom (s + 5)
    btoi =
      foldl (\acc c -> acc * 2 + c) 0
    htob =
      fromJust . flip lookup table
      where
        table =
          [ ('0', [0, 0, 0, 0]),
            ('1', [0, 0, 0, 1]),
            ('2', [0, 0, 1, 0]),
            ('3', [0, 0, 1, 1]),
            ('4', [0, 1, 0, 0]),
            ('5', [0, 1, 0, 1]),
            ('6', [0, 1, 1, 0]),
            ('7', [0, 1, 1, 1]),
            ('8', [1, 0, 0, 0]),
            ('9', [1, 0, 0, 1]),
            ('A', [1, 0, 1, 0]),
            ('B', [1, 0, 1, 1]),
            ('C', [1, 1, 0, 0]),
            ('D', [1, 1, 0, 1]),
            ('E', [1, 1, 1, 0]),
            ('F', [1, 1, 1, 1])
          ]
