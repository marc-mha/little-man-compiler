module CodeGener where

import Data.Bits (Bits (shiftR), (.&.))
import Data.ByteString (ByteString, pack)
import Data.Word (Word16, Word8)
import Parser (POperator (..))
import Resolver

encodeOperator :: POperator -> Int
encodeOperator HLT = 0x0000
encodeOperator ADD = 0x1000
encodeOperator SUB = 0x2000
encodeOperator STA = 0x3000
encodeOperator LDA = 0x5000
encodeOperator BRA = 0x6000
encodeOperator BRZ = 0x7000
encodeOperator BRP = 0x8000
encodeOperator INP = 0x9000
encodeOperator OUT = 0xA000
encodeOperator DAT = 0x0000

encodeOperation :: ROperation -> Int
encodeOperation (RUnargumented operator) = encodeOperator operator
encodeOperation (RArgumented operator (Address operand)) = encodeOperator operator + operand

half :: [Word16] -> [Word8]
half [] = []
half (w : ws) = w1 : w2 : half ws
  where
    w1 = fromIntegral . (.&. 0xFF) . (`shiftR` 8) $ w
    w2 = fromIntegral . (.&. 0xFF) $ w

toBytes :: [Int] -> ByteString
toBytes = pack . half . map (fromIntegral . (.&. 0xFFFF))

isSequential :: [Int] -> Bool
isSequential [] = True
isSequential [_] = True
isSequential (x : y : zs) = (x + 1 == y) && isSequential (y : zs)

generate :: [Addressed ROperation] -> Maybe ByteString
generate = undefined
