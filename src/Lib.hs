module Lib
  ( newEVM,
    run,
    EVM (..),
    hexToDec,
    Tx (..),
    Block (..),
  )
where

import Crypto.Hash (Keccak_256 (Keccak_256), hashWith)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Maybe (fromJust, fromMaybe, isJust)
import DataSpace (DataSpace, memorySize, newDataSpace, readBytes, writeBytes)
import Debug.Trace (trace)
import Numeric (readHex, showHex)
import Stack
  ( Stack,
    stackNew,
    stackPeekN,
    stackPop,
    stackPush,
    stackSwapNM,
  )

type Bytecode = [(Char, Char)]

data EVM = EVM
  { pc :: Int,
    memory :: DataSpace,
    stack :: Stack Integer,
    storage :: DataSpace
  }
  deriving (Show)

data Tx = Tx
  { txGasprice :: Maybe String,
    txOrigin :: Maybe String,
    txFrom :: Maybe String,
    txTo :: Maybe String,
    txValue :: Maybe String,
    txData :: Maybe String
  }
  deriving (Show)

data Block = Block
  { blockCoinbase :: Maybe String,
    blockDifficulty :: Maybe String,
    blockBasefee :: Maybe String,
    blockGaslimit :: Maybe String,
    blockNumber :: Maybe String,
    blockTimestamp :: Maybe String,
    blockChainid :: Maybe String
  }
  deriving (Show)

data EVMOperation
  = STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | SDIV
  | MOD
  | SMOD
  | ADDMOD
  | MULMOD
  | EXP
  | SIGNEXTEND
  | LT
  | GT
  | SLT
  | SGT
  | EQ
  | ISZERO
  | AND
  | OR
  | XOR
  | NOT
  | BYTE
  | SHL
  | SHR
  | SAR
  | SHA3
  | ADDRESS
  | BALANCE
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATALOAD
  | CALLDATASIZE
  | CALLDATACOPY
  | CODESIZE
  | CODECOPY
  | GASPRICE
  | EXTCODESIZE
  | EXTCODECOPY
  | RETURNDATASIZE
  | RETURNDATACOPY
  | EXTCODEHASH
  | BLOCKHASH
  | COINBASE
  | TIMESTAMP
  | NUMBER
  | DIFFICULTY
  | GASLIMIT
  | CHAINID
  | SELFBALANCE
  | BASEFEE
  | POP
  | MLOAD
  | MSTORE
  | MSTORE8
  | SLOAD
  | SSTORE
  | JUMP
  | JUMPI
  | PC
  | MSIZE
  | GAS
  | JUMPDEST
  | PUSH0
  | PUSH1
  | PUSH2
  | PUSH3
  | PUSH4
  | PUSH5
  | PUSH6
  | PUSH7
  | PUSH8
  | PUSH9
  | PUSH10
  | PUSH11
  | PUSH12
  | PUSH13
  | PUSH14
  | PUSH15
  | PUSH16
  | PUSH17
  | PUSH18
  | PUSH19
  | PUSH20
  | PUSH21
  | PUSH22
  | PUSH23
  | PUSH24
  | PUSH25
  | PUSH26
  | PUSH27
  | PUSH28
  | PUSH29
  | PUSH30
  | PUSH31
  | PUSH32
  | DUP1
  | DUP2
  | DUP3
  | DUP4
  | DUP5
  | DUP6
  | DUP7
  | DUP8
  | DUP9
  | DUP10
  | DUP11
  | DUP12
  | DUP13
  | DUP14
  | DUP15
  | DUP16
  | SWAP1
  | SWAP2
  | SWAP3
  | SWAP4
  | SWAP5
  | SWAP6
  | SWAP7
  | SWAP8
  | SWAP9
  | SWAP10
  | SWAP11
  | SWAP12
  | SWAP13
  | SWAP14
  | SWAP15
  | SWAP16
  | LOG0
  | LOG1
  | LOG2
  | LOG3
  | LOG4
  | CREATE
  | CALL
  | CALLCODE
  | RETURN
  | DELEGATECALL
  | CREATE2
  | STATICCALL
  | REVERT
  | INVALID
  | SELFDESTRUCT
  deriving (Show, Eq, Enum)

pushOps :: [EVMOperation]
pushOps =
  [ PUSH0,
    PUSH1,
    PUSH2,
    PUSH3,
    PUSH4,
    PUSH5,
    PUSH6,
    PUSH7,
    PUSH8,
    PUSH9,
    PUSH10,
    PUSH11,
    PUSH12,
    PUSH13,
    PUSH14,
    PUSH15,
    PUSH16,
    PUSH17,
    PUSH18,
    PUSH19,
    PUSH20,
    PUSH21,
    PUSH22,
    PUSH23,
    PUSH24,
    PUSH25,
    PUSH26,
    PUSH27,
    PUSH28,
    PUSH29,
    PUSH30,
    PUSH31,
    PUSH32
  ]

binaryOps :: [EVMOperation]
binaryOps =
  [ ADD,
    MUL,
    SUB,
    DIV,
    SDIV,
    MOD,
    SMOD,
    EXP,
    SIGNEXTEND,
    Lib.LT,
    Lib.GT,
    SLT,
    SGT,
    Lib.EQ,
    AND,
    OR,
    XOR,
    BYTE,
    SHL,
    SHR,
    SAR,
    SHA3
  ]

dupOps :: [EVMOperation]
dupOps =
  [ DUP1,
    DUP2,
    DUP3,
    DUP4,
    DUP5,
    DUP6,
    DUP7,
    DUP8,
    DUP9,
    DUP10,
    DUP11,
    DUP12,
    DUP13,
    DUP14,
    DUP15,
    DUP16
  ]

swapOps :: [EVMOperation]
swapOps =
  [ SWAP1,
    SWAP2,
    SWAP3,
    SWAP4,
    SWAP5,
    SWAP6,
    SWAP7,
    SWAP8,
    SWAP9,
    SWAP10,
    SWAP11,
    SWAP12,
    SWAP13,
    SWAP14,
    SWAP15,
    SWAP16
  ]

newEVM :: EVM
newEVM = EVM {pc = 0, memory = newDataSpace, stack = stackNew, storage = newDataSpace}

hexToDec :: [Char] -> Integer
hexToDec hexStr = read ("0x" ++ hexStr) :: Integer

hexToDecFull :: [Char] -> Integer
hexToDecFull hexStr = read (hexStr) :: Integer

sha3_256 :: ByteString -> String
sha3_256 = show . hashWith Keccak_256

hexToOperation :: (Char, Char) -> EVMOperation
hexToOperation (b1, b2) =
  case hexToDec [b1, b2] of
    0x00 -> STOP
    0x01 -> ADD
    0x02 -> MUL
    0x03 -> SUB
    0x04 -> DIV
    0x05 -> SDIV
    0x06 -> MOD
    0x07 -> SMOD
    0x08 -> ADDMOD
    0x09 -> MULMOD
    0x0A -> EXP
    0x0B -> SIGNEXTEND
    0x10 -> Lib.LT
    0x11 -> Lib.GT
    0x12 -> SLT
    0x13 -> SGT
    0x14 -> Lib.EQ
    0x15 -> ISZERO
    0x16 -> AND
    0x17 -> OR
    0x18 -> XOR
    0x19 -> NOT
    0x1A -> BYTE
    0x1B -> SHL
    0x1C -> SHR
    0x1D -> SAR
    0x20 -> SHA3
    0x30 -> ADDRESS
    0x31 -> BALANCE
    0x32 -> ORIGIN
    0x33 -> CALLER
    0x34 -> CALLVALUE
    0x35 -> CALLDATALOAD
    0x36 -> CALLDATASIZE
    0x37 -> CALLDATACOPY
    0x38 -> CODESIZE
    0x39 -> CODECOPY
    0x3A -> GASPRICE
    0x3B -> EXTCODESIZE
    0x3C -> EXTCODECOPY
    0x3D -> RETURNDATASIZE
    0x3E -> RETURNDATACOPY
    0x3F -> EXTCODEHASH
    0x40 -> BLOCKHASH
    0x41 -> COINBASE
    0x42 -> TIMESTAMP
    0x43 -> NUMBER
    0x44 -> DIFFICULTY
    0x45 -> GASLIMIT
    0x46 -> CHAINID
    0x47 -> SELFBALANCE
    0x48 -> BASEFEE
    0x50 -> POP
    0x51 -> MLOAD
    0x52 -> MSTORE
    0x53 -> MSTORE8
    0x54 -> SLOAD
    0x55 -> SSTORE
    0x56 -> JUMP
    0x57 -> JUMPI
    0x58 -> PC
    0x59 -> MSIZE
    0x5A -> GAS
    0x5B -> JUMPDEST
    0x5F -> PUSH0
    0x60 -> PUSH1
    0x61 -> PUSH2
    0x62 -> PUSH3
    0x63 -> PUSH4
    0x64 -> PUSH5
    0x65 -> PUSH6
    0x66 -> PUSH7
    0x67 -> PUSH8
    0x68 -> PUSH9
    0x69 -> PUSH10
    0x6A -> PUSH11
    0x6B -> PUSH12
    0x6C -> PUSH13
    0x6D -> PUSH14
    0x6E -> PUSH15
    0x6F -> PUSH16
    0x70 -> PUSH17
    0x71 -> PUSH18
    0x72 -> PUSH19
    0x73 -> PUSH20
    0x74 -> PUSH21
    0x75 -> PUSH22
    0x76 -> PUSH23
    0x77 -> PUSH24
    0x78 -> PUSH25
    0x79 -> PUSH26
    0x7A -> PUSH27
    0x7B -> PUSH28
    0x7C -> PUSH29
    0x7D -> PUSH30
    0x7E -> PUSH31
    0x7F -> PUSH32
    0x80 -> DUP1
    0x81 -> DUP2
    0x82 -> DUP3
    0x83 -> DUP4
    0x84 -> DUP5
    0x85 -> DUP6
    0x86 -> DUP7
    0x87 -> DUP8
    0x88 -> DUP9
    0x89 -> DUP10
    0x8A -> DUP11
    0x8B -> DUP12
    0x8C -> DUP13
    0x8D -> DUP14
    0x8E -> DUP15
    0x8F -> DUP16
    0x90 -> SWAP1
    0x91 -> SWAP2
    0x92 -> SWAP3
    0x93 -> SWAP4
    0x94 -> SWAP5
    0x95 -> SWAP6
    0x96 -> SWAP7
    0x97 -> SWAP8
    0x98 -> SWAP9
    0x99 -> SWAP10
    0x9A -> SWAP11
    0x9B -> SWAP12
    0x9C -> SWAP13
    0x9D -> SWAP14
    0x9E -> SWAP15
    0x9F -> SWAP16
    0xA0 -> LOG0
    0xA1 -> LOG1
    0xA2 -> LOG2
    0xA3 -> LOG3
    0xA4 -> LOG4
    0xF0 -> CREATE
    0xF1 -> CALL
    0xF2 -> CALLCODE
    0xF3 -> RETURN
    0xF4 -> DELEGATECALL
    0xF5 -> CREATE2
    0xFA -> STATICCALL
    0xFD -> REVERT
    0xFE -> INVALID
    0xFF -> SELFDESTRUCT
    _ -> error "Unknown operation code (during translation)."

run :: EVM -> Maybe Tx -> Maybe Block -> String -> Maybe EVM
run evm tx block code =
  case stringToBytecode code of
    Just bc -> execute evm tx block bc
    Nothing -> Nothing

stringToBytecode :: String -> Maybe Bytecode
stringToBytecode [] = Just []
stringToBytecode (op1 : op2 : bc) = (:) <$> Just (op1, op2) <*> stringToBytecode bc
stringToBytecode _ = Nothing

execute :: EVM -> Maybe Tx -> Maybe Block -> Bytecode -> Maybe EVM
execute evm _ _ [] = Just evm
execute evm tx block bc
  | pc evm < 0 = Nothing
  | pc evm >= length bc = Just evm
  | bc !! pc evm == ('0', '0') = Just evm {pc = pc evm + 1}
  | isPushOp (bc !! pc evm) =
      let byte = bc !! pc evm
          pushNumber = (16 * (digitToInt (fst byte) - 6) + digitToInt (snd byte)) + 1
       in runOperation evm tx block (bc !! pc evm) (Just (flatten (getSublist (pc evm + 1) pushNumber bc))) >>= \evm' -> execute evm' tx block bc
  | bc !! pc evm == ('5', '6') =
      stackPop (stack evm) >>= \(stack', x) ->
        if checkJumpdest bc (fromInteger x) then execute evm {pc = fromInteger x, stack = stack'} tx block bc else Nothing
  | bc !! pc evm == ('5', '7') =
      stackPop (stack evm) >>= \(stack', x) ->
        stackPop stack' >>= \(stack'', y) ->
          if y /= 0 && checkJumpdest bc (fromInteger x) then execute evm {pc = fromInteger x, stack = stack''} tx block bc else execute evm {pc = pc evm + 1, stack = stack''} tx block bc
  | otherwise = runOperation evm tx block (bc !! pc evm) Nothing >>= \evm' -> execute evm' tx block bc

isPushOp :: (Char, Char) -> Bool
isPushOp ('6', _) = True
isPushOp ('7', _) = True
isPushOp _ = False

getSublist :: Int -> Int -> Bytecode -> Maybe Bytecode
getSublist n i list
  | n < 0 || i < 0 || n + i > length list = Nothing
  | otherwise = Just $ take i . drop n $ list

flatten :: Maybe Bytecode -> [Char]
flatten Nothing = []
flatten (Just xs) = concatMap (\(c1, c2) -> [c1, c2]) xs

checkJumpdest :: Bytecode -> Int -> Bool
checkJumpdest bc pos
  | pos < 0 || pos >= length bc = False
  | bc !! pos /= ('5', 'b') = False
  | otherwise = notInRanges pos (pushRanges bc 0)

pushRanges :: Bytecode -> Int -> [(Int, Int)]
pushRanges [] _ = []
pushRanges (byte : bc) pos
  | isPushOp byte = (pos, pos + pushNumber) : pushRanges (drop pushNumber bc) (pos + pushNumber + 1)
  | otherwise = pushRanges bc (pos + 1)
  where
    pushNumber = (16 * (digitToInt (fst byte) - 6) + digitToInt (snd byte)) + 1

notInRanges :: Int -> [(Int, Int)] -> Bool
notInRanges _ [] = True
notInRanges pos ((start, end) : ranges) = (pos < start || pos > end) && notInRanges pos ranges

integerToHex :: Integer -> String
integerToHex n =
  let hex = showHex n ""
   in replicate (64 - length hex) '0' ++ hex

integerToHex8 :: Integer -> String
integerToHex8 n = showHex n ""

hexToByteString :: String -> ByteString
hexToByteString hexStr = BS.pack $ map (fst . head . readHex) (groupPairs hexStr)
  where
    groupPairs [] = []
    groupPairs (a : b : rest) = [a, b] : groupPairs rest
    groupPairs _ = error "Invalid hex string"

runOperation :: EVM -> Maybe Tx -> Maybe Block -> (Char, Char) -> Maybe [Char] -> Maybe EVM
runOperation evm tx block op d
  | hexop == STOP = Just evm {pc = pc evm + 1}
  | hexop == ADDMOD =
      stackPop (stack evm) >>= \(stack', x) ->
        stackPop stack' >>= \(stack'', y) ->
          stackPop stack'' >>= \(stack''', z) ->
            Just evm {pc = pc evm + 1, stack = stackPush stack''' ((x + y) `mod` z)}
  | hexop == MULMOD =
      stackPop (stack evm) >>= \(stack', x) ->
        stackPop stack' >>= \(stack'', y) ->
          stackPop stack'' >>= \(stack''', z) ->
            Just evm {pc = pc evm + 1, stack = stackPush stack''' ((x * y) `mod` z)}
  | hexop == POP = stackPop (stack evm) >>= \(stack', _) -> Just evm {pc = pc evm + 1, stack = stack'}
  | hexop == ISZERO =
      stackPop (stack evm) >>= \(stack', x) ->
        Just evm {pc = pc evm + 1, stack = stackPush stack' (if x == 0 then 1 else 0)}
  | hexop == NOT =
      stackPop (stack evm) >>= \(stack', x) ->
        Just evm {pc = pc evm + 1, stack = stackPush stack' (complement x)}
  | hexop == PC = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (fromIntegral (pc evm))}
  | hexop == INVALID = Nothing
  | hexop == JUMPDEST = Just evm {pc = pc evm + 1}
  | hexop == GAS = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) ((2 ^ 256) - 1)}
  | hexop == MSTORE =
      stackPop (stack evm) >>= \(stack', x) ->
        stackPop stack' >>= \(stack'', y) ->
          Just evm {pc = pc evm + 1, stack = stack'', memory = writeBytes (memory evm) (fromInteger x) (integerToHex y)}
  | hexop == MSTORE8 =
      stackPop (stack evm) >>= \(stack', x) ->
        stackPop stack' >>= \(stack'', y) ->
          Just evm {pc = pc evm + 1, stack = stack'', memory = writeBytes (memory evm) (fromInteger x) (integerToHex8 (y `mod` 256))}
  | hexop == SHA3 =
      stackPop (stack evm) >>= \(stack', x) ->
        stackPop stack' >>= \(stack'', y) ->
          let (memory', bytes) = readBytes (memory evm) (fromInteger x) (fromInteger y)
           in Just evm {pc = pc evm + 1, stack = stackPush stack'' $ hexToDec (sha3_256 (hexToByteString bytes)), memory = memory'}
  | hexop == MLOAD =
      stackPop (stack evm) >>= \(stack', x) ->
        let (memory', bytes) = readBytes (memory evm) (fromInteger x) 32
         in Just evm {pc = pc evm + 1, memory = memory', stack = stackPush stack' (hexToDec bytes)}
  | hexop == MSIZE = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (fromIntegral (memorySize (memory evm)))}
  | hexop == ADDRESS = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (txTo (fromJust tx))))}
  | hexop == CALLER = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (txFrom (fromJust tx))))}
  | hexop == ORIGIN = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (txOrigin (fromJust tx))))}
  | hexop == GASPRICE = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (txGasprice (fromJust tx))))}
  | hexop == BASEFEE = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (blockBasefee (fromJust block))))}
  | hexop == COINBASE = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (blockCoinbase (fromJust block))))}
  | hexop == TIMESTAMP = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (blockTimestamp (fromJust block))))}
  | hexop == NUMBER = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (blockNumber (fromJust block))))}
  | hexop == DIFFICULTY = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (blockDifficulty (fromJust block))))}
  | hexop == GASLIMIT = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (blockGaslimit (fromJust block))))}
  | hexop == CHAINID = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (blockChainid (fromJust block))))}
  | hexop == BLOCKHASH = stackPop (stack evm) >>= \(stack', _) -> Just evm {pc = pc evm + 1, stack = stackPush stack' 0}
  | hexop == CALLVALUE = Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (hexToDecFull (fromJust (txValue (fromJust tx))))}
  | hexop == CALLDATALOAD =
      stackPop (stack evm) >>= \(stack', x) ->
        let value = fromJust (txData (fromJust tx))
            offset = fromInteger x * 2
            data' = if offset < length value then take 64 (drop offset value) else replicate 64 '0'
         in Just evm {pc = pc evm + 1, stack = stackPush stack' (hexToDec (data' ++ replicate (64 - length data') '0'))}
  | hexop == CALLDATASIZE =
      let value = if isJust tx then length (fromJust (txData (fromJust tx))) `div` 2 else 0
       in Just evm {pc = pc evm + 1, stack = stackPush (stack evm) (toInteger value)}
  | hexop `elem` swapOps = Just evm {pc = pc evm + 1, stack = stackSwapNM (stack evm) (fromEnum hexop - fromEnum SWAP1 + 1) 0}
  | hexop `elem` dupOps =
      let n = fromEnum hexop - fromEnum DUP1
       in stackPeekN (stack evm) n >>= \x -> Just evm {pc = pc evm + 1, stack = stackPush (stack evm) x}
  | hexop `elem` binaryOps =
      stackPop (stack evm) >>= \(stack', x) ->
        stackPop stack' >>= \(stack'', y) ->
          let result = performBinaryOp hexop x y
           in Just evm {pc = pc evm + 1, stack = stackPush stack'' result}
  | hexop `elem` pushOps =
      let n = fromEnum hexop - fromEnum PUSH1 + 2
       in Just evm {pc = pc evm + n, stack = stackPush (stack evm) hexdata}
  | otherwise = error $ "Unknown operation code " ++ [fst op, snd op]
  where
    hexop = hexToOperation op
    hexdata = hexToDec (fromMaybe "0" d)
    performBinaryOp op x y = case op of
      ADD -> (x + y) `mod` (2 ^ 256)
      MUL -> (x * y) `mod` (2 ^ 256)
      SUB -> (x - y) `mod` (2 ^ 256)
      DIV -> if y == 0 then 0 else x `div` y
      SDIV -> if y == 0 then 0 else signed x `div` signed y
      MOD -> if y == 0 then 0 else x `mod` y
      SMOD -> if y == 0 then 0 else signed x `mod` signed y
      EXP -> (x ^ y) `mod` (2 ^ 256)
      SIGNEXTEND -> signExtend x y
      Lib.LT -> if x < y then 1 else 0
      Lib.GT -> if x > y then 1 else 0
      SLT -> if signed x < signed y then 1 else 0
      SGT -> if signed x > signed y then 1 else 0
      Lib.EQ -> if x == y then 1 else 0
      AND -> x .&. y
      OR -> x .|. y
      XOR -> x `xor` y
      BYTE -> if y == 0 || x > 31 then 0 else (y `shiftR` (8 * fromIntegral (31 - x))) .&. 0xFF
      SHL -> (y `shiftL` fromIntegral x) `mod` (2 ^ 256)
      SHR -> (y `shiftR` fromIntegral x) `mod` (2 ^ 256)
      SAR -> shiftArithmeticRight y x
      _ -> error "Unsupported binary operation"

signed :: Integer -> Integer
signed x = if x >= 2 ^ 255 then x - 2 ^ 256 else x

signExtend :: Integer -> Integer -> Integer
signExtend t x
  | t >= 31 = x
  | otherwise =
      let bitPosition = fromIntegral (t + 1) * 8 - 1
          mask = 1 `shiftL` bitPosition
          signBit = (x .&. mask) /= 0
       in if signBit
            then x .|. complement (mask - 1)
            else x .&. (mask - 1)

shiftArithmeticRight :: Integer -> Integer -> Integer
shiftArithmeticRight x y
  | y >= 256 = if testBit x 255 then -1 else 0
  | otherwise =
      let shifted = (x .&. maxEvmWord) `shiftR` fromIntegral y
       in if testBit x 255
            then shifted .|. (maxEvmWord `shiftL` fromIntegral (256 - y))
            else shifted
  where
    maxEvmWord = 2 ^ 256 - 1
