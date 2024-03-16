module Lib
  ( newEVM,
    run,
    EVM (..),
    hexToDec,
  )
where

import Crypto.Hash (Digest, SHA3_256, hash)
import Data.Bits
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Data.Stack (Stack, stackNew, stackPop, stackPush, stackSize)

data EVM = EVM
  { pc :: Int,
    memory :: [Int],
    stack :: Stack Integer,
    storage :: [(Int, Int)]
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

newEVM :: EVM
newEVM = EVM {pc = 0, memory = [], stack = stackNew, storage = []}

hexToDec :: String -> Integer
hexToDec hexStr = read ("0x" ++ hexStr) :: Integer

hexToOperation :: String -> EVMOperation
hexToOperation hexStr =
  case hexToDec hexStr of
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
    _ -> error "Unknown operation code."

run :: EVM -> String -> Maybe EVM
run evm [] = Just evm
run evm (op1 : op2 : bc)
  | op1 == '6' && op2 >= '0' && op2 <= 'F' =
      let n = 2 * (digitToInt op2 + 1)
       in runOperation evm [op1, op2] (Just (take n bc)) >>= \evm' -> run evm' (drop n bc)
  | otherwise = runOperation evm [op1, op2] Nothing >>= \evm' -> run evm' bc
run evm (b1 : b2 : bc) = runOperation evm [b1, b2] Nothing >>= \evm' -> run evm' bc

runOperation :: EVM -> String -> Maybe [Char] -> Maybe EVM
runOperation evm op d
  | hexop == STOP = Just evm {pc = pc evm + 1}
  | hexop `elem` binaryOps =
      stackPop (stack evm) >>= \(stack', x) ->
        stackPop stack' >>= \(stack'', y) ->
          let result = performBinaryOp hexop x y
           in Just evm {pc = pc evm + 1, stack = stackPush stack'' result}
  | hexop `elem` pushOps =
      let n = fromEnum hexop - fromEnum PUSH1 + 2
       in Just evm {pc = pc evm + n, stack = stackPush (stack evm) hexdata}
  | otherwise = error "Unknown operation code."
  where
    hexop = hexToOperation op
    hexdata = hexToDec (fromMaybe "0" d)
    performBinaryOp op x y = case op of
      ADD -> x + y
      MUL -> x * y
      SUB -> x - y
      DIV -> if y == 0 then 0 else x `div` y
      SDIV -> if y == 0 then 0 else x `div` y -- Necesita manejo especial para números negativos
      MOD -> if y == 0 then 0 else x `mod` y
      SMOD -> if y == 0 then 0 else x `mod` y -- Necesita manejo especial para números negativos
      EXP -> x ^ y -- Considerar limitación de tamaño
      SIGNEXTEND -> signExtend x y -- Implementar la lógica de sign-extend
      Lib.LT -> if x < y then 1 else 0
      Lib.GT -> if x > y then 1 else 0
      SLT -> if signed x < signed y then 1 else 0 -- Implementar la lógica para comparación con signo
      SGT -> if signed x > signed y then 1 else 0 -- Implementar la lógica para comparación con signo
      Lib.EQ -> if x == y then 1 else 0
      AND -> x .&. y
      OR -> x .|. y
      XOR -> x `xor` y
      BYTE -> (x `shiftR` (8 * fromIntegral y)) .&. 0xFF -- Implementar la lógica para obtener el enésimo byte
      SHL -> x `shiftL` fromIntegral y
      SHR -> x `shiftR` fromIntegral y
      SAR -> x
      SHA3 -> x -- I have to define memory first
      _ -> error "Unsupported binary operation"

signed :: Integer -> Integer
signed x = if x >= 2 ^ 255 then x - 2 ^ 256 else x

signExtend :: Integer -> Integer -> Integer
signExtend x y = if x >= 2 ^ (8 * y) then x - 2 ^ (8 * y) else x
