module WASM where

import Data.Int
import Data.Word

type I32 = Int32
type I64 = Int64
type U32 = Word32
type U64 = Word64
type F32 = Float
type F64 = Double
-----------------------VALUES----------------------------
data Value = S32  I32  | S64  I64
           | U32  U32  | U64  U64
           | Fl32 F32  | Fl64 F64
           | Vec128 [Value]
           -- | Ref Ptr
           -- the VecVal8 and VecVal16 types
           -- are only found inside vectors
           | VecVal8 Int8 | VecVal16 Int16
           deriving (Show, Eq)

type Name = [Char]

-- type Ptr = Int -- pointer

-----------------------VALUES----------------------------

-----------------------TYPES----------------------------
data NumType  = I32 | I64 | F32 | F64 deriving (Show, Eq)
data VecType  = V128 deriving (Show, Eq)
data RefType = FuncRef   -- reference to functions defined withing the module
             | ExternRef -- something like `malloc`
             deriving (Show, Eq)

data ValType = NTy NumType | VTy VecType | RTy RefType deriving (Show, Eq)

type ResultType = [ValType]

-- ResultType -> ResultType
data FuncType = FTy ResultType ResultType deriving Eq

instance Show FuncType where
  show (FTy argty resty) =
    show argty <> "->" <> show resty

type MemType    = (U32, U32)

type TableType  = (U32, U32) -- tabletype ::= limits reftype

data Mut = Const | Var deriving (Show, Eq)

data GlobalType = GTy Mut ValType deriving (Show, Eq)

data ExternType = Func FuncType
                | Table TableType
                | Mem MemType
                | Global GlobalType
                deriving (Show, Eq)

-----------------------TYPES----------------------------


-----------------------Instructions---------------------
data Instr = -- Numeric Instructions --
  I32Const U32 | I32UnOp IUnOp  | I32BinOp IBinOp | I32TestOp ITestOp | I32RelOp IRelOp |
  -- XXX: Extend onwards not included
  I64Const U64 | I64UnOp IUnOp  | I64BinOp IBinOp | I64TestOp ITestOp | I64RelOp IRelOp |
  F32Const F32 | F32FUnOp FUnOp | F32BinOp FBinOp | F32RelOp FRelOp   |
  F64Const F64 | F64FUnOp FUnOp | F64BinOp FBinOp | F64RelOp FRelOp
  deriving (Show, Eq)

data IUnOp  = Clz  | Ctz | PopCnt deriving (Show, Eq)
data IBinOp = ADD | SUB | MUL | DIV_U | DIV_S | REM_U | REM_S
            | AND | OR  | XOR | SHL   | SHR_U | SHR_S | ROTL
            | ROTR deriving (Show, Eq)

data FUnOp = ABS | NEG | SQRT | CEIL | FLOOR | TRUNC | NEAREST deriving (Show, Eq)

data FBinOp = ADD_F | SUB_F | MUL_F | DIV | MIN | MAX | COPYSIGN deriving (Show, Eq)

data ITestOp = EQZ deriving (Show, Eq)
data IRelOp  = EQ   | NE   | LT_U | LT_S | GT_U
             | GT_S | LE_U | LE_S | GE_U | GE_S deriving (Show, Eq)

data FRelOp = EQ_F | NE_F | LT_F | GT_F | LE_F | GE_F deriving (Show, Eq)

-----------------------Instructions---------------------

data Trap

data Function

data Table -- used for emulating function pointers

data Memory

data Module



