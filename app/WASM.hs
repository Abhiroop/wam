module WASM where

import Data.Int
import Data.Word
import qualified Data.Vector as V
import Data.Vector.Generic.Mutable (write)

type I8   = Int8
type I16  = Int16
type I32  = Int32
type I64  = Int64
type U32  = Word32
type U64  = Word64
type F32  = Float -- using Haskell Float instead of IEEE754
type F64  = Double
type Byte = Word8

-- page-size = 64 KiB (unit of `Limits`)

-----------------------VALUES----------------------------
data Value = Byte Byte
           | U32  U32  | U64  U64
           | S32  I32  | S64  I64
           | S8   I8   | S16  I16
           | Fl32 F32  | Fl64 F64
           | Vec128 [Value] -- representing vectors as a list now
           | Name Name
           | Ref  Pointer   -- not in spec but might be convenient (NOTE 2)
           deriving (Show, Eq)

type Name = [Char]

type Pointer = Word

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

-- https://webassembly.github.io/spec/core/syntax/types.html#limits
type Limits = (U32, Maybe U32)

type MemType = Limits

data TableType = TTy Limits RefType deriving (Show, Eq)-- tabletype ::= limits reftype

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
  F64Const F64 | F64FUnOp FUnOp | F64BinOp FBinOp | F64RelOp FRelOp   |

  -- XXX: Vector instructions not included

  -- Reference Instructions
  RefNull RefType | Ref_IsNull | RefFunc FuncIdx |

  -- Parametric Instructions
  Drop | Select (Maybe ValType) |

  -- Variable Instructions
  LocalGet LocalIdx   | LocalSet LocalIdx   | LocalTee LocalIdx |
  GlobalGet GlobalIdx | GlobalSet GlobalIdx |

  -- Table Instructions
  TableGet TableIdx  | TableSet TableIdx  | TableSize TableIdx |
  TableGrow TableIdx | TableFill TableIdx | TableCopy TableIdx TableIdx |
  TableInit TableIdx ElemIdx | ElemDrop ElemIdx |

  -- Memory Instructions
  LOADI32     MemArg | STOREI32    MemArg | LOAD8I32_U MemArg | LOAD8I32_S MemArg |
  LOAD16I32_U MemArg | LOAD16I32_S MemArg | STORE8I32  MemArg | STORE16I32 MemArg |
  LOADI64     MemArg | STOREI64    MemArg | LOAD8I64_U MemArg | LOAD8I64_S MemArg |
  LOAD16I64_U MemArg | LOAD16I64_S MemArg | STORE8I64  MemArg | STORE16I64 MemArg |
  LOAD32I64_U MemArg | LOAD32I64_S MemArg | STORE32I64 MemArg |
  LOADF32     MemArg | LOADF64     MemArg | STOREF32   MemArg | STOREF64   MemArg |
  -- XXX: Vector load-store ignored
  MemorySIZE | MemoryGROW         | MemoryFILL |
  MemoryCOPY | MemoryINIT DataIdx | DataDROP DataIdx |

  -- Control Instructions
  NOP | UNREACHABLE |
  Block  BlockType [Instr] |
  Loop   BlockType [Instr] |
  IFELSE BlockType [Instr] [Instr] |
  BR LabelIdx | BR_IF LabelIdx |
  BR_Table [LabelIdx] LabelIdx | -- switch-case; last idx is default
  RETURN | CALL FuncIdx | CALL_INDIRECT TableIdx TypeIdx
  deriving (Show, Eq)

type Expr = [Instr]


data BlockType = TyIdx TypeIdx
               | ValTy (Maybe ValType) -- shorthand for () -> Maybe ValType
               deriving (Show, Eq)

newtype Offset = Offset U32 deriving (Show, Eq) -- address offset
newtype Align  = Align  U32 deriving (Show, Eq) -- alignment
type MemArg = (Offset, Align)

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

------------------------Modules-------------------------

data Module = Module { types   :: [FuncType]
                     , funcs   :: [Function]
                     , tables  :: [Table]
                     , memory  :: Memory -- see NOTE 1
                     , globals :: [Global]
                     , elems   :: [Elem]
                     , datas   :: [MemData]
                     , start   :: Maybe Start
                     , imports :: [Import]
                     , exports :: [Export]
                     }
              deriving (Show, Eq)


data Function = Function { f_ty     :: TypeIdx   -- index to `types` field in Module
                         , f_locals :: [ValType] -- referred through local index
                         , f_body   :: Expr
                         }
                deriving (Show, Eq)

-- Mimics function pointers
data Table   = T TableType Pointer deriving (Show, Eq)

------------------Memory--------------------

data Memory = Memory MemType (V.Vector Byte) deriving (Show, Eq)

-- NOTE 1;
-- https://webassembly.github.io/spec/core/syntax/modules.html#memories
-- in `Module` it could be `memory :: [Memory]` but not in the current
-- spec. According to the spec, at most one memory may be defined or imported
-- in a single module, and all constructs implicitly reference this memory 0.

------------------Memory---------------------

data Global = G { g_ty   :: GlobalType
                , g_init :: Expr
                , g_val  :: Value
                }
              deriving (Show, Eq)

-- NOTE 2
-- A global can have a `GlobalType` and a `GlobalType` includes a reference
-- type so if there is a global element with the reference type we can store
-- a `Ref x` in the `val` field, where `x` is the pointer. Albeit the integer
-- types in `Value` can capture pointer but tagging it with the `Ref` tag
-- makes it more legible. Might need to revise this.


data Elem = Elem { e_ty   :: RefType
                 , e_init :: [Expr]
                 , e_mode :: ElemMode
                 }
            deriving (Show, Eq)

data ElemMode = EPassive
              | EActive { e_table  :: TableIdx
                        , e_offset :: Expr}
              | EDeclarative
              deriving (Show, Eq)


data MemData = MemData { d_init :: V.Vector Byte
                       , d_mode :: MemDataMode
                       }
             deriving (Show, Eq)

data MemDataMode = DPassive
                 | DActive { d_memory :: MemIdx
                           , d_offset :: Expr
                           }
                 deriving (Show, Eq)

type Start = FuncIdx


data Export = Export { ex_name :: Name
                     , ex_desc :: ExportDesc
                     }
            deriving (Show, Eq)

data ExportDesc = ExFunc   FuncIdx
                | ExTable  TableIdx
                | ExMem    MemIdx
                | ExGlobal GlobalIdx
                deriving (Ord, Show, Eq)

data Import = Import { imp_module :: Name
                     , imp_name   :: Name
                     , imp_desc   :: ImportDesc
                     }
              deriving (Show, Eq)

data ImportDesc = ImpFunc   TypeIdx
                | ImpTable  TableType
                | ImpMem    MemType
                | ImpGlobal GlobalType
                deriving (Show, Eq)

-- Indices/"pointers"
type TypeIdx   = U32
type FuncIdx   = U32
type TableIdx  = U32
type MemIdx    = U32
type GlobalIdx = U32
type ElemIdx   = U32
type DataIdx   = U32
type LocalIdx  = U32
type LabelIdx  = U32

------------------------Modules-------------------------



data Trap




read :: V.Vector a -> Int -> a
read vec idx = vec V.! idx

safeRead :: V.Vector a -> Int -> Maybe a
safeRead vec idx = vec V.!? idx

-- The operation will be performed in place if it is safe
-- to do so and will modify a copy of the vector otherwise.
update :: V.Vector a -> Int -> a -> V.Vector a
update vec idx val = V.modify (\v -> write v idx val) vec
