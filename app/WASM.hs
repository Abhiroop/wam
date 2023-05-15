{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WASM where

import Data.Int
import Data.Word
import Data.Vector (Vector, modify, (!), (!?))
import Data.Vector.Generic.Mutable (write)

import qualified Control.Monad.Trans.State.Strict as S

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
           | Vec128 (Vector Value)
           -- references
           | Ref Ref
           deriving (Show, Eq)

data Ref = Ref_Null  RefType
         | RefFAddr  FuncAddr
         | RefExtern ExternAddr
         deriving (Show, Eq)

type Name = [Char]

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
data Table   = T TableType Word32 deriving (Show, Eq)
-- XXX: Word32 is an opaque value of a particular reference type
-- https://webassembly.github.io/spec/core/syntax/modules.html#tables

------------------Memory--------------------

data Memory = Memory MemType (Vector Byte) deriving (Show, Eq)

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


data MemData = MemData { d_init :: Vector Byte
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

data Result = Result Value
            | Trap
            deriving (Show, Eq)


---------------Runtime Structures------------------------

-- Using Vector on all of the instances for better performance

data Store = Store { funcInst  :: Vector FuncInst
                   , tableInst :: Vector TableInst
                   , memInst   :: Vector MemInst
                   , glblInst  :: Vector GlobalInst
                   , elemInst  :: Vector ElemInst
                   , datasInst :: Vector DataInst
                   }
             deriving (Show, Eq)


data FuncInst =
      FuncInst { funcType   :: FuncType
               , moduleInst :: ModuleInst
               , code       :: Function
               }
    | HostInst { funcType   :: FuncType
               , hostCode   :: HostFunc
               }
    deriving (Show, Eq)

newtype HostFunc = HostFunc (Vector Value -> IO (Vector Value))

instance Show HostFunc where
  show (HostFunc _) = "<nil>"

instance Eq HostFunc where
  (HostFunc _) == (HostFunc _)
    = error "Cannot compare functions"

data ModuleInst = ModuleInst { funcTypes   :: Vector FuncType
                             , funcaddrs   :: Vector Addr
                             , tableaddrs  :: Vector Addr
                             , memaddrs    :: Vector Addr
                             , globaladdrs :: Vector Addr
                             , exportsM    :: Vector ExportInst
                             }
                deriving (Eq, Show)

data ExportInst = ExportInst { exportName  :: Name
                             , exportValue :: ExternVal
                             }
                  deriving (Eq, Show)

data ExternVal = ExternFunc  FuncAddr
               | ExternTable TableAddr
               | ExternMem   MemAddr
               | ExternGlbl  GlobalAddr
               deriving (Show, Eq)

data TableInst = TableInst { tableTy   :: TableType
                           , tableElem :: Vector Ref
                           }
                 deriving (Show, Eq)

data MemInst = MemInst { memTy   :: MemType
                       , memData :: Vector Byte
                       }
               deriving (Show, Eq)

data GlobalInst = GlobalInst { gTy  :: GlobalType
                             , gVal :: Value
                             }
                  deriving (Show, Eq)

data ElemInst = ElemInst { eTy   :: RefType
                         , eElem :: Vector Ref
                         }
              deriving (Show, Eq)

data DataInst = DataInst (Vector Byte) deriving (Show, Eq)

type Stack = [StackVal]

type ArgArity = Int

data StackVal = Val Value
              | Label ArgArity BranchTarget
              | Frame { arity   :: ArgArity
                      , locals  :: Vector Value
                      , modinst :: ModuleInst
                      }
              deriving (Show, Eq)

type BranchTarget = [Instr] -- XXX: maybe an index is a better representation

---------------Runtime Structures------------------------


data WAM = WAM { store :: Store
               , stack :: Stack
               } deriving (Show, Eq)


newtype Interpreter a = Interpreter { runInterp :: S.State WAM a }
  deriving (Functor, Applicative, Monad)


interp :: Interpreter Result
interp = undefined
{- do
l <- gets state
let (WAM {store = str) =l 
search store and get instruction i
eval i

-}

-- s |- E[i] ==> (s', a)
eval :: Instr -> Interpreter Result
eval (I32Const u32) = undefined
-- eval WAM {stack = s, store{shadowstack, heap} = str,...}(CALL funcidx (Just lib) policy) =
  -- walk the stack and box all the data which are not in the policy
  -- eval error
-- have a State monad where the program state are the two things above



----------------------Addresses-------------------------
type Addr       = Word32
type FuncAddr   = Addr
type TableAddr  = Addr
type MemAddr    = Addr
type GlobalAddr = Addr
type ElemAddr   = Addr
type DataAddr   = Addr
type ExternAddr = Addr
----------------------Addresses-------------------------

read :: Vector a -> Int -> a
read vec idx = vec ! idx

safeRead :: Vector a -> Int -> Maybe a
safeRead vec idx = vec !? idx

-- The operation will be performed in place if it is safe
-- to do so and will modify a copy of the vector otherwise.
update :: Vector a -> Int -> a -> Vector a
update vec idx val = modify (\v -> write v idx val) vec
