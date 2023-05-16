{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Experiments.CEK where

type Var = String

data Lambda = Var :=> Exp deriving (Show, Eq)

data Exp = Ref Var
         | Lam Lambda
         | Exp :@ Exp
         deriving (Show, Eq)

type Σ    = (Exp, Env, Kont)
data D    = Clo (Lambda, Env) -- domain of values/ denotable values
type Env  = Var -> D
data Kont = Mt -- empty continuation
          | Ar (Exp   , Env, Kont) -- hold the argument `Exp`; evaluating function
          | Fn (Lambda, Env, Kont)
{- Fn - "I contain an evaluated function, and now I'm evaluating an argument term" -}


type Program = Exp

-- Is CEK push/enter or eval/apply??

step :: Σ -> Σ

-- (env ⊢ Ref x) ↦ env[x]
step (Ref x, env, k) = (Lam lam, env', k)
  where Clo (lam, env') = env x

-- (env ⊢ f e) ↦ f -- store away (e,env)
step (f :@ e, env, k) =
  (f, env, Ar (e, env, k)) -- push arg on continuation; evaluate f

-- (env ⊢ λ lam) ↦ store away (λ lam,env); pull (e, env') and eval (env ⊢ e);
step (Lam lam, env, Ar (e, env', k)) =
  (e, env', Fn (lam, env, k)) -- push func on continuation; evaluate arg

-- ((env' ⊢ λ x. e) (env ⊢ λ lam)) ↦ env'[x // (env ⊢ λ lam)] ⊢ e
-- (//) is substitution where if the first arg matches
-- you substitute or else you create a thunk
step (Lam lam, env, Fn (x :=> e, env', k)) =
  (e, env' // (x ==> Clo (lam, env)), k)

step (Lam _, _, Mt) =
  error "Pattern doesn't arise"

(==>) :: a -> b -> (a,b)
(==>) x y = (x, y)

(//) :: Eq a => (a -> b) -> (a,b) -> (a -> b)
(//) f (x,y) = \ x'->
  if (x == x')
  then y
  else f(x')

terminal :: (Σ -> Σ) -> (Σ -> Bool) -> Σ -> Σ
terminal step isFinal ς0
  | isFinal ς0 = ς0
  | otherwise = terminal step isFinal (step (ς0))

inject :: Program -> Σ
inject prog = (prog, ρ0, Mt)
  where
    ρ0 :: Env
    ρ0 = \x -> error $ "No binding for" ++ x

isFinal :: Σ -> Bool
isFinal (Lam _, _, Mt) = True
isFinal _ = False

evaluate :: Program -> Σ
evaluate pr = terminal step isFinal (inject(pr))

-- This style is not the most idiomatic Haskell
-- We could have simply used the State monad instead
-- of the recursive terminal function; but its nice
-- in that the recursion is captured outside the semantics.



-----------------------------------------------------------------


-- data Foo = Foo { a :: Int} deriving (Show, Generic)

-- data Bar = Bar { f :: Foo, b :: Int} deriving (Show, Generic)

-- x = Bar (Foo 3) 5

-- foo = x & field @"f" . field @"a" .~ 42

-- baz = x ^. field @"f" ^. field @"a"




-----------------------------------------------------------------
