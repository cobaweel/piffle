{- Piffle, Copyright (C) 2007, Jaap Weel. This program is free
   software; you can redistribute it and/or modify it under the terms
   of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option)
   any later version.  This program is distributed in the hope that it
   will be useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU General Public License for more details.  You should
   have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc.,
   59 Temple Place, Suite 330, Boston, MA 02111-1307 USA -}

-- TRANSFORM IR ------------------------------------------------------

{- The type of the transformation in this file says that it transforms
   from IR to IR, but so far, it doesn't actually change the IR it
   operates on. What it does do is go through it to count how much
   memory and how many CPU cycles it approximately uses, and throw an
   error (in the Compiler monad) if the resource bounds are exceeded. -}

module IrIr where
import qualified Data.Map as M
import Ir
import Position
import Control.Monad
import Compiler
import qualified Configure

-- RESOURCE USAGE ----------------------------------------------------

{- This type contains two integers, which represents rough estimates
   of respectively the amount of CPU time used and the amount of
   memory used. -}

data Bounds = Bounds { memBound, cpuBound :: Integer }
              deriving (Show)

(.+.) :: Bounds -> Bounds -> Bounds 
Bounds a b .+. Bounds x y =
    Bounds (a+x) (b+y)

(+.) :: Bounds -> Integer -> Bounds 
Bounds a b +. y =
    Bounds a (b+y)

(.+) :: Bounds -> Integer -> Bounds
Bounds a b .+ x =
    Bounds (a+x) b

(.<.) :: Bounds -> Bounds -> Bool
Bounds a b .<. Bounds x y =
    a < x && b < y

(.*.) :: Integral a => a -> Bounds -> Bounds
n .*. Bounds a b =
    Bounds a ((fromIntegral n)*b) -- Memory does not multiply!!

nil :: Bounds
nil =
    Bounds 0 0

mem :: Integral a => a -> Bounds
mem x =
    nil { memBound = fromIntegral x }

cpu :: Integral a => a -> Bounds
cpu x =
    nil { cpuBound = fromIntegral x }

maxBounds :: Bounds -> Bounds -> Bounds
maxBounds (Bounds a b) (Bounds x y) =
    Bounds (max a x) (max b y)

-- ENVIRONMENTS ------------------------------------------------------

{- In this pass, we'll be passing an environment around to keep track
   of the cost of functions. -}

type Env = M.Map Ident Bounds

empty :: Env
empty = M.empty

insertFun :: Ident -> Bounds -> Env -> Env
insertFun = M.insert

getFun :: Ident -> Env -> Compiler Bounds
getFun f env = maybe wrong return (M.lookup f env)
    where wrong = die ("function not in scope: " ++ show f)



-- TRANSLATE A FILE --------------------------------------------------

tFile :: File -> Compiler File
tFile (File ds) = 
    do Configure.Config { Configure.cpu = c, 
                          Configure.mem =  m, 
                          Configure.file = f } <- getConfig
       (env, gbl) <- tDeclarations empty ds
       thisPosition (fakePos f)
       filter <- getFun (Ident "filter") env
       let Bounds m' c' = filter .+. gbl in
        do case c of 
             Nothing ->
                  return ()
             Just c ->
                  when (c' > c) (whineAbout "cpu" c' c)
           case m of 
             Nothing ->
                  return ()
             Just m ->
                  when (m' > m) (whineAbout "memory" m' m)
           return (File ds)
    where 
      whineAbout s x y = 
          die (s ++ " use (" ++ show x ++ ") exceeds bound of " ++ show y)

-- TRANSLATE DECLARATIONS --------------------------------------------

tDeclarations :: Env -> [Declaration] -> Compiler (Env, Bounds)

tDeclarations env (d:ds) = 
    do (env, c) <- tDeclaration env d
       (env, cs) <- tDeclarations env ds
       return (env, c .+. cs)

tDeclarations env [] = 
    return (env, nil)

{- We assume each function argument consumes one word of memory;
   arrays are passed by reference. -}

tDeclaration env (Defun _ (Typing i _) argl e) = 
    do c <- tExp env e 
       return (insertFun i (c .+. mem (length argl)) env, nil)

{- We will crudely assume that each array element consumes a word of
   memory. XXXWe actually have more information than that. Can't we
   measure memory in bytes?XXX -}

tDeclaration env (Defvar _ (Typing _ t)) = 
    case t of 
      Array i _ ->
          return (env, mem i)
      _ ->
          return (env, mem 1)


-- TRANSLATE EXPRESSIONS ---------------------------------------------

{- Operators and indexing add one tick to the computation. -}  

tExp env (Binop p _ x _ y) =
    do thisPosition p
       cx <- tExp env x
       cy <- tExp env y
       return (cpu 1 .+. cx .+. cy)

tExp env (Unop p _ _ x) =
    do thisPosition p
       cx <- tExp env x
       return (cpu 1 .+. cx)

tExp env (Index p _ x y) =
    do thisPosition p
       cx <- tExp env x
       cy <- tExp env y
       return (cpu 1 .+. cx .+. cy)


{- Casts are dealt with at compile time, so they add 0 ticks. -}

tExp env (Cast p _ x _) =
    do thisPosition p
       cx <- tExp env x
       return cx

{- An application adds a tick, and whatever cost it takes to prepare
   the actual parameters. -}

tExp env (Apply p _ f xs) =
    do thisPosition p
       cxs <- tExp env `mapM` xs
       cf <- getFun f env
       return (cpu 1 .+. cf .+. foldr (.+.) nil cxs)

{- Because we can define functions within a block, we need to pass the
   environment around here. -}

tExp env (Seq p _ declarations expressions lastExpression) =
    do thisPosition p
       (env, costOfDeclarations) <- tDeclarations env declarations
       costsOfExpressions <- tExp env `mapM` (expressions ++ [lastExpression])
       return (foldr (.+.) nil (costOfDeclarations:costsOfExpressions))

{- We must pessimitically assume that an conditional adds the greatest
   cost, in both memory and CPU, of both of its branches. (Did this
   wrong initially!) -}

tExp env (If p _ condition consequent alternative) = 
    do thisPosition p
       costOfCondition <- tExp env condition
       costOfConsequent <- tExp env consequent
       costOfAlternative <- tExp env alternative
       return (costOfCondition .+. 
               (if costOfConsequent .<. costOfAlternative then
                    costOfConsequent 
                else
                    costOfAlternative))

{- A loop that is executed N times adds the time cost of its body (and
   condition clause) N times. Note that the .*. operator does not
   multiply the memory used, which is correct, because it gets pushed
   and popped on and off the stack all the time. -}

tExp env (ForIn p _ (iterations, _, _, _) _ range condition body) =
    do thisPosition p
       costOfRange <- tExp env range
       costOfCondition <- tExp env condition
       costOfBody <- tExp env body
       return (costOfRange .+. 
               (iterations .*. (costOfCondition .+. costOfBody)))

tExp env (ForFromTo p _ _ lb ub condition body) =
    do thisPosition p
       costOfCondition <- tExp env condition
       costOfBody <- tExp env body
       when (iterations < 0) (die "bounds are wrong")
       return (iterations .*. (costOfCondition .+. costOfBody))
    where 
      iterations = ub - lb

tExp _ _ =
    return (cpu 1)
