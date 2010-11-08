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

module IrPiffle where
import Data.Maybe(maybe)
import Position
import qualified Data.Map as M
import Compiler
import Piffle
import qualified Ir as I
import PrettyPP

-- TYPE CHECKER ------------------------------------------------------

{- This pass converts the Piffle code to IR, which means, practically,
   that it adds checks types and inserts type annotations. This is
   pretty straightforward, because Piffle uses static and explicit
   typing, and allows no forward declarations. -}


-- ENVIRONMENTS ------------------------------------------------------

{- The environment consists of two mappings. One maps variable names
   onto types, and the other maps function names onto function types,
   which consist of one output type and a list of input types. The
   mappings are implemented "behind the scenes" by Haskell as a
   self-balancing tree of some sort, and they are functional, which
   makes it easy to create an extended version of the same mapping
   when typechecking a block with local bindings, and still have the
   old mapping without the local bindings around for when you get
   back. -}

{- I've been told that you can hide environment handling in the
   monad. I prefer not to do that, because I think the algorithm is
   easier to understand when the environment are passed explicitly. In
   fact, I'm a little doubtful about even hiding position handling in
   the Monad; it's neat, but it also makes it too easy to forget to
   update the position. Fortunately, positions are used only for error
   messages, so they aren't as crucial as environments. -}

type FunType = (Type, [Type])

data Env = Env { venv :: M.Map Ident Type,
                 fenv :: M.Map Ident FunType }

empty :: Env
empty = Env M.empty M.empty

insertVar :: Ident -> Type -> Env -> Env
insertVar v t (Env venv fenv) =
    Env (M.insert v t venv) fenv

insertFun :: Ident -> FunType -> Env -> Env
insertFun f t (Env venv fenv) =
    Env venv (M.insert f t fenv)

getVar :: Ident -> Env -> Compiler Type
getVar  v   (Env venv _) =
    maybe wrong return (M.lookup v venv)
    where wrong =
              die ("variable " ++ show v ++ " not in scope")

getFun :: Ident -> Env -> Compiler FunType
getFun f   (Env _ fenv) =
    maybe wrong return (M.lookup f fenv)
    where wrong =
              die ("function " ++ show f ++ " not in scope")


-- GENSYMS -----------------------------------------------------------

{- This is a very primitive gensym mechanism, and it takes O(n log(n))
   in the size of the environment to generate a new gensym. It would
   be much prettier if this could be hidden away in the monad
   somewhere. (GENSYM is the name of the Common Lisp function that
   makes fresh symbols. It means GENerated SYMbol.) -}

gensym :: Ident -> String -> Type -> Env -> Compiler (Ident, Env)
gensym (Ident s) u t env =
    return (gensym' ("":show `map` [0..]) env)
    where gensym' (i:is) env@(Env venv _) =
              let g = Ident (s ++ "_" ++ u ++ i) in
              case M.lookup g venv of
                Nothing ->
                    (g, insertVar g t env)
                _ ->
                    gensym' is env
          gensym' [] _ =
              error "ONLY CALL GENSYM WITH INFINITE LISTS!"


-- HELPER FUNCTIONS --------------------------------------------------

{- These functions perform some basic operations useful in the
   construction of the type checker. -}

{- Check whether two expressions have the same type and return that
   type. Special cases are when one of them is an integer literal and
   the other is atomic, in which case we return the atomic type, or
   when one of them is void, in which case we return void.  -}

match :: I.Exp -> I.Exp -> Compiler Type
match x y = 
    case (ty x, ty y) of
      (t,u) 
          | t == u ->
              return t
      (Atomic SLiteral, u@(Atomic _)) ->
          return u
      (t@(Atomic _), Atomic SLiteral) ->
          return t
      (_,Void) ->
          return Void
      (Void,_) ->
          return Void
      (t,u) ->
          die (pp t ++ " doesn't match " ++ pp u)


{- Check whether an expression has a given type, but noting that all
   expressions are honorary members of type void, and integer literals 
   are honorary members of all atomic types. -}

hasType :: Type -> I.Exp -> Compiler ()
hasType t x = 
    case (ty x, t) of
      (Atomic SLiteral, Atomic _) ->
          return ()
      (_, Void) ->
          return ()
      (u, t) 
          | u == t ->
              return ()
          | otherwise ->
              die (pp u ++ " doesn't match " ++ pp t)


{- This function is like Prelude.zip, but it refuses to zip lists that
   do not match in length. -}

zipExactly :: [a] -> [b] -> Compiler [(a,b)]
zipExactly (x:xs) (y:ys) =
    do zs <- zipExactly xs ys
       return ((x,y):zs)
zipExactly []     [] =
    return []
zipExactly _      _ =
    die "lengths of lists don't match"


{- Assert that something must be an atomic type. -}

atomic :: Type -> Compiler Type
atomic t@(Atomic _) =
    return t
atomic t =
    die ("atomic type expected, not " ++ pp t)


{- Assert that something must be an array type, and take it apart. -}

array :: Type -> Compiler (Inty, Type)
array (Array i t) =
    return (i, Atomic t)
array t =
    die (pp t ++ " should be array type")


{- Assert that x must be an lvalue IF op is an assignment operator. -}

lvalue op x
    | op `elem` [Assign, Add_assign, Sub_assign, Mul_assign,
                 Div_assign, Mod_assign, Band_assign, Bor_assign, 
                 Xor_assign, Shl_assign, Shr_assign] =
      case x of
        I.Variable _ _ _ ->
            return x
        I.Index _ _ _ _ ->
            return x
        _ ->
            die "lvalue expected"
    | otherwise =
        return x


-- ACTUAL TYPE CHECKER -----------------------------------------------

{- Type-check a file. -}

tFile :: File -> Compiler I.File
tFile (File ds) =
    do (_, ds) <- tDeclarations empty ds
       return (I.File ds)

{- Type-check a list of declarations. This is done explicitly with
   recursion so as to make sure the environments get threaded through
   right, which is necessary to ensure proper lexical scoping. Note
   that we do not define a function within its own body, or in the
   bodies of any functions that come before it, but only in the code
   that comes after it; so we make sure right here and now that there
   can be no recursion in Piffle. -}

tDeclarations :: Env -> [Declaration] -> Compiler (Env, [I.Declaration])

tDeclarations env (d:ds) = 
    do (env, d) <- tDeclaration env d
       (env, ds) <- tDeclarations env ds
       return (env, d:ds)

tDeclarations env [] = 
    return (env, [])

tDeclaration :: Env -> Declaration -> Compiler (Env, I.Declaration)

tDeclaration env (Defvar p tp@(Typing i u)) =
    do thisPosition p
       return (insertVar i u env, I.Defvar p tp)

tDeclaration env (Defun p tp@(Typing f t) formalParameters e) = 
    do e <- tExp env' e
       thisPosition (pos e)
       _ <- hasType t e
       return (insertFun f (t,argl) env, I.Defun p tp formalParameters e)
    where env' =
              foldr (\(Typing v t) e -> insertVar v t e) env formalParameters
          argl =
              map (\(Typing _ t) -> t) formalParameters


{- Type-check and translate an expression. There isn't really much to
   translate, though, except in the "for ... in" case, where we need
   to generate some annotations to guide the code generator. -}

tExp :: Env -> Exp -> Compiler I.Exp

tExp _ (Literal p i@(IntLit _)) = 
    do thisPosition p
       return (I.Literal p (Atomic SLiteral) i)

tExp _ (Literal p u@UnitLit) = 
    do thisPosition p
       return (I.Literal p Void u)

tExp env (Variable p v) = 
    do thisPosition p
       t <- getVar v env
       return (I.Variable p t v)

{- Shift and shift-assign operations take two atomic arguments and
   return the same type as the first argument. In some cases, the
   first argument may have to be an lvalue. -}

tExp env (Binop p shifted operator shiftcount)
    | (operator `elem` [Shl, Shr, Shl_assign, Shr_assign]) =
        do thisPosition p
           shifted <- tExp env shifted
           shiftcount <- tExp env shiftcount
           returnType <- atomic (ty shifted)
           _ <- atomic (ty shiftcount)
           _ <- lvalue operator shifted
           return (I.Binop p returnType shifted operator shiftcount)

{- Relational and logical operations take two atomic arguments and
   return a boolean. -}

tExp env (Binop p operand1 operator operand2)  
    | (operator `elem` [And, Or, Gt, Lt, Ge, Le, Eq, Ne]) =
        do thisPosition p
           operand1 <- tExp env operand1
           operand2 <- tExp env operand2
           _ <- atomic (ty operand1)
           _ <- atomic (ty operand2)
           return (I.Binop p (Atomic Bool) operand1 operator operand2)

{- All other binary operators take two arguments of identical, atomic
   type and return the same type. In some cases, the first argument
   may have to be an lvalue. -}

tExp env (Binop p operand1 operator operand2) = 
    do thisPosition p
       operand1 <- tExp env operand1
       operand2 <- tExp env operand2
       returnType <- match operand1 operand2
       returnType <- atomic returnType
       _ <- lvalue operator operand1
       return (I.Binop p returnType operand1 operator operand2)

{- The ! operator takes an argument of any atomic type and returns a
   boolean.  -}

tExp env (Unop p UNot operand) =
    do thisPosition p
       operand <- tExp env operand
       _ <- atomic (ty operand)
       return (I.Unop p (Atomic Bool) UNot operand) 

{- All other unary operators take an argument of atomic type and
   return that type. -}

tExp env (Unop p operator operand) =
    do thisPosition p
       operand <- tExp env operand
       operandType <- atomic (ty operand)
       return (I.Unop p operandType operator operand) 

{- Casts can happen from any atomic type to any other atomic type. -}

tExp env (Cast p expression newAtomicType) =
    do thisPosition p
       expression <- tExp env expression
       _ <- atomic (ty expression)
       return (I.Cast p (Atomic newAtomicType) expression newAtomicType)

{- For index x[i], x must be an array of frobs, i must be atomic,
   and the expression returns a frob. -}

tExp env (Index p indexed index) = 
    do thisPosition p
       indexed <- tExp env indexed
       index <- tExp env index
       (_, elementType) <- array (ty indexed)
       _ <- atomic (ty index)
       return (I.Index p elementType indexed index)

{- For a function application, the number types of the arguments must
   match the arity and type of the function. -}

tExp env (Apply p function actualParameters) = 
    do thisPosition p
       actualParameters <- tExp env `mapM` actualParameters
       (returnType, argTypes) <- getFun function env
       argl <- zipExactly argTypes actualParameters
       _ <- uncurry (hasType) `mapM` argl
       return (I.Apply p returnType function actualParameters)

{- A sequence returns the type of the last expression. -}

tExp env (Seq p declarations expressions lastExpression) = 
    do thisPosition p
       (env, declarations) <- tDeclarations env declarations
       expressions <- tExp env `mapM` expressions
       lastExpression <- tExp env lastExpression
       return (I.Seq p (ty lastExpression) 
                declarations expressions lastExpression)

{- For an if statement, the condition must be of atomic type, and the
   types of the branches must match. -}

tExp env (If p condition consequent alternative) = 
    do thisPosition p
       condition <- tExp env condition
       consequent <- tExp env consequent
       alternative <- tExp env alternative
       _ <- atomic (ty condition)
       ty <- match consequent alternative
       return (I.If p ty condition consequent alternative)

{- While typechecking a for...in loop, we also generate to gensyms
   which the code generator will need. This is a bit of a hack, but it
   eliminates the need for passing an environment around in the code
   generator just to keep track of which symbols are in scope and
   therefore cannot be used as gensyms. We add the gensyms to the
   environment used for typechecking the subexpressions, so that
   nested loops are handled correctly. The implicit loop counter is
   deemed to be of type u32 (not that you'd ever notice.)  -}

tExp env (ForIn p i@(Ident _) a c b) = 
    do thisPosition p
       a <- tExp env a
       (n,t) <- array (ty a)
       (g, env) <- gensym i "count" (Atomic U32) env
       (h, env) <- gensym i "range" (ty a) env
       c <- tExp (insertVar i t env) c
       b <- tExp (insertVar i t env) b
       return (I.ForIn p Void (n, t, g, h) i a c b) 

{- The loop counter in a for...from...to loop is deemed to be of type
   u32, and does not need to be declared. XXXIs this a good idea? What
   about 64-bit machines?XXX Note also that the loop counter is
   defined within the condition clause. -}

tExp env (ForFromTo p counter lb ub condition block) = 
    do thisPosition p
       condition <- tExp (insertVar counter (Atomic U32) env) condition
       block <- tExp (insertVar counter (Atomic U32) env) block
       return (I.ForFromTo p Void counter lb ub condition block)

{- Extract the type from an IR expression. -}

ty :: I.Exp -> Type
ty =
    I.info

pos :: I.Exp -> Pos
pos =
    I.pos
