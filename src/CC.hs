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

-- TRANSFORM C -------------------------------------------------------

{- This module simplifies C code. It does transformations that (you'd
   hope) the C compiler will do anyway, but they make the output code
   look a lot nicer. This module is actually trickier than I thought
   it would be, especially because I want to make sure that all
   transformations are valid on C code in general, and not just on C
   code that I happen to know comes out of the Piffle compiler. I have
   fixed several bugs. Please read K&R carefully before adding a
   clever new simplification. -}

module CC where
import Control.Monad (liftM)
import C

-- THE C SIMPLIFIER --------------------------------------------------

{- Simplify a file -}

tFile :: File -> File
tFile (File ds) =
    File (map tDef ds)


{- Simplify a definition -}

tDef :: Def -> Def
tDef (Fundef s qt i b) =
    Fundef s qt i (tBody b)
tDef (Def s qt i e) =
    Def s qt i (tMaybeExp e)
tDef d =
    d

{- Simplify a body -}

tBody :: Body -> Body
tBody (Body ds ss) =
    case (tDef `map` ds, tStat `concatMap` ss) of
      (_, []) -> 
          Body [] []
      (ds, [Block (Body ds' ss)]) ->
          Body (ds++ds') ss
      (ds, ss) ->
          Body ds ss


-- SIMPLIFY STATEMENTS -----------------------------------------------

{- Simplify a statement when a single statement is expected (if the
   statement simplifies into a list of staments, we wrap a black
   around it. If the statement disappears, we insert a lone
   semicolon.) -}

tStat1     :: Stat -> Stat
tStat1 s = 
    case tStat s of
      [] ->
          Computation Nothing
      [x] ->
          x
      xs ->
          Block (Body [] xs)


{- Simplify a statement where any number of statements can be
   accomodated. -}

tStat :: Stat -> [Stat]

{- A lone semicolon can be left out of a list of expressions -}
tStat (Computation Nothing) = 
    []

{- A GNU block-with-a-value of which the value is ignored is just a
   block. -}
tStat (Computation (Just (GnuBody b))) =
    tStat (Block b)

{- A ternary expression of which the value is ignored can be more
   idiomatically expressed as an if statement. -}
tStat (Computation (Just (Ternary a b c))) =
    tStat (If a (computation b) (computation c))

tStat (Computation e) =
    [Computation (tMaybeExp e)]

{- If a block has no declarations, it can be spliced into the rest of a
   list of statements that it is part of. -}
tStat (Block (Body [] ss))  = 
    tStat `concatMap` ss

tStat (Block b) =
    [Block (tBody b)]

{- If the condition is a constant integer, we don't need the if
   statement. -}
tStat (If a b c) =
    case tExp a of
      LitInt 0 ->
          tStat c
      LitInt _ ->
          tStat b
      a' ->
          [If a' (tStat1 b) (tStat1 c)]

{- Returning a GNU block-with-a-value is like executing a regular
   block and returning the value of the last thing in it. If the last
   thing in it has no value, or there is nothing in it, we must add a
   "return;" in order to properly preserve the semantics here. (Piffle
   never generates code where this matters, which is why I almost got
   this case wrong...) -}

tStat (Return (Just (GnuBody body))) = 
    tStat (Block (returnify body))
    where returnify (Body ds ss)     = Body ds (returnify' ss)
          returnify' []              = [Return Nothing]
          returnify' [Computation x] = [Return x]
          returnify' (x:xs)          = x:(returnify' xs)

tStat (Return (Just (Ternary a b c))) =
    [If a (tStat1 (Return (Just b))) (tStat1 (Return (Just c)))]

{- Trivial cases -}

tStat (Return e) =
    [Return (tMaybeExp e)]

tStat (While e s) =
    [While (tExp e) (tStat1 s)]

tStat (Dowhile e s) =
    [Dowhile (tExp e) (tStat1 s)]

tStat (For a b c s) =
    [For (tMaybeExp a) (tMaybeExp b) (tMaybeExp c) (tStat1 s)]

tStat (Case e) =
    [Case (tExp e)]

tStat s =
    [s]


-- SIMPLIFY AN EXPRESSION --------------------------------------------

{- Simplify an expression -}

tExp :: Exp -> Exp

{- Because !, like the comparison and logical operators, guaranteed to
   return 0 or 1, !!x is not the same thing as x. (Did Ritchie read
   about constructive logic in his spare time and was he ashamed to
   admit it?) -}

{- CAN'T DO THIS: tExp (Unary Not (Unary Not e)) = tExp e -}

{- Negating an integer literal is quite predictable. -}
tExp (Unary Not e) =
    case tExp e of 
      LitInt 0 ->
          LitInt 1
      LitInt _ ->
          LitInt 0
      e' ->
          Unary Not e'

{- If one of the operands to && is a nonzero integer, or the first
   operand is 0, we can predict what'll happen. Unfortunately, if the
   second operand is 0, we can't do much of anything, because the
   first operand must still be evaluated for its side effects. -}

tExp (Binary And a b) =
    case (tExp a, tExp b) of
      (a, LitInt i) | i /= 0 ->
          a
      (LitInt i, b) | i /= 0 ->
          b
      (LitInt 0, _) ->
          LitInt 0
      (a,b) ->
          Binary And a b

{- Only a computer would write code like ({(x);}), but it does always
   reduce to x. -}

tExp (GnuBody (Body [] [Computation (Just e)])) =
    tExp e

tExp (GnuBody b) =
    GnuBody (tBody b)

{- Trivial cases -}

tExp (Unary o e) =
    Unary o (tExp e)
tExp (Binary o x y) =
    Binary o (tExp x) (tExp y)
tExp (Ternary x y z) =
    Ternary (tExp x) (tExp y) (tExp z)
tExp (Cast t e) =
    Cast t (tExp e)
tExp (ESize e) =
    ESize (tExp e)
tExp (Index x i) =
    Index (tExp x) (tExp i)
tExp (Dot e f) =
    Dot (tExp e) f
tExp (Arrow e f) =
    Arrow (tExp e) f
tExp e =
    e

{- Simplify an object of type Maybe Exp -}

tMaybeExp :: Maybe Exp -> Maybe Exp
tMaybeExp =
    liftM tExp
