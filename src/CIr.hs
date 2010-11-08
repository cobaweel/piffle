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

module CIr(tFile) where
import qualified C
import Ir

-- GENERATE C CODE ---------------------------------------------------

{- This is the code generator. It generates C code as an abstract
   syntax tree. The advantage of this is that we get modular code in
   the compiler and pretty, indented C code as output; and that we can
   do some transformations on the C code before finally printing
   it. The disadvantage is that this module may be slightly harder to
   read. -}


-- FILES -------------------------------------------------------------

tFile :: File -> C.File
tFile (File ds) = 
    let ds' = tDec `map` ds in
    C.File ds'


-- DECLARATIONS ------------------------------------------------------

{- Translate IR declarations. -}

tDec :: Declaration -> C.Def
tDec (Defvar _ (Typing n t)) = 
    C.def t' n' Nothing
    where  t' = tType t
           n' = tIdent n 

tDec (Defun _ (Typing n t) ps e) = 
    C.funDef t' n' (C.Body [] [C.Return (Just e')])
    where  t' = tFuntype t ps
           n' = tIdent n
           e' = tExp e 


-- STATEMENTS AND EXPRESSIONS ----------------------------------------

{- Translate an IR expression when a statement is expected in C. -}

tStat :: Exp -> C.Stat
tStat e = C.Computation (Just (tExp e))


{- Translate an IR expression when an expression is expected in C.
   Note that the loop translations use some gensyms (generated symbols
   guaranteed to be distinct from any symbol in scope) that were
   presciently inserted into the IR code in the previous pass. -}

tExp :: Exp -> C.Exp

tExp (Literal _ _ v) =
    tLit v

tExp (Variable _ _ v) =
    C.Variable (tIdent v)

tExp (Binop _ _ x o y) =
    C.Binary (tBinop o) (tExp x) (tExp y)

tExp (Unop _ _ o x) =
    C.Unary (tUnop o) (tExp x)

tExp (Cast _ _ x t) =
    C.Cast (tType (Atomic t)) (tExp x)

tExp (Index _ _ x i) =
    C.Index (tExp x) (tExp i)

tExp (Apply _ _ f ps) =
    C.Call (C.Variable (tIdent f)) (tExp `map` ps)

{- Fortunately, GNU C allows blocks with values, so we can
   straightforwardly translate blocks. (This is a feature that Ken
   Thompson had failed to copy from BCPL because it was hard to
   implement in a one-pass compiler, and somehow to this day it hasn't
   made it into the C standard. See Dennis Ritchie, "The Development
   of the C language.") -}

tExp (Seq _ _ ds es e) =
    C.gnuBody (tDec `map` ds) (tStat `map` (es ++ [e]))

{- Piffle if statements correspond to the C ternary operator, not to
   the C if statement. In some cases they can be reduced to if
   statements, but we leave that to the CC module. -}

tExp (If _ _ a b c) =
    C.Ternary (tExp a) (tExp b) (tExp c)

{- This translation is the trickiest. It involves two gensyms that
   were supplied in the typechecker, and also a type that is
   presciently saved in the IR by the type checker.

   "for v in r while c do b", where r : t[N] ===>
    {
        uint32_t <<gensym g>>;
        t *<<gensym h>> = [[r]];
        for ( <<gensym g>> = 0; <<gensym g>> < N; <<gensym g>>++ ) {
            t v = <<gensym h>>[<<gensym g>>];
            if (! [[c]]) break;
            [[b]];
        }
    }
-}

tExp (ForIn _ _ (i, t, g, h) v r c b) = 
    C.gnuBody
         [C.def (C.namedType "uint32_t") (tIdent g) Nothing,
          C.def (C.ptr (tType t)) (tIdent h) (Just (tExp r)) ]
         [C.for (C.Binary C.Assign (C.Variable (tIdent g)) (C.LitInt 0))
               (C.Binary C.Lt (C.Variable (tIdent g)) 
                     (C.LitInt (fromIntegral i)))
               (C.Unary C.Posincr (C.Variable (tIdent g)))
               (C.block
                [C.def (tType t) (tIdent v) 
                      (Just (C.Index (C.Variable (tIdent h)) 
                                  (C.Variable (tIdent g))))]
                [C.If (C.Unary C.Not (tExp c)) C.Break C.nop,
                 C.computation (tExp b)])]

{- 
   "for v from x to y while c do b" ===>
    { 
       uint32_t v; 
       for ( v = x; (v <= y) && [[c]]; v++ ) { 
          [[b]];
       }
    }
-}
         
tExp (ForFromTo _ _ v x y c b)     = 
    C.gnuBody
         [C.def (C.namedType "uint32_t") (tIdent v) Nothing]
         [C.for (C.Binary C.Assign (C.Variable (tIdent v)) (tLit (IntLit x)))
               (C.Binary C.And
                 (C.Binary C.Le (C.Variable (tIdent v)) (tLit (IntLit y)))
                 (tExp c))
               (C.Unary C.Posincr (C.Variable (tIdent v)))
               (C.block [] [(C.Computation (Just (tExp b)))])]


{- Translate a Piffle/IR literal to C expression. The empty GnuBody as
   a translation of "unit" seems to work, and it's as good as any,
   really. XXX For now, we'll leave overflow checking on literals as
   an exercise for the C compiler. No idea if gcc actually does that
   check. XXX -}

tLit :: Lit -> C.Exp
tLit UnitLit =
    C.gnuBody [] []
tLit (IntLit i) =
    C.LitInt (fromIntegral i)


{- Translate a Piffle/IR identifier to a C identifier. -}

tIdent :: Ident -> C.Identifier
tIdent (Ident s) =
    (C.Identifier s)


-- TYPES -------------------------------------------------------------

{- Translate a Piffle/IR type to a C type. The uint8_t &c types are
   supposed to be consistently defined on all platforms in stdint.h
   (this is presumably in section 7.18 of the C99 standard, which I
   don't have access to.) Anyway, gcc defines them. -}

tType :: Type -> C.Type
tType Void =
    C.Void
tType (Atomic Bool) = 
    C.NamedType (C.TypeName "int")
tType (Atomic U8) =
    C.NamedType (C.TypeName "uint8_t")
tType (Atomic U16) =
    C.NamedType (C.TypeName "uint16_t")
tType (Atomic U32) =
    C.NamedType (C.TypeName "uint32_t")
tType (Atomic S8) =
    C.NamedType (C.TypeName "int8_t")
tType (Atomic S16) =
    C.NamedType (C.TypeName "int16_t")
tType (Atomic S32) =
    C.NamedType (C.TypeName "int32_t")

{- We shouldn't usually have to convert an "Atomic SLiteral" type when
   translating IR; that type is used by the type checker, but it
   should disappear after erasure. Then again, if we do somehow end up
   encountering one, I don't think anything would be lost by calling
   it an int. -}

tType (Atomic SLiteral) = 
    C.NamedType (C.TypeName "int")

{- XXXIs it ALWAYS correct to put the bound into the C type? I think
   it is, because Piffle only hase one-dimensional arrays. For
   multidimensional arrays, this would become trickier.XXX -}

tType (Array i t) =
    C.Array (tType (Atomic t)) (Just (C.LitInt (fromIntegral i)))


{- Translate a Piffle/Ir function type to a C type. -}

tFuntype :: Type -> [Typing] -> C.Type
tFuntype t ps =
    C.Proto (tReturnType t) (tParam `map` ps)
    where tParam (Typing n t) =
              ((C.Regular, tType t), tIdent n)
          tReturnType (Array _ t) =
              C.ptr (tType (Atomic t))
          tReturnType t =
              tType t


-- OPERATORS ---------------------------------------------------------

{- Translate operators. These happen to be mostly the same, so this
   seems a little wasteful, but I wanted to leave it in for
   flexibility in case I ever want the set of operators to be
   different in Piffle and C. -}

tBinop :: Binop -> C.Binop

tBinop Add =
    C.Add
tBinop Sub =
    C.Sub
tBinop Mul =
    C.Mul
tBinop Div =
    C.Div
tBinop Mod =
    C.Mod
tBinop And =
    C.And
tBinop Or =
    C.Or
tBinop Band =
    C.Band
tBinop Bor =
    C.Bor
tBinop Xor =
    C.Xor
tBinop Shl =
    C.Shl
tBinop Shr =
    C.Shr
tBinop Eq =
    C.Eq
tBinop Ne =
    C.Ne
tBinop Lt =
    C.Lt
tBinop Gt =
    C.Gt
tBinop Le =
    C.Le
tBinop Ge =
    C.Ge
tBinop Assign =
    C.Assign
tBinop Add_assign =
    C.AddAssign
tBinop Sub_assign =
    C.SubAssign
tBinop Mul_assign =
    C.MulAssign
tBinop Div_assign =
    C.DivAssign
tBinop Mod_assign =
    C.ModAssign
tBinop Band_assign =
    C.BandAssign
tBinop Bor_assign =
    C.BorAssign
tBinop Xor_assign =
    C.XorAssign
tBinop Shl_assign =
    C.ShlAssign
tBinop Shr_assign =
    C.ShrAssign

tUnop :: Unop -> C.Unop

tUnop UPlus =
    C.Plus
tUnop UMinus =
    C.Minus
tUnop UNeg =
    C.Bnot
tUnop UNot =
    C.Not

