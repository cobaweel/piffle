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

module CommonUtil where
import Data.Bits
import Common
import Compiler

-- IMPLEMENTATION OF OPERATORS  --------------------------------------

{- This is an implementation of all the Piffle operators, which can be
   used inside the compiler to reduce constant expressions. As
   mentioned in the Piffle report, all arithmetic on constant
   expressions, that is expressions composed only of literals of which
   the value can be statically determined, is done by the compiler
   using 64-bit signed integers. -}

{- XXX check bounds on second argument of Shl, Shr ??? -}

operate :: Binop -> Lit -> Lit -> Compiler Lit 
operate op (IntLit x) (IntLit y) =
    do z <- operate' op x y
       return (IntLit z)
    where operate' Add x y =
              return $ x + y
          operate' Sub x y =
              return $ x - y
          operate' Mul x y =
              return $ x * y
          operate' Div _ 0 =
              die "divide by 0"
          operate' Div x y =
              return $ x `div` y -- XXX / or div???
          operate' Mod x y =
              return $ x `mod` y
          operate' And _ 0 =
              return $ 0 
          operate' And 0 _ =
              return $ 0
          operate' And _ _ =
              return $ 1
          operate' Or 1 _ =
              return $ 1
          operate' Or _ 1 =
              return $ 1
          operate' Or _ _ =
              return $ 0
          operate' Band x y =
              return $ x .&. y
          operate' Bor x y =
              return $ x .|. y
          operate' Xor x y =
              return $ x `xor` y
          operate' Shl x y =
              return $ x `shiftL` (fromIntegral y :: Int)
          operate' Shr x y =
              return $ x `shiftR` (fromIntegral y :: Int)
          operate' Eq x y =
              return $ if x == y then 1 else 0
          operate' Ne x y =
              return $ if x /= y then 1 else 0
          operate' Lt x y =
              return $ if x < y  then 1 else 0
          operate' Gt x y =
              return $ if x > y  then 1 else 0
          operate' Le x y =
              return $ if x <= y then 1 else 0
          operate' Ge x y =
              return $ if x >= y then 1 else 0
          operate' _ _ _ =
              die "invalid binary operation"
operate _ _ _ =
    die "invalid use of binary operation"

unoperate :: Unop -> Lit -> Compiler Lit
unoperate op (IntLit x) =
    do z <- unoperate' op x 
       return (IntLit z)
    where unoperate' UPlus x =
              return $ x
          unoperate' UMinus x =
              return $ -x
          unoperate' UNeg x =
              return $ complement x
          unoperate' UNot x =
              return $ if x == 0 then 1 else 0
unoperate _ _ =
    die "invalid unary operation"

                  


