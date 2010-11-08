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

module CommonAst( 
                 Ident(..), 
                 Inty, 
                 Lit(..), 
                 Typing(..), 
                 Type(..),
                 AtomicType(..), 
                 Binop(..), 
                 Unop(..)
                ) where
import Data.Int

-- COMMON SYNTAX ELEMENTS  -------------------------------------------

{- These are syntax elements that are identical in both the Piffle AST
   and the intermediate representation. They are in this separate
   module, because that way I do not have to write boring do-nothing
   translation code between, say, literals and literals. -}

{- Identifiers are just strings. There is no good reason, given the
   architecture of this compiler, to have a symbol table. -}
data Ident = Ident String
             deriving (Ord, Eq)
instance Show Ident where
    show (Ident s) = s

{- Inty is an integer as used internally by the compiler. I decided to
   use Int64 and not Integer, because on the off chance that anyone
   would want to implement a Piffle compiler in C, it would be nasty
   if they had to do arbitrary precision arithmetic for
   compatibility. -}
type Inty = Int64

{- Integer literals -}
data Lit = UnitLit
         | IntLit Inty
           deriving(Show)

{- Identifier-type association -}
data Typing = Typing Ident Type
              deriving(Show)

{- Types -}
data Type = Void
          | Atomic AtomicType
          | Array Inty AtomicType
            deriving (Show, Eq)

{- Atomic types. The SLiteral type is for internal use only; it gets
   assigned to integer literals. -}
data AtomicType = Bool | U8 | U16 | U32 | S8 | S16 | S32 | SLiteral
                  deriving(Show, Eq)


{- Binary operators (subset of C) -}
data Binop = Add | Sub | Mul | Div | Mod | And | Or | Band | Bor | Xor 
           | Shl | Shr | Eq | Ne | Lt | Gt | Le | Ge | Assign
           | Add_assign | Sub_assign | Mul_assign | Div_assign 
           | Mod_assign | Band_assign | Bor_assign | Xor_assign
           | Shl_assign | Shr_assign
             deriving (Show, Eq)

{- Unary operators (subset of C) -}
data Unop = UPlus | UMinus | UNeg | UNot
            deriving (Show, Eq)

