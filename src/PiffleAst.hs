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

module PiffleAst(
                 Exp(..),
                 Declaration(..),
                 File(..),
                 module Common
                ) where
import Position
import Common

-- PIFFLE AST --------------------------------------------------------

{- There's more in Common.hs, where those elements of the AST are
   defined that Piffle has in common with the IR language. -}

{- Expressions -}
data Exp = Literal Pos Lit
         | Variable Pos Ident
         | Binop Pos Exp Binop Exp
         | Unop Pos Unop Exp
         | Cast Pos Exp AtomicType
         | Index Pos Exp Exp
         | Apply Pos Ident [Exp]
         | Seq Pos [Declaration] [Exp] Exp
         | If Pos Exp Exp Exp
         | ForIn Pos Ident Exp Exp Exp
         | ForFromTo Pos Ident Inty Inty Exp Exp
           deriving (Show)

{- Declarations -}
data Declaration = Defvar Pos Typing 
                 | Defun Pos Typing [Typing] Exp
                   deriving (Show)

{- An entire source file -}
data File = File [Declaration]
            deriving(Show)


