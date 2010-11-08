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


module IrAst(
             Exp(..),
             Declaration(..),
             File(..),
             module Common
            ) where

import Common
import Position

-- PIFFLE INTERMEDIATE REPRESENTATION AST ----------------------------

{- There's more in Common.hs ... -}

{- Expressions -}

data Exp = Literal { pos :: Pos, info :: Type, 
                     val :: Lit }
         | Variable { pos :: Pos, info :: Type, 
                      var :: Ident }
         | Binop { pos :: Pos, info :: Type, 
                   left :: Exp, binop :: Binop, right :: Exp }
         | Unop { pos :: Pos, info :: Type,
                  unop :: Unop, right :: Exp }
         | Cast { pos :: Pos, info :: Type,
                  hd :: Exp, ty :: AtomicType }
         | Index { pos :: Pos, info :: Type,
                   hd :: Exp, index ::  Exp }
         | Apply { pos :: Pos, info :: Type,
                   fun :: Ident, argl :: [Exp] }
         | Seq { pos :: Pos, info :: Type,
                 ds :: [Declaration], es :: [Exp], e :: Exp }
         | If { pos :: Pos, info :: Type,
                condition, consequent, alternative :: Exp }
         | ForIn { pos :: Pos, info :: Type, gen :: (Inty, Type, Ident, Ident), 
                   var :: Ident, range :: Exp, condition, block :: Exp }
         | ForFromTo { pos :: Pos, info :: Type,
                       var :: Ident, lb, ub :: Inty, condition, block :: Exp }
           deriving (Show)

{- Declarations -}
data Declaration = Defvar { dpos :: Pos, 
                            typing :: Typing }
                 | Defun { dpos :: Pos, 
                           typing :: Typing, params :: [Typing], init :: Exp }
                   deriving (Show)

{- An entire source file -}
data File = File [Declaration]
            deriving(Show)
