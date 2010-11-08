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


module CommonPretty() where
import CommonAst
import PrettyUtil
import Prelude hiding(head)

-- PRETTY PRINTER ----------------------------------------------------

instance Pretty Ident where
    pretty (Ident s) =
        text s

instance Pretty Binop where
    pretty Add =
        text "+"
    pretty Sub =
        text "-"
    pretty Mul =
        text "*"
    pretty Div =
        text "/"
    pretty Mod =
        text "%"
    pretty And =
        text "&&"
    pretty Or =
        text "||"
    pretty Band =
        text "&"
    pretty Bor =
        text "|"
    pretty Xor =
        text "^"
    pretty Shl =
        text "<<"
    pretty Shr =
        text ">>"
    pretty Eq =
        text "=="
    pretty Ne =
        text "!="
    pretty Lt =
        text "<"
    pretty Gt =
        text ">"
    pretty Le =
        text "<="
    pretty Ge =
        text ">="
    pretty Assign =
        text "="
    pretty Add_assign =
        text "+="
    pretty Sub_assign =
        text "-="
    pretty Mul_assign =
        text "*="
    pretty Div_assign =
        text "/="
    pretty Mod_assign =
        text "%="
    pretty Band_assign =
        text "&="
    pretty Bor_assign =
        text "|="
    pretty Xor_assign =
        text "^="
    pretty Shl_assign =
        text "<<="
    pretty Shr_assign =
        text ">>="

instance Pretty Unop where
    pretty UPlus =
        text "+"
    pretty UMinus =
        text "-"
    pretty UNeg =
        text "~"
    pretty UNot =
        text "!"

instance Pretty Type where
    pretty Void =
        text "void"
    pretty (Atomic t) =
        pretty t
    pretty (Array i t) =
        pretty t <> brackets (pretty i)

instance Pretty AtomicType where
    pretty Bool = 
        text "bool"
    pretty U8 =
        text "u8"
    pretty U16 =
        text "u16"
    pretty U32 =
        text "u32"
    pretty S8 =
        text "s8"
    pretty S16 =
        text "s16"
    pretty S32 =
        text "s32"
    pretty SLiteral =
        text "sliteral"

instance Pretty Lit where
    pretty UnitLit =
        text "unit"
    pretty (IntLit i) =
        text (show i)

instance Pretty Inty where
    pretty i =
        text (show i)
