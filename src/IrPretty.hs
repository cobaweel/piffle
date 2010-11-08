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


module IrPretty() where
import IrAst
import PrettyUtil
import Prelude hiding(head)

-- PRETTY PRINTER FOR INTERMEDIATE REPRESENTATION --------------------

instance Pretty Exp where
    pretty (Literal _ info i) = 
        pretty i <>
        coloredType info
    pretty (Variable _ info v) = 
        pretty v <>
        coloredType info
    pretty (Binop _ info a op b) = 
        parens (align $ pretty a <+> pretty op </> pretty b) <>
        coloredType info
    pretty (Unop _ info op a) = 
        parens (align $ pretty op <> pretty a) <>
        coloredType info
    pretty (Cast _ info e t) = 
        parens (align $ pretty e <+> colon <+> pretty t) <>
        coloredType info
    pretty (Index _ info a b) = 
        parens (align $ pretty a <> brackets (pretty b)) <>
        coloredType info
    pretty (Apply _ info f a) = 
        pretty f <> commad (pretty `map` a) <>
        coloredType info
    pretty (Seq _ info ds es e) = 
        braceblock (vcat (pretty `map` ds) <$>
                    vcat ((<>semi) `map` (pretty `map` (es ++ [e])))) <>
        coloredType info
    pretty (If _ info a b c) = 
        parens (align $
                text "if" <+> pretty a <+> pretty "then" <$>
                tab (pretty b) <$> text "else" <$> tab (pretty c) )<> 
        coloredType info
    pretty (ForIn _ info g i a c b) = 
        parens ( text "for" <+> pretty i <+> text "in" <+> pretty a <+>
                 text "while" </> pretty c <+> text "do" <$> tab (pretty b) )<$> 
        coloredType info <> dot <> (pretty g)
    pretty (ForFromTo _ info i x0 x1 c b) =
        parens ( text "for" </> pretty i <+> text "from" </> pretty x0 <+> 
                 text "to" </> pretty x1 <+> text "while" </> pretty c <+> 
                 text "do" <$> tab (pretty b) ) <$>
        coloredType info

instance Pretty Declaration where
    pretty (Defvar _ t) = 
        text "var" <+> pretty t <> semi
    pretty (Defun _ (Typing f t) ts e) = 
        text "fun" <+> pretty f </> commad (pretty `map` ts) <+> 
        colon </> pretty t <+> text "=" </> pretty e <> semi <> line

instance Pretty Typing where 
    pretty (Typing v t) = pretty v <+> text ":" <+> pretty t

instance Pretty File where
    pretty (File ds) = (vcat $  pretty `map` ds) <> line

coloredType t = sgr 36 <> text "." <>pretty t <> sgr 0 
