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

module PifflePretty where
import PiffleAst
import PrettyUtil
import Prelude hiding(head)


-- PRETTY PRINTER ----------------------------------------------------

{- This is the code required to print out a piece of Piffle code based
   on the AST in a legible, indented format. Because the AST is an
   *abstract* syntax tree, and not a plain parse tree, the process of
   parsing is not entirely reversible. For example, the compiler
   "forgets" whether you use parentheses in expressions or rely on
   precedence, and the pretty-printer will liberally sprinkle
   parentheses throughout expressions that have operators in
   them. Still, the pretty-printer is a very useful tool for
   debugging, and because of the PPrint combinator library, it's also
   very straightforward, which is why you will not many find comments
   in the pretty-printing code. -}

instance Pretty Exp where
    pretty (Literal _ i) = 
        pretty i
    pretty (Variable _ v) = 
        pretty v 
    pretty (Binop _ a op b) = 
        parens (pretty a <+> pretty op <+> pretty b)
    pretty (Unop _ op a) = 
        pretty op <> pretty a
    pretty (Cast _ e t) = 
        parens (pretty e <+> colon <+> pretty t)
    pretty (Index _ a b) = 
        pretty a <> brackets (pretty b)
    pretty (Apply _ f a) = 
        pretty f <> commad (pretty `map` a)                    
    pretty (Seq _ ds es e) = 
        braceblock (vcat ((pretty `map` ds) ++
                          (((<>semi) . pretty) `map` (es ++ [e]))))
    pretty (If _ a b c) = parens $ align $
        text "if" <+> pretty a <+> pretty "then" <+>
        pretty b <+> text "else" <+> pretty c
    pretty (ForIn _ i a c b) = parens $ align $
        text "for" <+> pretty i <+> text "in" <+> pretty a <+>
        text "while" <+> pretty c <+> text "do" <+> pretty b
    pretty (ForFromTo _ i x0 x1 c b) = parens$
        text "for" <+> pretty i <+> text "from" <+> pretty x0 <+> 
        text "to" <+> pretty x1 <+> text "while" <+> pretty c <+> 
        text "do" <+> pretty b

instance Pretty Declaration where
    pretty (Defvar _ t) = 
        text "var" <+> pretty t <> semi
    pretty (Defun _ (Typing f t) ts e) = 
        text "fun" <+> pretty f <+> commad (pretty `map` ts) <+> 
        colon <+> pretty t <+> text "=" <+> pretty e <> semi <> line

instance Pretty Typing where 
    pretty (Typing v t) = pretty v <+> text ":" <+> pretty t

instance Pretty File where
    pretty (File ds) = (vcat $ pretty `map` ds) <> line

