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

-- EXTENSIONS OF THE PPRINT PRETTY-PRINTER LIBRARY -------------------


module PrettyUtil(
                  module Syntax.PPrint, 
                  tab, 
                  commad,
                  braceblock
                 ) where
import Syntax.PPrint


-- Indent something by four spaces. 
tab :: Doc -> Doc
tab x =
    indent 4 x


-- A typical K&R block in braces
braceblock :: Doc -> Doc
braceblock x =
    lbrace <$> tab x <$> rbrace

{- commad is like the built-in tupled, but with a more conventional
   layout -}

commad :: [Doc] -> Doc
commad ds =
    parens (align (commad' ds))
    where commad' [] =
              empty
          commad' [x] =
              x
          commad' (x:xs) =
              x <> text ", " <//> commad' xs


instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a,b,c,d) where
    pretty (a,b,c,d) =
        commad [pretty a, pretty b, pretty c, pretty d]
