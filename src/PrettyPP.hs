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

-- PRETTY-PRINTING PRETTY THINGS -------------------------------------

module PrettyPP(pp,ppColor) where
import PrettyUtil

{- Pretty-print something in 78-column format, so that it can be read
   on a standard terminal. -}
pp :: Pretty p => p -> String
pp p =
    (displayS (renderPretty ribbon lineWidth (pretty p))) ""

{- Pretty-print something in 78-column format, with ANSI escape codes
   used for color iff the first argument is True. -}
ppColor :: Pretty p => Bool -> p -> String
ppColor inColor p =
    (displayS (renderColorfully inColor ribbon lineWidth (pretty p))) ""

lineWidth = 78
ribbon = 0.8


