-- Position.hs: positions in source files

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


module Position(
                Pos, 
                fakePos,
                posToXY
               ) where
import Text.ParserCombinators.Parsec.Pos
import PrettyUtil

-- SOURCE POSITIONS --------------------------------------------------

{- For now, I will simply use the Parsec source positions, but if I
   want them to be more complicated (it might be nice, in particular,
   to use ranges as opposed to points) this is where the additional
   code would go. -}

type Pos = SourcePos

instance Pretty Pos where
    {- This is the format understood by GNU Emacs, so stick to it. -}
    pretty p = 
        text (sourceName p) <> colon <> pretty y <> colon <> pretty x
        where (x,y) =
                  posToXY p

{- Extract a (column,line) coordinate from an object of type Pos. -}

posToXY :: Pos -> (Int, Int)
posToXY p =
    (sourceColumn p - 1, sourceLine p - 1)

{- When desperatately in need of an object of type Pos, but without
   any meaningful data to construct one out of, use fakePos. -}

fakePos :: String -> Pos
fakePos s =
    newPos s 1 1



