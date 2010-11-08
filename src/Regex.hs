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

-- SLIGHTLY WRAPPED REGEX FUNCTIONS ----------------------------------

module Regex where
import Text.Regex
import Data.Maybe

{- Search and replace, more or less like UNIX sed. -}

sed :: String -> String -> String -> String
sed regex =
    subRegex (mkRegex regex)


{- Grep for a given regex, and return a list of results. Why on earth
   the regex library chooses to use "Maybe [String]" as the return
   type of matchRegex, when [] is a perfectly adequate representation
   of their being no match, is beyond me. -}

grep :: String -> String -> [String]
grep regex =
    fromMaybe [] . matchRegex (mkRegex regex)


{- Grep for a given regex, but only return a Boolean saying whether it
   was found. -}

grepB :: String -> String -> Bool
grepB regex =
    isJust . matchRegex (mkRegex regex)


