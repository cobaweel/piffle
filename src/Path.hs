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

module Path(
            readFileInSearchPaths, 
            searchInSearchPath, 
            getSearchPath,
            parseSearchPath
           ) where
import Control.Monad
import Data.List (break)
import Data.Maybe (fromMaybe)
import System.Posix.Env
import System.Posix.Files

-- SEARCH PATHS ------------------------------------------------------

{- This module contains some utilities for dealing with standard(ish)
   UNIX search paths, such as the $PATH variable. XXX It module would
   be simpler and more reliable if it used System.FilePath, but that
   library is not available in ghc 6.4; when I get around to upgrading
   to ghc 6.6 I will switch to using System.FilePath. -}

{- IT IS IMPORTANT TO NOTE THAT FOR THIS LIBRARY, A PARSED PATH MAY
   STILL CONTAIN DIRECTORIES WITHOUT TRAILING SLASHES. TRAILING
   SLASHES ARE ADDED WHEN THE PATH IS ACTUALLY USED. -}

{- Read the file named fn, in the search path specified in environment
   variable envVariable, or else in the search path specified. -}
readFileInSearchPaths :: String -> [String] -> String -> IO String
readFileInSearchPaths fn searchPath envVariable =
    do envPath <- getSearchPath envVariable
       fn <- searchInSearchPath fn (envPath ++ searchPath)
       readFile fn

{- Search for filename in the given search path, and if it is found,
   return path to the file. If not found, fail (in IO). -}
searchInSearchPath :: String -> [String] -> IO String
searchInSearchPath filename searchPath =
    case searchPath of
      [] ->
          fail ("file " ++ filename ++ " not found")
      x:xs ->
          let fullFileName = normalizePath x ++ filename in
          do exist <- fileExist fullFileName
             if exist then
                 return fullFileName
              else
                 searchInSearchPath filename xs

{- Retrieve an environment variable and parse it as a search
   path. Empty elements are interpreted as referring to the current
   working directory. All elements are delivered with trailing
   slash. -}
getSearchPath :: String -> IO [String]
getSearchPath envVariable =
    do path <- getEnv envVariable
       return (parseSearchPath (fromMaybe "" path))

{- This parses a standard UNIXy search path of directories separated by
   colons. Note that empty elements are included.  -}
parseSearchPath :: String -> [String]
parseSearchPath path = 
    case break (== ':') path of
      ("", "") ->
          []
      (element, "") ->
          [element]
      (element, _:rest) ->
          element:(parseSearchPath rest)

{- Add a / to the end of a path if there isn't one. Interpret the
   empty path as "./" (this is a bit arbitrary, but does the right thing
   in many cases.)  -}
normalizePath :: String -> String
normalizePath "" = 
    "./" 
normalizePath path = 
    if maybeLast path == Just '/' then 
        path
    else
        path ++ "/"

{- Data.List really ought to contain this function. I don't like
   unsafe functions like 'last'. They make me have to actually
   manually check invariants. Bletch. -}
maybeLast :: [t] -> Maybe t
maybeLast [] = 
    Nothing
maybeLast xs =
    Just (last xs)
