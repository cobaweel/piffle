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

module Configure (Config(..), configure) where
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import Control.Monad.Error
import Control.Monad.Identity

-- PARSE COMMAND LINE ------------------------------------------------

{- The function configure takes a list of command line options and
   returns a structure of type Config that contains all the
   configuration information specified on the command line, or
   alternatively a string containing an error message explaining the
   problem. -}

{- The debug level is an integer that is 99 by default. You can set it
   to 0 with the -d flag, or to some arbitrary integer i with the -di
   flag. All log (warning, debug, &c) messages are logged at a debug
   level. -}

data Config = Config { help :: Bool,
                       color :: Bool,
                       debug :: Integer,
                       cpu, mem :: Maybe Integer,
                       file :: String,
                       boilerplate :: Maybe String,
                       argv :: [String]
                     }
               deriving (Show, Eq)

configure :: [String] -> Either String Config
configure = 
    runIdentity . runErrorT . configure'

configure' :: [String] -> ErrorT String Identity Config
configure' argv = 
    case getOpt Permute optionSpec argv of
      (opts, [file], []) ->
          let conf  = Config { help = False,
                               color = False,
                               debug = 99,
                               cpu = Nothing,
                               mem = Nothing,
                               file = file,
                               boilerplate = Nothing,
                               argv = argv } in
          do conf <- foldM handleOpt conf opts
             when (help conf) (fail (usage "usage: "))
             return conf
      (_, _, []) ->
          fail (usage "need exactly one file name to compile")
      (_, _, errs) ->
          fail (usage (unlines errs))
    where
      handleOpt conf Help =
          return (conf { help = True })
      handleOpt conf Color =
          return (conf { color = True })
      handleOpt conf (Debug i) =
          return (conf { debug = i })
      handleOpt conf (Boiler s) =
          return (conf { boilerplate = Just s })
      handleOpt _ (Cpu (Nothing)) = 
          fail "cpu bound malformed"
      handleOpt conf (Cpu (Just i)) =
          return (conf { cpu = Just i })
      handleOpt _ (Mem (Nothing)) = 
          fail "mem bound malformed"
      handleOpt conf (Mem (Just i)) =
          return (conf { mem = Just i })
      usage s =
          usageInfo (s ++ "\n pfc filename [ flags ]") optionSpec

data Flag = Help
          | Color
          | Debug Integer
          | Boiler String
          | Cpu (Maybe Integer)
          | Mem (Maybe Integer)
            deriving (Show, Eq)

optionSpec :: [ OptDescr Flag]
optionSpec =
    [ Option "h" ["help"] (NoArg Help) "this message",
      Option "d" ["debug"] 
      (OptArg (Debug . fromMaybe 0 . readMaybe . fromMaybe "0") "") 
      "verbosity level",
      Option "a" ["ansicolors"] (NoArg Color) "colored output for easy debugging",
      Option "B" ["boiler"] (ReqArg Boiler "") "boilerplate",
      Option "C" ["cpu"] (ReqArg (Cpu . readMaybe) "") "cpu bound",
      Option "M" ["mem"] (ReqArg (Mem . readMaybe) "") "memory bound" ]

readMaybe   :: Read x => String -> Maybe x
readMaybe s = 
    case map fst (reads s) of
      [] -> 
          Nothing
      x:_ ->
          Just x


                         
