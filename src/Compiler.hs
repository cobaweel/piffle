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

-- COMPILER MONAD ----------------------------------------------------

{- This module does various sorts of behind-the-scenes heavy lifting
   in the form of a monad in which all compiler passes are run. This
   includes including logging, keeping track of the position in the
   source, and recording error messages. -}

module Compiler (
                 runCompiler,
                 prettyFaultWithQuote, 
                 prettyLogMessages,
                 Compiler, 
                 fault,
                 die,
                 logMessage,
                 thisPosition,
                 whatPosition,
                 getConfig
                ) where
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Text.ParserCombinators.Parsec.Error
import Position
import Configure
import PrettyUtil
import PrettyPP

-- ERROR MONAD -------------------------------------------------------

type LoggingT = WriterT [LogEntry]
type FaultyT = ErrorT Fault
type PositionalT = StateT Pos
type ConfiguredT = ReaderT Config

type Compiler a = 
    ConfiguredT (PositionalT (FaultyT (LoggingT Identity))) a

runCompiler :: Pos -> Config -> Compiler a -> (Either Fault a, [LogEntry])
runCompiler initialPos config compiler =  
    let configured = runReaderT compiler config in
    let positional = runStateT configured initialPos in
    let faulty = runErrorT positional in
    let logging = runWriterT faulty in
    let (output, log) = runIdentity logging in
    (either Left (Right . fst) output, log)

data Fault = Fault { pos :: Maybe Pos,
                     msg :: [String] }

instance Error Fault where
    noMsg =
        Fault { pos = Nothing, msg = [] }
    strMsg s =
        Fault { pos = Nothing, msg = [s] }

thisPosition :: Pos -> Compiler ()
thisPosition =
    put

whatPosition :: Compiler Pos
whatPosition =
    get

logMessage :: Integer -> [Doc] -> Compiler ()
logMessage i ss =
    tell (LogEntry i `map` ss)

getConfig :: Compiler Config
getConfig =
    ask

-- die :: String -> Compiler ()
die str =
    do f <- fault str
       throwError f

-- CREATING ERRORS ---------------------------------------------------

class Faultable e where
    fault :: e -> Compiler Fault

instance Faultable ParseError where
    fault e = 
        return (Fault { pos = Just (errorPos e),
                        msg = tail (lines (show e)) })

instance Faultable String where
    fault s = 
        do p <- whatPosition
           return (Fault { pos = Just p, msg = [s] })


-- DISPLAYING ERROR MESSAGES -----------------------------------------

instance Show Fault where
    show =
        pp

instance Pretty Fault where
    pretty (Fault { pos = Nothing, msg = ss }) = 
        colored 31 (text "pfc:" </> align (vcat (text `map` ss)))
    pretty (Fault { pos = Just p, msg = ss }) = 
        colored 31 (pretty p <> colon </> align (vcat (text `map` ss)))

{- Given the full text of a source file, and an error, print the error
   along with a quote from the textfile and a little arrow-like thing
   pointing at the trouble spot. -}

prettyFaultWithQuote :: String -> Fault -> Doc
prettyFaultWithQuote _ f@(Fault { pos = Nothing }) = 
    pretty f <> line
prettyFaultWithQuote t f@(Fault { pos = Just p }) = 
    pretty f <$>
    case posToXY p of
      (0,0) ->
          empty
      (x,y) ->
          indent 2 (align (text (lines t !! y) <$>
                           indent x (colored 31 $ text "^"))) <>
          line

-- LOG MESSAGES ------------------------------------------------------

data LogEntry = LogEntry Integer Doc

prettyLogMessages :: Integer -> [LogEntry] -> Doc
prettyLogMessages i ms = 
    vjam (showLogMessage i `map` ms)
    where
      showLogMessage i (LogEntry j s)
          | i < j =
              s
          | otherwise =
              empty
          


