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


module PiffleParse ( reader ) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import PiffleAst
import Prelude
import Compiler
import Control.Monad.Error

-- PIFFLE PARSER -----------------------------------------------------

{- This file contains the code for parsing a piffle source file and
   turning it into an AST object in memory. -}


-- DRIVER ------------------------------------------------------------

{- The main entry point to the parser. Parse the contents of a file. -}
reader :: String -> String -> Compiler File
reader text filename = 
    case parse file filename text of
      Right file ->
          return file
      Left error ->
          do f <- fault error
             throwError f


-- SCANNER -----------------------------------------------------------

{- XXX Is it really necessary to declare all these things as reserved
   names? In particular, type names need not be reserved. It's not C,
   after all. But then again, why would you want a variable named
   "bool"? Also, there may be some halfway decent arguments for
   disallowing nested comments. For example, cpp doesn't like them,
   and do we want a program suddenly failing when somebody decides to
   run it through cpp when it worked fine before?. Questions,
   questions... Is there a difference between reservedNames and
   reservedOpNames in a language that does not allow custom operators
   and in which no operator would ever make a valid identifier? XXX -}

lexer = P.makeTokenParser $ emptyDef
         { commentStart   = "/*", 
           commentEnd     = "*/",
           commentLine    = "//",
           nestedComments = True,
           identStart     = letter <|> oneOf "_",
           identLetter    = alphaNum <|> oneOf "_",
           reservedNames  = [  "if",  "then", "else", 
                               "for", "in", "from", "to", "while", 
                               "do", "var", "fun",
                               "unit", "true", "false",
                               "void", "bool", "u8", "u16", "u32", 
                               "s8", "s16", "s32" ],
           reservedOpNames= [ "+", "-", "*", "/", "%", "&&", "||",
                              "&", "|", "^", "<<", ">>", "==", "!=",
                              "<", ">", "<=", ">=", "=", "+=", "-=",
                              "*=", "/=", "%=", "&=", "|=", "^=",
                              "<<=", ">>=", "!", "~", ":" ],
           caseSensitive  = True } 

-- Abbreviate basic lexemes
identifier =
    P.identifier lexer
whiteSpace =
    P.whiteSpace lexer
commaSep =
    P.commaSep lexer
parens =
    P.parens lexer
braces =
    P.braces lexer
brackets =
    P.brackets lexer
reserved =
    P.reserved lexer
reservedOp =
    P.reservedOp lexer
semi =
    P.semi lexer
natural =
    P.natural lexer



-- SOME HELPER FUNCTIONS FOR THE PARSER ------------------------------

atPosition f =
    do p <- getPosition; f p

{- In parsec, the concept of one thing following another is always
   expressed by the monadic operator (>>=). This is nice and elegant
   and all, but I prefer to define some of my own operators for common
   cases, so that the grammar can be shorter. -}

{-
  p .<>+ q =
    do e <- p; q; return e
-}

q +<>. p =
    do q; e <- p; return e -- same as >>

k  <>. p =
    reserved k +<>. p
{-
p .<>  k =
    p .<>+ reserved k
-}

o ~<>. p =
    reservedOp o +<>. p

{-
p .<>~ o =
    p .<>+ reservedOp o
-}

s `atom`  u =
    do reserved s; return u

{- This is terribly inefficient but the alternative is a mess XXX -}
allLast :: a -> [a] -> ([a], a)
allLast zero []  = 
    ([],zero)
allLast _ lst = 
    (init lst, last lst)



-- PARSER ------------------------------------------------------------

{- This is where we can later add symbol table code if needed. I don't
   think it's needed, though. The compiler seems to run plenty fast
   simply using Strings for identifiers. -}
symbol =
    do i <- identifier
       return (Ident i)
    <?> "identifier"

{- Read an integer literal, but complain if it overflows the 64-bit
   datatype that we keep those in. XXXThis is uglyXXX -}
inty = 
    do iInteger <- natural
       let iInty = fromIntegral iInteger :: Inty 
           iIntyInteger = fromIntegral iInty :: Integer in
         if iInteger == iIntyInteger then
             return iInty
         else 
             fail "integer overflow"
    <?> "natural number"

-- PARSE AN EXPRESSION -----------------------------------------------

expression = buildExpressionParser table exp1
    where
      table =
          [[ una "+" UPlus, una "-" UMinus, una "!" UNot, una "~" UNeg ],
           [ binl "*" Mul, binl "/" Div, binl "%" Mod],
           [ binl "+" Add, binl "-" Sub],
           [ binl "<<" Shl, binl ">>" Shr],
           [ binl "<" Lt, binl "<=" Le, binl ">" Gt, binl ">=" Ge],
           [ binl "==" Eq, binl "!=" Ne],
           [ binl "&" Band], 
           [ binl "^" Xor],
           [ binl "|" Bor],
           [ binl "&&" And],
           [ binl "||" Or],
           [ binr "=" Assign, binr "+=" Add_assign, binr "-=" Sub_assign, 
             binr "*=" Mul_assign, binr "/=" Div_assign, binr "%=" Mod_assign, 
             binr "&=" Band_assign, binr "|=" Bor_assign, binr "^=" Xor_assign,
             binr "<<=" Shl_assign, binr ">>=" Shr_assign]]
      binr =
          bin AssocRight
      binl =
          bin AssocLeft
      bin dir name fun =
          Infix (atPosition $ \p ->
                 do reservedOp name
                    return (\x y -> Binop p x fun y)) dir
      una name fun =
          Prefix (atPosition $ \p ->
                  do reservedOp name
                     return (\x -> Unop p fun x)) 

exp1 =
    atPosition $ \p ->
    do e <- exp2
       e <- option e (do i <- brackets expression
                         return (Index p e i)
                      <?> "array index")
       e <- option e (do t <- ":" ~<>. atomictype
                         return (Cast p e t)
                      <?> "type cast")
       return e

exp2 =
    atPosition exp3 

exp3 p =
    (eUnit <?> "unit") <|>
    (eBool <?> "boolean") <|>
    (eInt <?> "integer") <|> 
    (eSeq <?> "block") <|> 
    (eIf <?> "if-expression") <|> 
    (eFor <?> "for-expression") <|> 
    eSymbol <|>
    (eParens <?> "parenthesized-expression")
    where eUnit =
              "unit" `atom` (Literal p UnitLit)
          eBool = 
              "true" `atom` (Literal p (IntLit 1)) <|>
              "false" `atom` (Literal p (IntLit 0))
          eInt =
              do n <- inty
                 return (Literal p (IntLit n))
          eSeq =              
              braces (do ds <- declaration `endBy` semi
                         es <- expression  `endBy` semi
                         let (xs, x) = allLast (Literal p UnitLit) es in
                          return (Seq p ds xs x))
          eIf =          
              do a <- "if" <>. expression
                 b <- "then" <>. expression
                 c <- unitOr ("else" <>. expression)
                 return (If p a b c)
          eFor =
              do x <- "for" <>. symbol
                 ((do a <- "in" <>. expression
                      c <- trueOr ("while" <>. expression)
                      b <- "do" <>. expression
                      return (ForIn p x a c b)) <|>
                  (do a0 <- "from" <>. inty
                      a1 <- "to" <>. inty
                      c  <- trueOr ("while" <>. expression) 
                      b  <- "do" <>. expression
                      return (ForFromTo p x a0 a1 c b)))
          eSymbol =
              do n <- symbol
                 ((do a <- parens (commaSep expression) <?> "function args"
                      return (Apply p n a)) <|>
                  (do return (Variable p n)))
          eParens =
              parens expression
          trueOr other =
              atPosition $ \p -> Literal p (IntLit 1) `option` other
          unitOr other =
              atPosition $ \p -> Literal p UnitLit    `option` other


-- PARSE A DECLARATION -----------------------------------------------

declaration = atPosition $ \p ->
              (do t <- "var" <>. typing
                  return (Defvar p t)
               <?> "variable declaration") <|>
              (do f <- "fun" <>. symbol
                  a <- parens (commaSep typing) 
                  t <- option Void (":" ~<>. atype)
                  e <- "=" ~<>. expression
                  return (Defun p (Typing f t) a e)
               <?> "function declaration")
    where
      typing = do v <- symbol
                  t <- ":" ~<>. atype
                  return (Typing v t)
      atype  = ("void" `atom` Void) <|>
               (do t <- atomictype
                   ((do i <- brackets inty
                        return (Array i t)) <|>
                    (do return (Atomic t))))
               <?> "type"

atomictype =
    "bool" `atom` Bool <|>
    "u8" `atom` U8 <|> "u16" `atom` U16 <|> "u32" `atom` U32 <|>
    "s8" `atom` S8 <|> "s16" `atom` S16 <|> "s32" `atom` S32 <?>
    "atomic type"


-- PARSE AN ENTIRE FILE ----------------------------------------------
                   
file =
    do whiteSpace
       d <- declaration `endBy` semi
       eof
       return (File d)
