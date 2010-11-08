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

-- C ABSTRACT SYNTAX -------------------------------------------------

{- This file is adapted from an earlier compiler I wrote for the BlooP
   and FlooP languages from Douglas Hofstadter's book Goedel, Escher,
   Bach. It is originally adapted using Emacs macros from an ML
   program called FrontC by Hugues Cassé (released under GPL; thanks
   for sharing, Hugues!). -}


module C(
         Identifier(..), 
         TypeName(..), 
         StructName(..), 
         UnionName(..),
         EnumName(..), 
         LabelName(..), 
         GotoTarget(..), 
         FieldName(..),
         File(..), 
         Def(..), 
         Type(..), 
         Storage(..), 
         Qualified,
         Qual(..), 
         Body(..),
         Stat(..), 
         Exp(..), 
         Binop(..), 
         Unop(..), 
         funDef, 
         def, 
         ptr,
         namedType, 
         block, 
         computation, 
         nop, 
         for, 
         gnuBody
        ) where


-- NAMES OF THINGS ---------------------------------------------------

{- The reason these newtypes are defined here is that this way,  we can
   distinguish between the various sorts of names used in a C program
   for the purposes of "scrap your boilerplate" abstract tree-walking
   algorithms. (see Ralf Laemmel's papers) -}

newtype Identifier = Identifier String 
newtype TypeName   = TypeName   String 
newtype StructName = StructName String 
newtype UnionName  = UnionName  String 
newtype EnumName   = EnumName   String 
newtype LabelName  = LabelName  String 
newtype GotoTarget = GotoTarget String 
newtype FieldName  = FieldName  String 


-- AST ---------------------------------------------------------------

{- This an AST that should cover all of ANSI C, as well as a few GNU
   features. This being an _abstract_ syntax tree, C programs that
   consist of different sequences of tokens but have the same meaning
   may in some cases have the same AST. The drawback of this is that
   it is not always immediately obvious how the AST relates to actual
   C code. When in doubt, consult the pretty printer code.

   Note that exactly where you draw the line between syntax and
   semantics is a little arbitrary in a language as complex as C; as
   John McCarthy said, "there is no such thing as syntax"; as Noam
   Chomsky said, "colorless green ideas sleep furiously"; as Lewis
   Caroll said, "all mimsy were the borogoves"; and so on. This
   particular AST is more abstract than most, which complicates the
   pretty printer as well as any parser that might be written for it,
   but simplifies everything else. -}


-- TYPES AND DECLARATIONS --------------------------------------------

{- Types and declarations are a big mess in C. The idea here is that
   we express the conceptual structure of a declaration in the AST,
   and not the twisted way that it happens to be written down.

     * In C declarations, part of the type goes before the identifier,
       and part comes after it. This allows abuses of notation where
       identifiers can be declared on a single line if the part of
       their type that happens to come after the identifier is
       different, as long as the part that happens to come before is
       identical. ("int a, b[3];") I get rid of this as soon as
       possible, that is, before even constructing the AST, and I
       rigorously split up declarations into the identifier being
       declared and the type it is being given. All declarations that
       declare multiple identifiers at once are split up.

     * I allow only one storage modifier [K&R A8.1], and one of
       "const", "volatile", or "const volatile", [A8.2] and forget the
       (irrelevant) order of these things.

     * All integer type notations [A8.2], including the absence of a
       type (which means int), are normalized to one of the 8 integer
       types that C requires:

              unsigned char ~ char,
              signed char ~ char, 
              unsigned short int ~ unsigned short,
              signed short int ~ signed short ~ short,
              unsigned int ~ unsigned,
              signed int ~ signed ~,
              unsigned long int ~ unsigned long,
              signed long int ~ signed long ~ long. 

    * It is unfortunately necessary at this stage to allow for
      unspecified storage, because the inference of a storage class
      when none is specified is context-dependent.

    XXX Pre-ANSI ("K&R") prototypes are not covered here. They are not
    required for now, but if this code is ever re-used for other
    purposes, they may have to be added. XXX -}

data File    = File [Def]
               

data Def     = Fundef Storage Qualified Identifier Body
             | Typedef Storage Qualified Identifier     
             | Def Storage Qualified Identifier (Maybe Exp)
               

funDef t i b = Fundef Somewhere (Regular, t) i b
def t i e    = Def Somewhere (Regular, t) i e


data Type    = Void
             | UChar                         -- unsigned char
             | SChar                         -- (signed) char
             | UShort                        -- unsigned short (int)
             | SShort                        -- (signed) short (int)
             | UInt                          -- unsigned (int)
             | SInt                          -- (signed) (int)
             | ULong                         -- unsigned long (int)
             | SLong                         -- (signed) long (int)
             | Float                         -- float
             | Double                        -- double
             | LongDouble                    -- long double
             | NamedType TypeName
             | Struct StructName [(Qualified, FieldName, Maybe Int)]
             | Union UnionName [(Qualified, FieldName)]
             | Enum EnumName [(Identifier, Maybe Exp)]
             | Ptr Qualified
             | Proto Type [(Qualified, Identifier)] 
             | Array Type (Maybe Exp)

ptr t =
    Ptr (Regular, t)
namedType n =
    NamedType (TypeName n)

data Storage = Somewhere                     -- (not specified)
             | Auto                          -- auto
             | Static                        -- static
             | Extern                        -- extern
             | Register                      -- register
               

type Qualified = (Qual, Type)

data Qual = Regular | Const | Volatile | ConstVolatile
               

data Body    = Body [Def] [Stat]
               


-- STATEMENTS AND EXPRESSIONS ----------------------------------------

{- The syntax for statements and expressions, fortunately, is a lot
   more straightforward than that for declarations. In general, I
   follow the K&R syntax, but throw away information about whether
   precedence or parentheses are used to construct an equivalent
   expression. If statements always must have an else-clause, for it
   is easy enough to fill in something like "Computation Nothing". -}

data Stat    = Computation (Maybe Exp)
             | Block Body
             | If Exp Stat Stat
             | While Exp Stat
             | Dowhile Exp Stat
             | For (Maybe Exp) (Maybe Exp) (Maybe Exp) Stat
             | Break
             | Continue
             | Return (Maybe Exp)
             | Switch Exp Stat
             | Case Exp 
             | Default 
             | Label LabelName 
             | Goto GotoTarget

block ds ss =
    Block (Body ds ss)
computation e =
    Computation (Just e)
nop =
    Computation Nothing
for a b c s =
    For (Just a) (Just b) (Just c) s

data Exp     = Unary    Unop Exp
             | Binary   Binop Exp Exp
             | Ternary  Exp Exp Exp
             | Cast     Type Exp         --  XXX QUALIFIED? ???
             | Call     Exp [Exp]
             | Variable Identifier
             | ESize    Exp                  -- sizeof
             | TSize    Type             -- sizeof   XXX QUALIFIED? ???
             | Index    Exp Exp              -- a[b]
             | Dot      Exp FieldName        -- a.head
             | Arrow    Exp FieldName        -- a->head
             | GnuBody  Body                 -- ({ ... })
             | LitInt   Integer  
             | LitFloat Double
             | LitChar  Char
             | LitString String 
             | LitCompound [Exp]

gnuBody ds ss =
    GnuBody (Body ds ss)

data Binop   = Add                           -- + 
             | Sub                           -- - 
             | Mul                           -- * 
             | Div                           -- / 
             | Mod                           -- % 
             | And                           -- && 
             | Or                            -- || 
             | Band                          -- & 
             | Bor                           -- | 
             | Xor                           -- ^ 
             | Shl                           -- << 
             | Shr                           -- >> 
             | Eq                            -- == 
             | Ne                            -- != 
             | Lt                            -- < 
             | Gt                            -- > 
             | Le                            -- <= 
             | Ge                            -- >= 
             | Assign                        -- = 
             | AddAssign                     -- += 
             | SubAssign                     -- -= 
             | MulAssign                     -- *= 
             | DivAssign                     -- /= 
             | ModAssign                     -- %= 
             | BandAssign                    -- &= 
             | BorAssign                     -- |= 
             | XorAssign                     -- ^= 
             | ShlAssign                     -- <<= 
             | ShrAssign                     -- >>= 
             | Comma                         -- ,       

data Unop    = Minus                -- - 
             | Plus                 -- + 
             | Not                  -- ! 
             | Bnot                 -- ~ 
             | Memof                -- * 
             | Addrof               -- \& 
             | Preincr              -- ++
             | Predecr              -- --
             | Posincr              -- ++
             | Posdecr              -- --



