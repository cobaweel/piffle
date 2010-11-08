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


-- PRETTY PRINTING ---------------------------------------------------

{- This file is adapted from an earlier compiler I wrote for the BlooP
   and FlooP languages from Douglas Hofstadter's book Goedel, Escher,
   Bach. Unlike the ANSI C AST description, which I automatically
   generated from somebody else's ML code, this is entirely my own
   work. The idea I had with the BlooP compiler was to output C code
   that would look as similar as possible to what you might write by
   hand, which explains why I took so much trouble to provide an
   elaborate pretty printer. In particular, it tries to be smart about
   parentheses. -}

module CPretty() where
import C
import PrettyUtil hiding (SChar)


-- NOTE ABOUT TESTING ------------------------------------------------

{- Because the Piffle compiler does not use all C language constructs,
   some unused parts of the C pretty printer have not been tested very
   well! -}


-- HELPER FUNCTIONS --------------------------------------------------

{- kr will print its arguments on separate lines, enclosed in braces
   and indented 4 spaces, as per "K&R" indentation style for sequences
   of statements, as well as fields in structs and unions. -}

kr :: [Doc] -> [Doc] -> Doc
kr ds [] = 
    lbrace <$> indent 4 (vsep ds) <$> rbrace
kr [] ss = 
    lbrace <$> indent 4 (vsep ss) <$> rbrace
kr ds ss = 
    lbrace <$> indent 4 (vsep ds <$> line <> vsep ss) <$> rbrace

{- kc will print its arguments separated by commas and enclosed by
   braces, as is used for enum declarations and compound
   initializers. -}

kc :: [Doc] -> Doc
kc es = 
    braces (hsep ((<> comma) `map` es))

aparens = parens . align


-- DECLARATIONS ------------------------------------------------------

instance Pretty File where
    pretty (File ds) =
        vsep (pretty `map` ds) <> line

instance Pretty Body where
    pretty (Body ds ss) =
        kr (pretty `map` ds) (pretty `map` ss)

instance Pretty Storage where
    pretty Somewhere =
        empty
    pretty Auto =
        text "auto "
    pretty Static =
        text "static "
    pretty Extern =
        text "extern "
    pretty Register  = text "register "

instance Pretty Def where
    pretty (Fundef s qt n b) =
        line <> pretty s <> prettyQualified qt (pretty n) <$> pretty b
    pretty (Def s qt n Nothing) =
        pretty s <> prettyQualified qt (pretty n) <> semi
    pretty (Def s qt n e) =
        pretty s <> prettyQualified qt (pretty n) <+> 
        text "=" <+> pretty e <> semi
    pretty (Typedef s qt n) =
        text "typedef" <+> pretty s <> prettyQualified qt (pretty n) <> semi

instance Pretty Type where
    pretty t =
        prettyType t empty

instance Pretty Qual where
    pretty Regular =
        empty
    pretty Const =
        text "const "
    pretty Volatile =
        text "volatile "
    pretty ConstVolatile =
        text "const volatile "

{- Declarations in C are byzantine. They are written inside-out, and
   every time I turn my brain inside-out to match C syntax for almost
   long enough to understand this code, it flips inside-in before I
   can finish it. At some point I got it right though. -}

{- Print a declaration of a variable (or, in the case of a typedef, a
   type name) 'n' that will have type 't' and storage class 'q'. The
   variable name may be the empty document; that is how we produce
   types for sizeof() and casts. -}

prettyQualified :: Qualified -> Doc -> Doc
prettyQualified (q,t) n =
    pretty q <> prettyType t n

{- Print a declaration of a variable 's' with type 't' and no
   particular storage class. -}

prettyType :: Type -> Doc -> Doc
prettyType t s =
    base <> wedge t'
    where (base, t') = 
              pt t s
          wedge x
              | isEmpty x =
                  empty
              | otherwise =
                  space <> x

{- This is where the magic happens. Given a type and an identifier, we
   produce two results: the "base" of the type, and the rest of the
   type with the identifier filled in at the point where in any sane
   programming language we would expect the "base". -}

pt :: Type -> Doc -> (Doc, Doc)
pt (Ptr (q,t)) s =
    case t of
      Array _ _ -> 
          pt t (aparens inside)
      Proto _ _ -> 
          pt t (aparens inside)
      _ -> 
          pt t inside
    where 
      inside = 
          text "*" <> pretty q <> s
pt (Array t e) s = 
    pt t (s <> brackets (pretty e))
pt (Proto t []) s = 
    pt t (s <> aparens (text "void"))
pt (Proto t p) s = 
    pt t (s <> commad (param `map` p))
    where param (qt, f) =
              prettyQualified qt (pretty f)
pt t s = 
    (pb t,s)


{- This is a helper function for pt, which can print only base
   types. PRECONDITION: ARGUMENT *MUST* BE A BASE TYPE. -}

pb :: Type -> Doc

pb Void =
    text "void"
pb UChar =
    text "unsigned char"
pb SChar =
    text "signed char"
pb UShort =
    text "unsigned short"
pb SShort =
    text "signed short"
pb UInt =
    text "unsigned int"
pb SInt =
    text "signed int"
pb ULong =
    text "unsigned long"
pb SLong =
    text "signed long"
pb Float =
    text "float"
pb Double =
    text "double"
pb LongDouble =
    text "long double"
pb (NamedType t) =
    pretty t 
pb (Struct n []) =
    text "struct" <+> pretty n
pb (Struct n fs) =
    text "struct" <+> pretty n <+> kr (struct `map` fs) []
    where struct (qt, f, Nothing) =
              prettyQualified qt (pretty f) <> semi
          struct (qt, f, Just i) =
              prettyQualified qt (pretty f) <> colon <> pretty i <> semi
pb (Union n []) =
    text "union" <+> pretty n
pb (Union n fs) =
    text "union" <+> pretty n <+> kr (union `map` fs) []
    where union (qt, f) =
              prettyQualified qt (pretty f) <> semi
pb (Enum n []) =
    text "enum" <+> pretty n
pb (Enum n fs) =
    text "enum" <+> pretty n <+> kc (enum `map` fs)
    where enum (f, Nothing) =
              pretty f
          enum (f, e) =
              pretty f <+> text "=" <+> pretty e
pb _ =
    error "BUG: NOT A BASE TYPE"


{- This function tests the type printing. It returns a list of
   Booleans, representing the outcome of a number of tests. They
   should all comeout as True, or else something is wrong. The tests
   are taken from K&R. -}

{-
testPrettyType = 
    [ (px $ pointerTo $ pointerTo $ Void) == "void **x",
      (px $ pointerTo $ arrayOf $ Void) == "void (*x)[]",
      (px $ functionReturning $ pointerTo $ arrayOf $
       pointerTo $ functionReturning $ Void) == "void (*(*x())[])()",
      (px $ arrayOf $ pointerTo $ functionReturning $ 
       pointerTo $ arrayOf $ Void) == "void (*(*x[])())[]"]
    where px t                = show (prettyType t (text "x"))
          functionReturning t = Proto t []
          pointerTo t         = Ptr (Regular, t)
          arrayOf t           = Array t Nothing
-}

-- STATEMENTS --------------------------------------------------------

{- In order to avoid the whole dangling else mess, I enclose all
   then-clauses and else-clauses in braces. (That's good programming
   practice anyway.) Also, I leave out else clauses that are
   empty. Otherwise, statements are pretty straightforward. -}

instance Pretty Stat where
    pretty (Computation a) =
        pretty a <> semi
    pretty (Block a) =
        pretty a
    pretty (If a b c) =
        text "if" <+> aparens (pretty a) <+> pretty (blockify b) <+>
        case c of 
          Computation Nothing ->
              empty
          _ ->
              text "else" <+> pretty (blockify c)
    pretty (While a b) =
        text "while" <+> aparens (pretty a) <+> pretty b
    pretty (Dowhile a b) =
        text "do" <+> pretty b <+> text "while" <+> aparens (pretty a) <> semi
    pretty (For a b c d) =
        text "for" <+> 
        aparens (pretty a <> semi </> pretty b <> semi </> pretty c) <+>
        pretty (blockify d)
    pretty Break =
        text "break" <> semi
    pretty Continue =
        text "continue" <> semi
    pretty (Return a) =
        text "return" <+> pretty a <> semi
    pretty (Switch a b) =
        text "switch" <+> aparens (pretty a) <+> pretty b
    pretty (Case a) =
        text "case" <+> pretty a <> colon
    pretty Default =
        text "default" <> colon
    pretty (Label a) =
        pretty a <> colon
    pretty (Goto a) =
        pretty "goto" <+> pretty a <> semi

blockify :: Stat -> Stat
blockify s@(Block _) =
    s
blockify s =
    Block (Body [] [s])


-- EXPRESSIONS -------------------------------------------------------

{- The pretty-printing of expressions is somewhat conservative. I try
   to omit the most obviously superfluous parentheses. Note that to
   emulate a human C programmer in this regard would be very tricky.
   Most programmers rely on some, but not all, of the precedence
   rules, and code that maximally exploits the precedence rules might
   be as hard to read as code with a few superfluous parentheses. -}

instance Pretty Exp where
  pretty (Unary Posincr e) =
      parenize2 e <> pretty Posincr
  pretty (Unary Posdecr e) =
      parenize2 e <> pretty Posdecr
  pretty (Unary op e) =
      pretty op <> parenize2 e
  pretty (Binary op x y) =
      parenize3 x <+> pretty op </> parenize3 y
  pretty (Ternary x y z) =
      parenize3 x <+> text "?" </> parenize3 y <+> text ":" </> parenize3 z
  pretty (Cast t e) =
      aparens (pretty t) </> parenize e
  pretty (Call e es) =
      parenize e <> commad (pretty `map` es)
  pretty (Variable v) =
      pretty v
  pretty (ESize e) =
      text "sizeof" <> parenize e
  pretty (TSize t) =
      text "sizeof" <> (aparens (pretty t))
  pretty (Index a i) =
      parenize a <> brackets (pretty i)
  pretty (Dot e f) =
      parenize e <> text "." <> pretty f
  pretty (Arrow e f) =
      parenize e <> text "->" <> pretty f
  pretty (GnuBody b) =
      aparens (pretty b)
  pretty (LitInt i) =
      pretty i
  pretty (LitFloat f) =
      pretty f
  pretty (LitChar c) =
      pretty c
  pretty (LitString s) =
      pretty s
  pretty (LitCompound c) =
      kc (pretty `map` c)


{- This function will enclose its argument in parentheses so that it
   can safely be used as part of a compound expression, except in a
   few cases where it couldn't possibly be a problem to leave the
   parens off. -}

parenize :: Exp -> Doc
parenize e@(LitInt _) =
    pretty e
parenize e@(LitFloat _) =
    pretty e
parenize e@(LitChar _) =
    pretty e
parenize e@(LitString _) =
    pretty e
parenize e@(LitCompound _) =
    pretty e
parenize e@(Variable _) =
    pretty e
parenize e@(GnuBody _) =
    pretty e
parenize e =
    aparens (pretty e)


{- This is like parenize, but a little bit more aggressive in how many
   parens it leaves out. -}

parenize2 :: Exp -> Doc
parenize2 e@(Dot _ _) =
    pretty e
parenize2 e@(Arrow _ _) =
    pretty e
parenize2 e@(Call _ _) =
    pretty e
parenize2 e@(Index _ _) =
    pretty e
parenize2 e =
    parenize e


{- This is like parenize2, but a little bit more aggressive still in
   how many parens it leaves out. -}

parenize3 :: Exp -> Doc
parenize3 e@(Cast _ _) =
    pretty e
parenize3 e@(ESize _) =
    pretty e
parenize3 e@(TSize _) =
    pretty e
parenize3 e@(Unary _ _) =
    pretty e
parenize3 e =
    parenize2 e


-- OPERATORS ---------------------------------------------------------

instance Pretty Binop where
  pretty Add =
      text "+" 
  pretty Sub =
      text "-" 
  pretty Mul =
      text "*" 
  pretty Div =
      text "/" 
  pretty Mod =
      text "%" 
  pretty And =
      text "&&" 
  pretty Or =
      text "||" 
  pretty Band =
      text "&" 
  pretty Bor =
      text "|" 
  pretty Xor =
      text "^" 
  pretty Shl =
      text "<<" 
  pretty Shr =
      text ">>" 
  pretty Eq =
      text "==" 
  pretty Ne =
      text "!=" 
  pretty Lt =
      text "<" 
  pretty Gt =
      text ">" 
  pretty Le =
      text "<=" 
  pretty Ge =
      text ">=" 
  pretty Assign =
      text "=" 
  pretty AddAssign =
      text "+=" 
  pretty SubAssign =
      text "-=" 
  pretty MulAssign =
      text "*=" 
  pretty DivAssign =
      text "/=" 
  pretty ModAssign =
      text "%=" 
  pretty BandAssign =
      text "&=" 
  pretty BorAssign =
      text "|=" 
  pretty XorAssign =
      text "^=" 
  pretty ShlAssign =
      text "<<=" 
  pretty ShrAssign =
      text ">>=" 
  pretty Comma =
      text ","

instance Pretty Unop where
  pretty Minus =
      text "-" 
  pretty Plus =
      text "+" 
  pretty Not =
      text "!" 
  pretty Bnot =
      text "~" 
  pretty Memof =
      text "*" 
  pretty Addrof =
      text "&" 
  pretty Preincr =
      text "++" 
  pretty Predecr =
      text "--" 
  pretty Posincr =
      text "++" 
  pretty Posdecr =
      text "--" 


-- SYMBOLS -----------------------------------------------------------

instance Pretty Identifier where
    pretty (Identifier s) =
        text s

instance Pretty TypeName where
    pretty (TypeName s) =
        text s

instance Pretty StructName where
    pretty (StructName s) =
        text s

instance Pretty UnionName where
    pretty (UnionName s) =
        text s

instance Pretty EnumName where
    pretty (EnumName s) =
        text s

instance Pretty LabelName where
    pretty (LabelName s) =
        text s

instance Pretty GotoTarget where
    pretty (GotoTarget s) =
        text s

instance Pretty FieldName where
    pretty (FieldName s) =
        text s

