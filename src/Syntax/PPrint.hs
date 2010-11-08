-- | Pretty print module based on Philip Wadlers \"prettier printer\":
-- \"A prettier printer\", draft paper, April 1997, revised March 1998,
-- <http://cm.bell-labs.com/cm/cs/who/wadler/papers/prettier/prettier.ps> 

-- Modified by Jaap Weel as part of the Piffle compiler to, among
-- other things, be even prettier: now in COLOR!

module Syntax.PPrint ( Doc, Pretty, pretty, show, putDoc, hPutDoc,
               (<>), (<+>), (</>), (<//>), (<$>), (<$$>), (<$$$>), sep,
               fillSep, hsep, vsep, cat, fillCat, hcat, vcat, vjam,
               punctuate, align, hang, indent, fill, fillBreak, list,
               tupled, semiBraces, encloseSep, angles, langle, rangle,
               parens, lparen, rparen, braces, lbrace, rbrace,
               brackets, lbracket, rbracket, dquotes, dquote, squotes,
               squote, comma, space, dot, backslash, semi, colon,
               equals, string, bool, int, integer, float, double,
               rational, softline, softbreak, empty, char, text, line,
               linebreak, nest, group, column, nesting, width,
               SimpleDoc(..), renderPretty, renderColorfully,
               renderCompact, displayS, displayIO, sgr, colored, isEmpty )
               where

import IO (Handle,hPutStr,hPutChar,stdout)

{- From the documentation originally distributed with this library:
Copyright 2000, Daan Leijen. All rights reserved. Redistribution and
use in source and binary forms, with or without modification, are
permitted provided that the following conditions are met:
Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.  Redistributions
in binary form must reproduce the above copyright notice, this list of
conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution. This software is
provided by the copyright holders "as is" and any express or implied
warranties, including, but not limited to, the implied warranties of
merchantability and fitness for a particular purpose are
disclaimed. In no event shall the copyright holders be liable for any
direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute
goods or services; loss of use, data, or profits; or business
interruption) however caused and on any theory of liability, whether
in contract, strict liability, or tort (including negligence or
otherwise) arising in any way out of the use of this software, even if
advised of the possibility of such damage.  -}

infixr 5 </>,<//>,<$>,<$$>
infixr 6 <>,<+>

-- | The document (list xs) comma seperates the documents xs and
-- encloses them in square brackets. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma seperators are put in front of the elements.
list :: [Doc] -> Doc
list            = encloseSep lbracket rbracket comma

-- | The document (tupled xs) comma seperates the documents xs and
-- encloses them in parenthesis. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma seperators are put in front of the elements.
tupled :: [Doc] -> Doc
tupled          = encloseSep lparen   rparen  comma

-- | The document (semiBraces xs) seperates the documents xs with semi
-- colons and encloses them in braces. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All semi colons are put in front of the elements.
semiBraces :: [Doc] -> Doc
semiBraces      = encloseSep lbrace   rbrace  semi

-- | The document (encloseSep l r sep xs) concatenates the documents xs
-- seperated by sep and encloses the resulting document by l and r. The
-- documents are rendered horizontally if that fits the page. Otherwise
-- they are aligned vertically. All seperators are put in front of the
-- elements.
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right sep ds
    = case ds of
        []  -> left <> right
        [d] -> left <> d <> right
        _   -> align (cat (zipWith (<>) (left : repeat sep) ds) <> right) 


-- | (punctuate p xs) concatenates all in documents xs with document p
-- except for the last document.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []      = []
punctuate _ [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

-- | The document (sep xs) concatenates all documents xs either
-- horizontally with (<+>), if it fits the page, or vertically with
-- (<$>).
sep :: [Doc] -> Doc
sep             = group . vsep

-- | The document (fillSep xs) concatenates documents xs horizontally
-- with (<+>) as long as its fits the page, than inserts a line and
-- continues doing that for all documents in xs.
fillSep :: [Doc] -> Doc
fillSep         = fold (</>)

-- | The document (hsep xs) concatenates all documents xs horizontally
-- with (<+>).
hsep :: [Doc] -> Doc
hsep            = fold (<+>)

-- | The document (vsep xs) concatenates all documents xs vertically
-- with (<$>). If a group undoes the line breaks inserted by vsep, all
-- documents are seperated with a space.
vsep :: [Doc] -> Doc
vsep            = fold (<$>) 

-- | The document (cat xs) concatenates all documents xs either
-- horizontally with (<>), if it fits the page, or vertically with
-- (<$$>).
cat :: [Doc] -> Doc
cat             = group . vcat

-- | The document (fillCat xs) concatenates documents xs horizontally
-- with (<>) as long as its fits the page, than inserts a linebreak
-- and continues doing that for all documents in xs.
fillCat :: [Doc] -> Doc
fillCat         = fold (<//>)

-- | The document (hcat xs) concatenates all documents xs horizontally
-- with (<>).
hcat :: [Doc] -> Doc
hcat            = fold (<>)

-- | The document (vcat xs) concatenates all documents xs vertically
-- with (<$$>). If a group undoes the line breaks inserted by vcat,
-- all documents are directly concatenated.
vcat :: [Doc] -> Doc
vcat            = fold (<$$>) 

vjam :: [Doc] -> Doc
vjam            = fold (<$$$>)

fold _ []       = empty
fold f ds       = foldr1 f ds

-- | The document (x <> y) concatenates document x and document y. It
-- is an associative operation having empty as a left and right unit.
(<>) :: Doc -> Doc -> Doc
x <> y = x `beside` y

-- | The document (x <+> y) concatenates document x and y with a space
-- in between.
(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> space <> y

-- | The document (x </> y) concatenates document x and y with a
-- softline in between. This effectively puts x and y either next to each
-- other (with a space in between) or underneath each other.
(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

-- | The document (x <//> y) concatenates document x and y with a
-- softbreak in between. This effectively puts x and y either right
-- next to each other or underneath each other.
(<//>) :: Doc -> Doc -> Doc
x <//> y = x <> softbreak <> y   

-- | The document (x <$> y) concatenates document x and y with a line
-- in between.
(<$>) :: Doc -> Doc -> Doc
x <$> y = x <> line <> y

-- | The document (x <$$> y) concatenates document x and y with a
-- linebreak in between.
(<$$>) :: Doc -> Doc -> Doc
x <$$> y = x <> linebreak <> y

-- | This behaves like <$$>, but generates no empty lines for empty
-- | documents.
(<$$$>) :: Doc -> Doc -> Doc
Empty <$$$> x = x
x <$$$> Empty = x
x <$$$> y     = x <$$> y

-- | The document softline behaves like space if the resulting output
-- fits the page, otherwise it behaves like line.
softline :: Doc
softline = group line

-- | The document softbreak behaves like empty if the resulting output
-- fits the page, otherwise it behaves like line.
softbreak :: Doc                
softbreak = group linebreak

-- | Enclose a 'Doc' in single quotes
squotes :: Doc -> Doc
squotes = enclose squote squote

-- | Enclose a 'Doc' in double quotes
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

-- | Enclose a 'Doc' in braces
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | Enclose a 'Doc' in parentheses
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | Enclose a 'Doc' in angle brackets
angles :: Doc -> Doc
angles = enclose langle rangle

-- | Enclose a 'Doc' in square brackets
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

enclose l r x = l <> x <> r

-- | The character \'(\'
lparen :: Doc
lparen = char '('

-- | The character \')\'
rparen :: Doc
rparen = char ')'

-- | The character \'<\'
langle :: Doc
langle = char '<'

-- | The character \'>\'
rangle :: Doc
rangle = char '>'

-- | The character \'{\'
lbrace :: Doc
lbrace = char '{'

-- | The character \'}\'
rbrace :: Doc
rbrace = char '}'

-- | The character \'[\'
lbracket :: Doc
lbracket = char '['

-- | The character \']\'
rbracket :: Doc
rbracket = char ']'

-- | The character \'\'\'
squote :: Doc
squote = char '\''

-- | The character \'\"\'
dquote :: Doc
dquote = char '"'

-- | The character \';\'
semi :: Doc
semi = char ';'

-- | The character \':\'
colon :: Doc
colon = char ':'

-- | The character \',\'
comma :: Doc
comma = char ','

-- | The character \' \'
space :: Doc
space = char ' '

-- | The character \'.\'
dot :: Doc
dot = char '.'

-- | The character \'\\\'
backslash :: Doc
backslash = char '\\'

-- | The character \'=\'
equals :: Doc
equals = char '='

-- | The document (string s) concatenates all characters in s using
-- line for newline characters and char for all other characters. It
-- is used instead of text whenever the text contains newline
-- characters.
string "" = empty
string ('\n':s) = line <> string s
string s = case (span (/='\n') s) of
                    (xs,ys) -> text xs <> string ys

-- | The document (bool b) shows the literal boolean b using text.
bool :: Bool -> Doc
bool b = text (show b)

-- | The document (int i) shows the literal integer i using text.
int :: Int -> Doc                  
int i = text (show i)

-- | The document (integer i) shows the literal integer i using text.
integer :: Integer -> Doc
integer i = text (show i)

-- | The document (float f) shows the literal float f using text.
float :: Float -> Doc
float f = text (show f)

-- | The document (double d) shows the literal double d using text.
double :: Double -> Doc
double d = text (show d)

-- | The document (rational r) shows the literal rational r using
-- text.
rational :: Rational -> Doc
rational r = text (show r)
                  
class Pretty a where
  pretty        :: a -> Doc 
  prettyList    :: [a] -> Doc
  prettyList    = list . map pretty

instance Pretty a => Pretty [a] where
  pretty        = prettyList
  
instance Pretty Doc where
  pretty        = id  
  
instance Pretty () where
  pretty ()     = text "()"

instance Pretty Bool where
  pretty b      = bool b
  
instance Pretty Char where
  pretty c      = char c
  prettyList s  = string s
    
instance Pretty Int where
  pretty i      = int i
  
instance Pretty Integer where
  pretty i      = integer i

instance Pretty Float where
  pretty f      = float f

instance Pretty Double where
  pretty d      = double d

instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty (x,y)  = tupled [pretty x, pretty y]

instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing        = empty
  pretty (Just x)       = pretty x
  

-- | The document (fillBreak i x) first renders document x. It than
-- appends spaces untill the width is equal to i. If the width of x is
-- already larger than i, the nesting level is increased by i and a
-- line is appended.
fillBreak :: Int -> Doc -> Doc
fillBreak f x   = width x (\w ->
                  if (w > f) then nest f linebreak 
                             else text (spaces (f - w)))
   
-- | The document (fill i x) renders document x. It than appends
-- spaces until the width is equal to i. If the width of x is already
-- larger, nothing is appended.
fill :: Int -> Doc -> Doc
fill f d        = width d (\w ->
                  if (w >= f) then empty
                              else text (spaces (f - w)))

width d f       = column (\k1 -> d <> column (\k2 -> f (k2 - k1)))        

-- | The document (indent i x) indents document x with i spaces.
indent :: Int -> Doc -> Doc
indent i d      = hang i (text (spaces i) <> d)

-- | The hang combinator implements hanging indentation. The document
-- (hang i x) renders document x with a nesting level set to the
-- current column plus i
hang :: Int -> Doc -> Doc
hang i d        = align (nest i d)

-- | The document (align x) renders document x with the nesting level
-- set to the current column. It is used for example to implement
-- hang.
align :: Doc -> Doc
align d         = column (\k ->
                  nesting (\i -> nest (k - i) d))   --nesting might be negative :-)


-- | The abstract data type Doc represents pretty documents.
data Doc        = Empty

                -- invariant: char is not '\n'
                | Char Char

                -- invariant: text doesn't contain '\n'
                | Text !Int String

                -- True <=> when undone by group, do not insert a space 
                | Line !Bool

                | Cat Doc Doc
                | Nest !Int Doc

                -- invariant: first lines of first doc longer than the
                -- first lines of the second doc
                | Union Doc Doc

                | Column  (Int -> Doc)
                | Nesting (Int -> Doc)

                -- ANSI "Select Graphics Rendition" color code
                | SGR Int

isEmpty       :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False
                
-- | The data type SimpleDoc represents rendered documents and is used
-- by the display functions.  The Int in SText contains the length of
-- the string. The Int in SLine contains the indentation for that
-- line. The library provides two default display functions displayS
-- and displayIO. You can provide your own display function by writing
-- a function from a SimpleDoc to your own output format.
data SimpleDoc  = SEmpty
                | SChar Char SimpleDoc
                | SText !Int String SimpleDoc
                | SLine !Int SimpleDoc

-- | The empty document is, indeed, empty. Although empty has no
-- content, it does have a 'height' of 1 and behaves exactly like
-- (text \"\") (and is therefore not a unit of <$>).
empty :: Doc
empty           = Empty

-- | The document (char c) contains the literal character c. The
-- character shouldn't be a newline ('\n'), the function line should
-- be used for line breaks.
char :: Char -> Doc
char '\n'       = line
char c          = Char c

-- | The document (text s) contains the literal string s. The string
-- shouldn't contain any newline ('\n') characters. If the string
-- contains newline characters, the function string should be used.
text :: String -> Doc
text ""         = Empty
text s          = Text (length s) s

-- | The line document advances to the next line and indents to the
-- current nesting level. Document line behaves like (text \" \") if the
-- line break is undone by group.
line :: Doc
line            = Line False

-- | The linebreak document advances to the next line and indents to
-- the current nesting level. Document linebreak behaves like empty if
-- the line break is undone by group.
linebreak :: Doc
linebreak       = Line True


beside x y      = Cat x y

-- | The document (nest i x) renders document x with the current
-- indentation level increased by i (See also hang , align and
-- indent).
nest :: Int -> Doc -> Doc
nest i x        = Nest i x

column f        = Column f
nesting f       = Nesting f     

-- | The group combinator is used to specify alternative layouts. The
-- document (group x) undoes all line breaks in document x. The
-- resulting line is added to the current line if that fits the
-- page. Otherwise, the document x is rendered without any changes.
group :: Doc -> Doc
group x         = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line break)    = if break then Empty else Text 1 " "
flatten (Union x _)     = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten other           = other                     --Empty,Char,Text
  
  

-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------

-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

-- list of indentation/document pairs; saves an indirection over [(Int,Doc)]
data Docs   = Nil
            | Cons !Int Doc Docs

-- | This is the default pretty printer which is used by show, putDoc
-- and hPutDoc. (renderPretty ribbonfrac width x) renders document x
-- with a page width of width and a ribbon width of (ribbonfrac *
-- width) characters. The ribbon width is the maximal amount of
-- non-indentation characters on a line. The parameter ribbonfrac
-- should be between 0.0 and 1.0. If it is lower or higher, the ribbon
-- width will be 0 or width respectively.

renderPretty = renderColorfully False

renderColorfully :: Bool -> Float -> Int -> Doc -> SimpleDoc
renderColorfully inColor rfrac w x =
    best 0 0 (Cons 0 x Nil)                
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (fromIntegral w * rfrac)))
      
      -- best :: n = indentation of current line
      --         k = current column  
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best _ _  Nil      = SEmpty
      best n k (Cons i d ds)  
        = case d of
            Empty       -> best n k ds                
            Char c      -> let k' = k+1 in seq k' (SChar c (best n k' ds))
            Text l s    -> let k' = k+l in seq k' (SText l s (best n k' ds))
            Line _      -> SLine i (best i i ds)                 
            Cat x y     -> best n k (Cons i x (Cons i y ds))                
            Nest j x    -> let i' = i+j in seq i' (best n k (Cons i' x ds))
            Union x y   -> nicest n k (best n k (Cons i x ds))                
                                      (best n k (Cons i y ds))                

            Column f    -> best n k (Cons i (f k) ds)
            Nesting f   -> best n k (Cons i (f i) ds)                            
            SGR i       -> if inColor then
                               SText 0 ("\^[["++show i++"m") (best n k ds)
                           else
                               best n k ds

      --nicest :: r = ribbon width, w = page width, 
      --          n = indentation of current line, k = current column
      --          x and y, the (simple) documents to chose from.
      --          precondition: first lines of x are longer than the first lines of y.                                      
      nicest n k x y    | fits width x  = x
                        | otherwise     = y
                        where
                          width = min (w - k) (r - k + n)
  
sgr i = SGR i
colored i d = sgr i <> d <> sgr 0
                                                                                      
fits w _        | w < 0         = False
fits _ SEmpty                   = True
fits w (SChar _ x)              = fits (w - 1) x                  
fits w (SText l _ x)            = fits (w - l) x
fits _ (SLine _ _)              = True

-- | (renderCompact x) renders document x without adding any
-- indentation. Since no 'pretty' printing is involved, this renderer
-- is very fast. The resulting output contains fewer characters as a
-- pretty printed version and can be used for output that is read by
-- other programs.
renderCompact :: Doc -> SimpleDoc
renderCompact x   
    = scan 0 [x]
    where
      scan _ []     = SEmpty
      scan k (d:ds) = case d of
                        Empty       -> scan k ds
                        Char c      -> let k' = k+1 in seq k' (SChar c (scan k' ds))
                        Text l s    -> let k' = k+l in seq k' (SText l s (scan k' ds))
                        Line _      -> SLine 0 (scan 0 ds)    
                        Cat x y     -> scan k (x:y:ds)
                        Nest _ x    -> scan k (x:ds)
                        Union _ y   -> scan k (y:ds)
                        Column f    -> scan k (f k:ds)
                        Nesting f   -> scan k (f 0:ds)
                        SGR _       -> scan k ds

-- | (displayS simpleDoc) takes the output simpleDoc from a rendering
-- function and transforms it to a ShowS type (for use in the Show
-- class).
displayS :: SimpleDoc -> ShowS
displayS SEmpty             = id
displayS (SChar c x)        = showChar c . displayS x
displayS (SText _ s x)      = showString s . displayS x
displayS (SLine i x)        = showString ('\n':indentation i) . displayS x


-- | (displayIO handle simpleDoc) writes simpleDoc to the file handle handle.
displayIO :: Handle -> SimpleDoc -> IO ()
displayIO handle simpleDoc
    = display simpleDoc
    where
      display SEmpty        = return ()
      display (SChar c x)   = do{ hPutChar handle c; display x}  
      display (SText _ s x) = do{ hPutStr handle s; display x}
      display (SLine i x)   = do{ hPutStr handle ('\n':indentation i); display x}

instance Show Doc where
  showsPrec _ doc       = displayS (renderPretty 0.4 80 doc)

-- | The action (putDoc doc) pretty prints document doc to the
-- standard output. with a page width of 80 characters and a ribbon
-- width of 40 characters. (JW: This used to be 100, which is an
-- abomination, dammit. Terminals should never need to be wider than
-- 80.)
putDoc :: Doc -> IO ()
putDoc doc              = hPutDoc stdout doc

-- | (hPutDoc handle doc) pretty prints document doc to the file
--   handle handle with a page width of 80 characters and a ribbon
--   width of 40 characters.
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc handle doc      = displayIO handle (renderPretty 0.4 80 doc)

-- | Insert spaces
spaces n | n <= 0    = ""
         | otherwise = replicate n ' '

-- | Insert spaces (deprecated; works exactly like 'spaces').
indentation n = spaces n
