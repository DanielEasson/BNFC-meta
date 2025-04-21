{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-
    BNF Converter: Pretty-printer generator
    Copyright (C) 2004  Author:  Aarne Ranta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

-----------------------------------------------------------------------------
-- | 
-- This module contains the pretty printer generators based on the the grammar 
-- and its annotations using the [`wl-pprint`](https://hackage.haskell.org/package/wl-pprint), [`wl-pprint-console`](https://hackage.haskell.org/package/wl-pprint-console) and 
-- [`colorful-monoids`](https://hackage.haskell.org/package/colorful-monoids) libraries.
-- 
-- Annotations can be used to customise the pretty printer generation process.
-- The available annotations can be grouped into the following categories:
--
-- Combinators:  
-- These annotations control the positioning of items in the output:  
-- 
--   - `Empty`: No spacing between this item and the next, regardless of width.  
--   - `Space`: Inserts a space between this item and the next, regardless of width.  
--   - `Linebreak`: Uses a line break instead of a regular line between items.  
--      Acts like `Empty` or a line depending on line width.  
--   - `Nest`: Sets the nesting level for this item and all following items  
--      until the end of the production. This is the only way to set indentation.  
-- 
-- Colors:  
-- These annotations control the color of the output.  
-- Use `Bg` or `Fg` before the color name to specify whether  
-- the background or foreground should be colored.  
-- 
-- Available colors:  
-- 
--   - `Red`  
--   - `Green`  
--   - `Blue`  
--   - `Yellow`  
--   - `Magenta`  
--   - `Cyan`  
--   - `White`  
--   - `Black`  
--   - `RGB`: Allows specifying a custom RGB color.  
-- 
-- Styles: 
-- These annotations control the text style in the output:  
-- 
--   - `Bold`  
--   - `Italic`  
--   - `Underline`  
--   - `Blink`  
--   - `Invert`  
-- 
-- Defaults:  
-- These default rules differ from other annotations:  
-- 
--   - `DefaultFg`: Sets the default foreground color for the grammar.  
--   - `DefaultBg`: Sets the default background color for the grammar.  
--   - `DefaultStyle`: Sets the default text style for the grammar.  
--
-- Example:
--
-- Using a C++ like grammar:
--
-- > bnfc [lbnf|
-- > comment "//" ;
-- > comment "/*" "*/" ;
-- > 
-- > Prog. Program  ::= [Function] ;
-- > Fun.  Function ::= TType (Italic, Space 1) Ident (Space 1) "(" (Empty) [Decl] (Empty) ")" (Space 1) "{" (Nest 2) [Stm] "}" ;
-- > Dec.  Decl     ::= TType [Ident] ; 
-- > 
-- > terminator Function "" ;
-- > terminator Stm "" ;
-- > separator  Decl "," ;
-- > separator  nonempty Ident "," ;
-- > 
-- > SDecl.   Stm ::= Decl ";" ;
-- > SExp.    Stm ::= Exp ";" ;
-- > SBlock.  Stm ::= "{" (Nest 2) [Stm] "}" ;
-- > SWhile.  Stm ::= "while" (Space 1, Bg Red) "(" (Empty) Exp (Empty) ")" (Space 1) Stm;
-- > SReturn. Stm ::= "return" Exp  ";" ;
-- > 
-- > EAss.    Exp  ::= Ident (Space 1) "=" (Space 1, Underline) Exp ;
-- > ELt.     Exp1 ::= Exp2 (Space 1) "<" (Space 1, Fg RGB 0 255 0) Exp2 ;
-- > EAdd.    Exp2 ::= Exp2 (Space 1) "+" (Invert, Space 1) Exp3 ;
-- > ESub.    Exp2 ::= Exp2 "-" Exp3 ;
-- > EMul.    Exp3 ::= Exp3 "*" Exp4 ;
-- > Call.    Exp4 ::= Ident "(" [Exp] ")" ;
-- > EVar.    Exp4 ::= Ident ;
-- > EStr.    Exp4 ::= String ;
-- > EInt.    Exp4 ::= Integer ; 
-- > EDouble. Exp4 ::= Double ;
-- > 
-- > coercions Exp 4 ;
-- > 
-- > separator Exp "," ;
-- > 
-- > TInt.    TType ::= "int" ;
-- > TDouble. TType ::= "double" ;
-- > 
-- > DefaultBg Blue ;
-- > |]
--
-- Annotations are written on the right of an item, enclosed in brackets.
-- Multiple annotations are separated by a comma.
-- The annotations are applied from right to left.
--
-- > basicExample :: IO ()
-- > basicExample = do
-- >   let test = [program|
-- >   int mx () { 
-- >       x = 0 ; 
-- >       while ( x < m ) { 
-- >           y = 0 ; 
-- >           while ( y < n ) { 
-- >               y = y + 1 ; 
-- >               z = z + 1 ;
-- >               k = k + 1 ;
-- >           } 
-- >        x = x + 1 ; 
-- >       }
-- >   }
-- >   |]
-- >   
-- >   pprintTree 25 test
--
-- More examples can be found in the examples directory.
-- Additionally the grammars used for the test suite can be found in the test/Grammars directory.
module Language.LBNF.CFtoPrettyPrinter (cf2PrettyPrinter) where

import Language.LBNF.CF
import Data.Char(toLower)
import Language.LBNF.Grammar(Annotation(..), Style(..), Color(..))
import Language.LBNF.Runtime (Pretty(..))
import System.IO (stdout, hPutStr)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import           Text.PrettyPrint.Annotated.WL as PP hiding (Doc, Pretty(..))
import qualified Text.PrettyPrint.Annotated.WL as WL
import Data.Monoid.Colorful.Flat


-- | A type alias for pretty-printed documents. Each document is annotated
-- with metadata (e.g., colors or styles) via a list of `Colored String` elements.
type Doc = WL.Doc [Colored String]

-- | Generates Template Haskell declarations for pretty-printing a BNFC grammar.
-- This function traverses the `CF` grammar structure and creates pretty-printing
-- instances for:
-- 
-- 1. Identifiers, if the grammar contains them.
-- 2. Tokens defined via pragmas in the grammar.
-- 3. General pretty-printing rules for all other grammar productions.
cf2PrettyPrinter :: CF -> Q [Dec]
cf2PrettyPrinter cf = sequence $ concat [
  -- | Generate a pretty-printing rule for identifiers, if the grammar contains them.
  if hasIdent cf then [identPrettyRule cf] else [],
  -- | Generate pretty-printing rules for tokens based on user-defined pragmas.
  [ownPrettyPrintRule cf own | (own, _) <- tokenPragmas cf],
  -- | Generate pretty-printing rules for all grammar productions.
  prettyrules cf
  ]


{-
showsPrintRule cf t = unlines $ [
  "instance Print " ++ t ++ " where",
  "  prt _ x = doc (shows x)",
  ifList cf t
  ]
-}

-- | Instance of the `Lift` type class for `Colored String`, enabling
-- Template Haskell to generate code for colorful and styled strings.
-- This implementation supports the following constructs:
-- 
-- - `Value`: A plain string value.
-- - `Style` and `Unstyle`: Apply or remove a specific style.
-- - `Fg` and `Bg`: Set foreground and background colors.
-- - `Push`, `Pop`, `Reset`: Manage the style stack.
instance Lift (Colored String) where
  lift (Value x)       = [| Value x |]
  lift (Style s)       = [| Style s |]
  lift (Unstyle s)     = [| Unstyle s |]
  lift (Fg color)      = [| Fg color |]
  lift (Bg color)      = [| Bg color |]
  lift Push            = [| Push |]
  lift Pop             = [| Pop |]
  lift Reset           = [| Reset |]
  liftTyped = liftTyped


-- | Instance of the `Lift` type class for `Color`, providing Template
-- Haskell support for the `Data.Monoid.Colorful.Flat.Color` type.
instance Lift Data.Monoid.Colorful.Flat.Color where
  lift DefaultColor       = [| DefaultColor |]
  lift Black              = [| Black |]
  lift Red                = [| Red |]
  lift Green              = [| Green |]
  lift Yellow             = [| Yellow |]
  lift Blue               = [| Blue |]
  lift Magenta            = [| Magenta |]
  lift Cyan               = [| Cyan |]
  lift White              = [| White |]
  lift DullBlack          = [| DullBlack |]
  lift DullRed            = [| DullRed |]
  lift DullGreen          = [| DullGreen |]
  lift DullYellow         = [| DullYellow |]
  lift DullBlue           = [| DullBlue |]
  lift DullMagenta        = [| DullMagenta |]
  lift DullCyan           = [| DullCyan |]
  lift DullWhite          = [| DullWhite |]
  lift (Color256 w)       = [| Color256 w |]
  lift (RGB r g b)        = [| RGB r g b |]
  liftTyped = liftTyped

-- | Instance of the `Lift` type class for `Style`, enabling Template
-- Haskell support for applying text styles. Supported styles include:
-- 
-- - `Bold`: Make text bold.
-- - `Italic`: Italicize text.
-- - `Underline`: Add an underline.
-- - `Invert`: Invert text colors.
-- - `Blink`: Make text blink (if supported by the terminal).
instance Lift Data.Monoid.Colorful.Flat.Style where
  lift Bold       = [| Bold |]
  lift Italic     = [| Italic |]
  lift Underline  = [| Underline |]
  lift Invert     = [| Invert |]
  lift Blink      = [| Blink |]
  liftTyped = liftTyped  

-- | Generates a pretty-printing rule for the `Ident` category of the grammar.
-- This ensures that the `Ident` rule is only generated when it is needed.
identPrettyRule cf = ownPrettyPrintRule cf "Ident"

-- | Generates a Template Haskell instance for the `Pretty` type class
-- for a specific grammar rule. This function takes into account:
-- 
-- - Default foreground colors (`defaultFg`) for the grammar.
-- - Default styles (`defaultStyle`) applied to the grammar production.
-- - Whether the category uses positional data (checked via `isPositionCat`).
ownPrettyPrintRule :: CF -> String -> DecQ
ownPrettyPrintRule cf own = do
  i <- newName "i"
  
  let fgdef = case defaultFg cf of
                Left "" -> Value ""
                Left s -> (Fg . colMap . str2Col) s
                Right [r,g,b] -> rgb2fg (read r) (read g) (read b)
      styledef = map (styleMap . str2Style) $ defaultStyle cf
      ann = [fgdef] ++ styledef
      posn = if isPositionCat cf own
               then conP (mkName own) [tupP [wildP, varP i]]
               else conP (mkName own) [varP i]
      body = normalB [| annotate $(lift ann) (pretty 0 ($(varE i))) |]
      prtc = funD 'pretty [clause [wildP, posn] body []]
  instanceD (cxt []) (appT (conT ''Pretty) $ conT $ mkName own) [prtc]

-- | Maps a `String` representation of a color to its corresponding
-- `Language.LBNF.Grammar.Color` value.
str2Col :: String -> Language.LBNF.Grammar.Color
str2Col "Color_Red"     = Color_Red
str2Col "Color_Blue"    = Color_Blue
str2Col "Color_Green"   = Color_Green
str2Col "Color_Yellow"  = Color_Yellow
str2Col "Color_Cyan"    = Color_Cyan
str2Col "Color_Magenta" = Color_Magenta
str2Col "Color_White"   = Color_White
str2Col "Color_Black"   = Color_Black

-- | Maps a `String` representation of a style to its corresponding
-- `Language.LBNF.Grammar.Style` value.
str2Style :: String -> Language.LBNF.Grammar.Style
str2Style "Style_Bold"      = Style_Bold 
str2Style "Style_Italic"    = Style_Italic   
str2Style "Style_Underline" = Style_Underline  
str2Style "Style_Invert"    = Style_Invert  
str2Style "Style_Blink"     = Style_Blink
str2Style s = error "Invalid Style"

{-unlines $ [
  "instance Print " ++ own ++ " where",
  "  prt _ (" ++ own ++ posn ++ ") = doc (showString i)",
  ifList cf own
  ]
 where
   posn = if isPositionCat cf own then " (_,i)" else " i"
-}
-- copy and paste from CFtoTemplate

-- | Generates a list of Template Haskell declarations for pretty-printing
-- all data constructors in the `CF` grammar. This includes constructing
-- `Pretty` instances for every grammar rule based on their structure,
-- and annotations.
prettyrules :: CF -> [Q Dec]
prettyrules cf =
  map (\(s,xs) -> pretty_case_fun s (map toArgs xs) (prettyifList cf s) ann bgdef ) $ cf2data cf
 where
   toArgs (cons,Left args) = ((cons, names (map (checkRes . var) args) (0 :: Int)), ruleOf cons)
   toArgs (cons,Right reg) = ((cons, names ["s"] (0 :: Int)), ruleOf cons)
   names [] _ = []
   names (x:xs) n
     | elem x xs = (x ++ show n) : names xs (n+1)
     | otherwise = x             : names xs n
   var ('[':xs)  = var (init xs) ++ "s"
   var "Ident"   = "id"
   var "Integer" = "n"
   var "String"  = "str"
   var "Char"    = "c"
   var "Double"  = "d"
   var xs        = map toLower xs
   checkRes s
        | elem s reservedHaskell = s ++ "'"
        | otherwise              = s
   reservedHaskell = ["case","class","data","default","deriving","do","else","if",
                      "import","in","infix","infixl","infixr","instance","let","module",
                      "newtype","of","then","type","where","as","qualified","hiding"]
   ruleOf s = maybe undefined id $ lookup s (rulesOfCF cf)
   fgdef = case defaultFg cf of
                Left "" -> Value ""
                Left s -> (Fg . colMap . str2Col) s
                Right [r,g,b] -> rgb2fg (read r) (read g) (read b)
   bgdef = case defaultBg cf of
                Left "" -> Nothing
                Left s -> Just $ str2Col s
                Right [r,g,b] -> Just $ rgb2col (read r) (read g) (read b)
   styledef = map (styleMap . str2Style) $ defaultStyle cf
   ann = [fgdef] ++ styledef

-- | Constructs a `Pretty` instance for a specific grammar case, including
-- its constructors and annotations.
pretty_case_fun cat xs lst ann bgdef =
 instanceD (cxt []) (appT (conT ''Pretty) $ conT $ mkName cat) $
  (newName "i" >>= \i -> newName "x" >>= prtc i) : lst where
    prtc i n = funD 'pretty [clause [varP i,varP n] body []] where
      body = normalB $ caseE (varE n) $
        map mtch xs
      mtch ((c,xx),r) = match
        (conP (mkName c) [varP (mkName x)|x <- xx])
        (normalB
          [| annotate $(lift ann) (
                group (
                  ppPrec
                    $(varE i)
                    $(litE $ IntegerL $ toInteger $ precCat $ fst r)
                    $(prettymkRhs xx (snd r) bgdef)
                  ))
          |])
        []



{-
unlines [
  "instance Print" +++ cat +++ "where",
  "  prt i" +++ "e = case e of",
  unlines $ map (\ ((c,xx),r) ->
    "   " ++ c +++ unwords xx +++ "->" +++
    "prPrec i" +++ show (precCat (fst r)) +++ mkRhs xx (snd r)) xs
  ]
-}

  {-
 "(concatD [" ++ unwords (intersperse "," (mk args its)) ++ "])"
 where
  mk args (Left "#" : items)      = mk args items
  mk (arg:args) (Left c : items)  = (prt c +++ arg)        : mk args items
  mk args       (Right s : items) = ("doc (showString" +++ show s ++ ")") : mk args items
  mk _ _ = []
  prt c = "prt" +++ show (precCat c)
-}

-- | Generates pretty-printing rules for lists in the BNFC grammar. The rules
-- handle the following constructs:
-- 
-- 1. **Empty lists** (`nil`).
-- 2. **Single-element lists** (`one`).
-- 3. **Cons lists** (`cons`), where a head element is followed by the tail.
prettyifList :: CF -> String -> [DecQ]
prettyifList cf cat = mkListRule $ nil cat ++ one cat ++ cons cat where
  nil cat  = [(listP [],prettymkRhs [] its bgdef) |
                            (f,(c,its)) <- rulesOfCF cf, isNilFun f , normCatOfList c == cat]
  one cat  = [(listP [varP $ mkName "x"], prettymkRhs ["x"] its bgdef) |
                            (f,(c,its)) <- rulesOfCF cf, isOneFun f , normCatOfList c == cat]
  cons cat = [(conP '(:) [varP $ mkName "x",varP $ mkName "xs"], prettymkRhs ["x","xs"] its bgdef) |
                            (f,(c,its)) <- rulesOfCF cf, isConsFun f , normCatOfList c == cat]
  mkListRule [] = []
  mkListRule rs = [do
    es <- newName "es"
    funD 'prettyList [clause [varP es] (normalB $ caseE (varE es) $ map mtch rs) []]]
  mtch (p,e) = match p (normalB e) []
  bgdef = case defaultBg cf of
                Left "" -> Nothing
                Left s -> Just $ str2Col s
                Right [r,g,b] -> Just $ rgb2col (read r) (read g) (read b)

-- | Generates the right-hand side (RHS) of a pretty-printing rule.
prettymkRhs :: [String] -> Either [(Maybe [Annotation], Either String String)] a -> Maybe Language.LBNF.Grammar.Color -> ExpQ
prettymkRhs args (Left its) bg = [| 
  let docs = $(listE $ map snd (mk args its))
      anns = $(listE $ map (liftMaybeAnn . fst) (mk args its))
  in applyColor (isBackground bg) (prettyWithSpacing docs anns) 
  |]
 where
  -- | Helper function to construct annotated documents from a list of variable names
  -- and annotations or regular strings. Handles special cases for `"String"` and `"Char"`.
  mk :: [String] -> [(Maybe [Annotation], Either String String)] -> [(Maybe [Annotation], ExpQ)]
  mk args ((_,Left "#") : items)      = mk args items
  mk (arg:args) ((ann,Left "String") : items)  =
    (ann, [| dquotes $ pretty $(lift $ precCat "String") $(varE $ mkName arg) |]) : mk args items
  mk (arg:args) ((ann,Left "Char") : items)  =
    (ann, [| squotes $ pretty $(lift $ precCat "Char") $(varE $ mkName arg) |]) : mk args items
  mk (arg:args) ((ann,Left c) : items)  = (ann, [| pretty $(lift $ precCat c) $(varE $ mkName arg) |]) : mk args items
  mk args       ((ann,Right s) : items) = (ann, [| pretty 0 $(lift (s :: String)) |]) : mk args items
  mk _ _ = []

prettymkRhs args (Right reg) _ = [| pretty 0 ($(varE $ mkName "s"))|]

-- | Determines if parenthesis need to be placed around a document
-- depending on predecedence values.
ppPrec :: Int -> Int -> Doc -> Doc
ppPrec i j d = if j < i
               then parens d
               else d

-- | Converts a `Maybe Language.LBNF.Grammar.Color` value into a
-- background `Colored String` annotation. If the value is `Nothing`,
-- then an empty `Value` is returned.
isBackground :: Maybe Language.LBNF.Grammar.Color -> Colored String
isBackground Nothing = Value ""
isBackground (Just bg) = (Bg . colMap) bg

-- | Lifts a `Maybe` annotation into a Template Haskell `ExpQ` expression.
liftMaybeAnn :: Maybe [Annotation] -> ExpQ
liftMaybeAnn Nothing  = [| Nothing |]
liftMaybeAnn (Just a) = [| Just $(listE (map lift a)) |]

-- | Provides a `Lift` instance for the `Annotation` type, enabling
-- Template Haskell to generate expressions for annotations.
instance Lift Annotation where
  lift Annotation_Linebreak = [| Annotation_Linebreak |]
  lift Annotation_Empty     = [| Annotation_Empty |]
  lift (Annotation_3 i)     = [| Annotation_3 i |]
  lift (Annotation_4 i)     = [| Annotation_4 i |]
  lift (Annotation_5 j)     = [| Annotation_5 j |]
  lift (Annotation_6 j)     = [| Annotation_6 j |]
  lift (AnnotationStyle style)  = [| AnnotationStyle style |] 
  liftTyped = liftTyped 

-- | Provides a `Lift` instance for the `Color` type from
-- `Language.LBNF.Grammar`.
instance Lift Language.LBNF.Grammar.Color where
  lift Color_Red               = [| Color_Red |]
  lift Color_Blue              = [| Color_Blue |]
  lift Color_Green             = [| Color_Green |]
  lift Color_Yellow            = [| Color_Yellow |]
  lift Color_Cyan              = [| Color_Cyan |]
  lift Color_Magenta           = [| Color_Magenta |]
  lift Color_White             = [| Color_White |]
  lift Color_Black             = [| Color_Black |]
  lift (Color_9 r g b)         = [| Color_9 r g b |]
  liftTyped = liftTyped

-- | Provides a `Lift` instance for the `Style` type from
-- `Language.LBNF.Grammar`.
instance Lift Language.LBNF.Grammar.Style where
  lift Style_Bold      = [| Style_Bold |]
  lift Style_Italic    = [| Style_Italic |]
  lift Style_Underline = [| Style_Underline |]
  lift Style_Invert    = [| Style_Invert |]
  lift Style_Blink     = [| Style_Blink |]
  liftTyped = liftTyped

-- | Combines a list of documents (`[Doc]`) with their corresponding annotations
-- (`[Maybe [Annotation]]`) into a single formatted document. Spacing, annotations,
-- and line breaks are applied as specified by the annotations.
prettyWithSpacing :: [Doc] -> [Maybe [Annotation]] -> Doc
prettyWithSpacing [] _ = Empty
prettyWithSpacing [d] [Nothing] = d
prettyWithSpacing [d] [Just ann] = applyAnnotations d Empty ann True 0
prettyWithSpacing (d:ds) (Nothing:mas) = (applyAnnotations d Empty [] True 0) <#> prettyWithSpacing ds mas
prettyWithSpacing (d:ds) (Just anns:mas) = applyAnnotations d (prettyWithSpacing ds mas) (anns) False 0

-- | Recursively applies a list of annotations to a pair of documents (`d1`, `d2`),
-- combining them with proper formatting and spacing. Annotations such as line breaks,
-- spaces, and colors are applied as specified.
applyAnnotations :: Doc -> Doc -> [Annotation] -> Bool -> Int -> Doc
applyAnnotations d1 d2 [] False 0 = d1 <#> d2
applyAnnotations d1 d2 [] True 0 = d1 <> d2
applyAnnotations d1 d2 [] False i = Nest i $ d1 <#> d2
applyAnnotations d1 d2 [] True i = Nest i $ d1 <> d2
applyAnnotations d1 d2 (Annotation_Empty:as) _ n = applyAnnotations d1 d2 as True n
applyAnnotations d1 d2 (Annotation_Linebreak:as) _ n = applyAnnotations (d1 <> linebreak) d2 as True n
applyAnnotations d1 d2 ((Annotation_3 i):as) _ n = applyAnnotations (d1 <> mconcat (replicate (fromIntegral i) space)) d2 as True n
applyAnnotations d1 d2 ((Annotation_4 i):as) l j = applyAnnotations d1 d2 as l (j + fromIntegral i)
applyAnnotations d1 d2 (Annotation_5 (Color_9 r g b):as) l n =
  applyAnnotations (annotate [rgb2fg r g b] d1) d2 as l n
applyAnnotations d1 d2 (Annotation_5 col:as) l n =
  applyAnnotations (annotate [(Fg . colMap) col] d1) d2 as l n
applyAnnotations d1 d2 (Annotation_6 (Color_9 r g b):as) l n =
  applyAnnotations (annotate [rgb2bg r g b] d1) d2 as l n
applyAnnotations d1 d2 (Annotation_6 col:as) l n =
  applyAnnotations (annotate [(Bg . colMap) col] d1) d2 as l n  
applyAnnotations d1 d2 (AnnotationStyle s:as) l n =
  applyAnnotations (annotate [styleMap s] d1) d2 as l n            

-- | Converts red, green, and blue color components (as `Integer`s) into
-- a foreground `Colored String` value. The function maps the RGB components
-- to an `RGB` constructor and wraps it in the `Fg` type for foreground colors.
rgb2fg :: Integer -> Integer -> Integer -> Colored String
rgb2fg r g b = Fg (RGB (fromIntegral r) (fromIntegral g) (fromIntegral b))

-- | Converts red, green, and blue color components (as `Integer`s) into
-- a background `Colored String` value. Similar to `rgb2fg`, but wraps the
-- `RGB` constructor in the `Bg` type for background colors.
rgb2bg :: Integer -> Integer -> Integer -> Colored String
rgb2bg r g b = Bg (RGB (fromIntegral r) (fromIntegral g) (fromIntegral b))

-- | Converts red, green, and blue color components (as `Integer`s) into
-- a `Language.LBNF.Grammar.Color` value using the `Color_9` constructor.
rgb2col :: Integer -> Integer -> Integer -> Language.LBNF.Grammar.Color
rgb2col r g b = Color_9 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | Maps a `Language.LBNF.Grammar.Color` value to its corresponding
-- `Data.Monoid.Colorful.Flat.Color` representation in RGB format.
colMap :: Language.LBNF.Grammar.Color -> Data.Monoid.Colorful.Flat.Color
colMap Color_Red     = RGB 255 0 0
colMap Color_Blue    = RGB 0 0 255
colMap Color_Green   = RGB 0 255 0
colMap Color_Yellow  = RGB 255 255 0
colMap Color_Cyan    = RGB 0 255 255
colMap Color_Magenta = RGB 255 0 255
colMap Color_White   = RGB 255 255 255
colMap Color_Black   = RGB 0 0 0

-- | Maps a `Language.LBNF.Grammar.Style` value to a `Colored String`
-- representation, allowing styles to be applied to items during pretty-printing.
styleMap :: Language.LBNF.Grammar.Style -> Colored String
styleMap Style_Bold = Style Bold
styleMap Style_Italic = Style Italic
styleMap Style_Underline = Style Underline
styleMap Style_Invert = Style Invert
styleMap Style_Blink = Style Blink

-- | Applies a color annotation (`Colored String`) to a `Doc` structure,
-- optionally propagating the color to all subcomponents of the document.
-- The function traverses the document recursively and applies the annotation
-- wherever specified.
applyColor :: Colored String -> Doc -> Doc
applyColor color doc = case doc of
  Empty ->
    Empty
  Char c ->
    annotate [color] (Char c)
  Text l s ->
    annotate [color] (Text l s)
  Line ->
    Line
  FlatAlt d1 d2 ->
    FlatAlt (applyColor color d1) (applyColor color d2)
  Cat d1 d2 ->
    Cat (applyColor color d1) (applyColor color d2)
  Nest i d ->
    Nest i (applyColor color d)
  Union d1 d2 ->
    Union (applyColor color d1) (applyColor color d2)
  Annotate ann d ->
    Annotate ann (applyColor (getBg ann color) d)  -- Maintains annotation priority over defaults
  Column f ->
    Column (applyColor color . f)
  Nesting f ->
    Nesting (applyColor color . f)
  Columns f ->
    Columns (applyColor color . f)
  Ribbon f ->
    Ribbon (applyColor color . f)

 
getBg :: [Colored String] -> Colored String -> Colored String
getBg [] bg = bg
getBg (Bg bg' : _) _ = Bg bg'
getBg (_ : xs) bg = getBg xs bg

