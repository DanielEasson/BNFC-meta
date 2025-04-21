{-# LANGUAGE TemplateHaskell, DefaultSignatures , FlexibleInstances #-}
-- | Contains things that are typically needed in modules that use
-- languages defined using BNFC-meta.
module Language.LBNF.Runtime(
  -- * Happy and Alex runtimes
  -- ord
  -- , listArray
  -- , (!)
  -- , Array
  -- , parseToQuoter

  ParseMonad(..)
  , err

  -- * Pretty printing runtimes
  , printTree
  , Language.LBNF.Runtime.Doc
  , doc
  , concatD
  , Print(..)
  , prPrec
  , PrintPlain(..)
  , pprintTree
  , Language.LBNF.Runtime.Pretty(..)
  , showDocStructure

  ) where



import Control.Monad (MonadPlus(..), liftM, foldM, (>=>), ap)
import Control.Applicative ( Applicative(..) )
import qualified Control.Monad.Fail as Fail

import Data.Char

import System.IO (stdout, hPutStr)
import Text.PrettyPrint.Annotated.WL as PP
import Data.Monoid.Colorful.Flat
import Text.PrettyPrint.Console.WL as WL
--import Language.LBNF.CFtoPrettyPrinter(Pretty(..))

------------------
-- Lexing, Parsing
------------------

-- * The result of a parse.
data ParseMonad a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad ParseMonad where
  return      = Ok
  Ok a  >>= f = f a
  Bad s >>= f = Bad s

instance Fail.MonadFail ParseMonad where
  fail        = Bad

instance Functor ParseMonad where
  fmap = liftM

instance Applicative ParseMonad where
  (<*>) = ap
  pure = return

--instance MonadPlus ParseMonad where
--  mzero = Bad "Err.mzero"
--  mplus (Bad _) y = y
--  mplus x       _ = x

-- * An eliminator for a parse result. Takes a function that recovers from any
-- parse errors. Typical usage: @err error (pCategory (tokens s)) :: Category@
err :: (String -> a) -> ParseMonad a -> a
err e b = case b of
    Bad s -> e s
    Ok x  -> x


-----------
-- PRINTING
-----------

-- * Overloaded pretty-printer
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Language.LBNF.Runtime.Doc
doc = (:)

render :: Language.LBNF.Runtime.Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . rend i (")":ts)
    t  : "]" :ts -> showString t . rend i ("]":ts)
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Language.LBNF.Runtime.Doc -> Language.LBNF.Runtime.Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Language.LBNF.Runtime.Doc] -> Language.LBNF.Runtime.Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Language.LBNF.Runtime.Doc
  prtList :: [a] -> Language.LBNF.Runtime.Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Language.LBNF.Runtime.Doc -> Language.LBNF.Runtime.Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)
  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])


instance Print Double where
  prt _ x = doc (shows x)

newtype PrintPlain = MkPrintPlain String

instance Print PrintPlain where
  prt _ (MkPrintPlain s) = doc $ showString s 

-- | A type alias for pretty-printed documents. Each document is annotated
-- with metadata (e.g., colors or styles) via a list of `Colored String` elements.
--type PDoc = WL.Doc [Colored String]

-- | A type class for defining pretty-printing behavior for various types.
class Pretty a where
  -- | Converts a value of type `a` into a pretty-printed document, optionally
  -- taking a precedence level.
  pretty :: Int -> a -> WL.Doc [Colored String]

  -- | Converts a list of elements into a pretty-printed document.
  -- By default, this uses `pretty` to format each element and constructs
  -- a document list.
  prettyList :: [a] -> WL.Doc [Colored String]
  prettyList = list . map (Language.LBNF.Runtime.pretty 0)

  -- | A default implementation of `pretty` for types that have a `Show` instance.
  default pretty :: Show a => Int -> a -> WL.Doc [Colored String]
  pretty _ = text . show


-- | Pretty-print a list of elements by applying `pretty` to each element
-- and formatting them as a list.
instance Language.LBNF.Runtime.Pretty a => Language.LBNF.Runtime.Pretty [a] where
  pretty _ = Language.LBNF.Runtime.prettyList

-- | Directly use a `Doc` document for an identity case.
instance Language.LBNF.Runtime.Pretty (WL.Doc [Colored String]) where
  pretty _ = id

-- | Pretty-print the unit type `()` as the string "()" in a document.
instance Language.LBNF.Runtime.Pretty () where
  pretty _ () = text "()"

-- | Pretty-print characters. Handles newline characters by splitting
-- text into multiple lines within the document.
instance Language.LBNF.Runtime.Pretty Char where
  pretty _ = char
  prettyList "" = mempty
  prettyList ('\n':s) = line <> Language.LBNF.Runtime.prettyList s
  prettyList s = let (xs, ys) = span (/='\n') s in text xs <> Language.LBNF.Runtime.prettyList ys

-- | Instance of `Pretty` for the `Bool` type.
instance Language.LBNF.Runtime.Pretty Bool

-- | Instance of `Pretty` for the `Int` type.
instance Language.LBNF.Runtime.Pretty Int

-- | Instance of `Pretty` for the `Word` type.
instance Language.LBNF.Runtime.Pretty Word

-- | Instance of `Pretty` for the `Integer` type.
instance Language.LBNF.Runtime.Pretty Integer

-- | Instance of `Pretty` for the `Float` type.
instance Language.LBNF.Runtime.Pretty Float

-- | Instance of `Pretty` for the `Double` type.
instance Language.LBNF.Runtime.Pretty Double

-- | Pretty-prints an expression using the generated `Pretty` instances, with support
-- for colored and styled output. This function renders the output to the console
-- with the specified width and formatting options.
pprintTree :: Language.LBNF.Runtime.Pretty a => Int -> a -> IO ()
pprintTree w = hPrintColored hPutStr stdout TermRGB . displayColored id id . renderPretty 1 w . Language.LBNF.Runtime.pretty 0

-- | Converts a `Doc` structure into a general string representation.
-- Used for debugging purposes, does not pretty print the `Doc`.
showDocStructure :: WL.Doc [Colored String] -> String
showDocStructure doc = case doc of
    Empty -> "Empty"
    Char c -> "Char " ++ show c
    Text _ s -> "Text " ++ show s
    Line -> "Line"
    FlatAlt x y -> "FlatAlt (" ++ showDocStructure x ++ ") (" ++ showDocStructure y ++ ")"
    Cat x y -> "Cat (" ++ showDocStructure x ++ ") (" ++ showDocStructure y ++ ")"
    Nest i x -> "Nest " ++ show i ++ " (" ++ showDocStructure x ++ ")"
    Union x y -> "Union (" ++ showDocStructure x ++ ") (" ++ showDocStructure y ++ ")"
    Annotate a x -> "Annotate " ++ show a ++ " (" ++ showDocStructure x ++ ")"
    Column f -> "Column <function>"
    Nesting f -> "Nesting <function>"
    Columns f -> "Columns <function>"
    Ribbon f -> "Ribbon <function>"