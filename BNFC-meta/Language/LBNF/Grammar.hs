{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Language.LBNF.Grammar(Language.LBNF.Grammar.myLexer
  , Language.LBNF.Grammar.tokens
  , Language.LBNF.Grammar.pGrammar
  , Language.LBNF.Grammar.pListDef
  , Language.LBNF.Grammar.pListItem
  , Language.LBNF.Grammar.pDef
  , Language.LBNF.Grammar.pRHS
  , Language.LBNF.Grammar.pListRHS
  , Language.LBNF.Grammar.pItem
  , Language.LBNF.Grammar.pMAnnotations
  , Language.LBNF.Grammar.pListAnnotation
  , Language.LBNF.Grammar.pAnnotation
  , Language.LBNF.Grammar.pColor
  , Language.LBNF.Grammar.pStyle
  , Language.LBNF.Grammar.pCat
  , Language.LBNF.Grammar.pCat1
  , Language.LBNF.Grammar.pLabel
  , Language.LBNF.Grammar.pMIdent
  , Language.LBNF.Grammar.pHsTyp
  , Language.LBNF.Grammar.pHsTyp1
  , Language.LBNF.Grammar.pListHsTyp
  , Language.LBNF.Grammar.pArg
  , Language.LBNF.Grammar.pListArg
  , Language.LBNF.Grammar.pExp
  , Language.LBNF.Grammar.pExp1
  , Language.LBNF.Grammar.pExp2
  , Language.LBNF.Grammar.pListExp2
  , Language.LBNF.Grammar.pListExp
  , Language.LBNF.Grammar.pListString
  , Language.LBNF.Grammar.pMinimumSize
  , Language.LBNF.Grammar.pReg2
  , Language.LBNF.Grammar.pReg1
  , Language.LBNF.Grammar.pReg3
  , Language.LBNF.Grammar.pReg
  , Language.LBNF.Grammar.pListIdent
  , Language.LBNF.Grammar.qGrammar
  , Language.LBNF.Grammar.qListDef
  , Language.LBNF.Grammar.qListItem
  , Language.LBNF.Grammar.qDef
  , Language.LBNF.Grammar.qRHS
  , Language.LBNF.Grammar.qListRHS
  , Language.LBNF.Grammar.qItem
  , Language.LBNF.Grammar.qMAnnotations
  , Language.LBNF.Grammar.qListAnnotation
  , Language.LBNF.Grammar.qAnnotation
  , Language.LBNF.Grammar.qColor
  , Language.LBNF.Grammar.qStyle
  , Language.LBNF.Grammar.qCat
  , Language.LBNF.Grammar.qCat1
  , Language.LBNF.Grammar.qLabel
  , Language.LBNF.Grammar.qMIdent
  , Language.LBNF.Grammar.qHsTyp
  , Language.LBNF.Grammar.qHsTyp1
  , Language.LBNF.Grammar.qListHsTyp
  , Language.LBNF.Grammar.qArg
  , Language.LBNF.Grammar.qListArg
  , Language.LBNF.Grammar.qExp
  , Language.LBNF.Grammar.qExp1
  , Language.LBNF.Grammar.qExp2
  , Language.LBNF.Grammar.qListExp2
  , Language.LBNF.Grammar.qListExp
  , Language.LBNF.Grammar.qListString
  , Language.LBNF.Grammar.qMinimumSize
  , Language.LBNF.Grammar.qReg2
  , Language.LBNF.Grammar.qReg1
  , Language.LBNF.Grammar.qReg3
  , Language.LBNF.Grammar.qReg
  , Language.LBNF.Grammar.qListIdent
  , Language.LBNF.Grammar.grammar
  , Language.LBNF.Grammar.listDef
  , Language.LBNF.Grammar.listItem
  , Language.LBNF.Grammar.def
  , Language.LBNF.Grammar.rHS
  , Language.LBNF.Grammar.listRHS
  , Language.LBNF.Grammar.item
  , Language.LBNF.Grammar.mAnnotations
  , Language.LBNF.Grammar.listAnnotation
  , Language.LBNF.Grammar.annotation
  , Language.LBNF.Grammar.color
  , Language.LBNF.Grammar.style
  , Language.LBNF.Grammar.cat
  , Language.LBNF.Grammar.cat1
  , Language.LBNF.Grammar.label
  , Language.LBNF.Grammar.mIdent
  , Language.LBNF.Grammar.hsTyp
  , Language.LBNF.Grammar.hsTyp1
  , Language.LBNF.Grammar.listHsTyp
  , Language.LBNF.Grammar.arg
  , Language.LBNF.Grammar.listArg
  , Language.LBNF.Grammar.exp
  , Language.LBNF.Grammar.exp1
  , Language.LBNF.Grammar.exp2
  , Language.LBNF.Grammar.listExp2
  , Language.LBNF.Grammar.listExp
  , Language.LBNF.Grammar.listString
  , Language.LBNF.Grammar.minimumSize
  , Language.LBNF.Grammar.reg2
  , Language.LBNF.Grammar.reg1
  , Language.LBNF.Grammar.reg3
  , Language.LBNF.Grammar.reg
  , Language.LBNF.Grammar.listIdent
  , Language.LBNF.Grammar.Grammar(..)
  , Language.LBNF.Grammar.Def(..)
  , Language.LBNF.Grammar.RHS(..)
  , Language.LBNF.Grammar.Item(..)
  , Language.LBNF.Grammar.MAnnotations(..)
  , Language.LBNF.Grammar.Annotation(..)
  , Language.LBNF.Grammar.Color(..)
  , Language.LBNF.Grammar.Style(..)
  , Language.LBNF.Grammar.Cat(..)
  , Language.LBNF.Grammar.Label(..)
  , Language.LBNF.Grammar.MIdent(..)
  , Language.LBNF.Grammar.HsTyp(..)
  , Language.LBNF.Grammar.Arg(..)
  , Language.LBNF.Grammar.Exp(..)
  , Language.LBNF.Grammar.MinimumSize(..)
  , Language.LBNF.Grammar.Reg(..)
  , Language.LBNF.Grammar.Ident(..)) where
import Language.LBNF.Compiletime
import Language.Haskell.TH(loc_package,location)

data Grammar = Grammar ([Def]) deriving (Show, Eq, Ord)
data Def
    = Rule Label Cat RHS
    | Comment String
    | Comments String String
    | Internal Label Cat ([Item])
    | Token Ident Reg
    | PosToken Ident Reg
    | Entryp ([Ident])
    | Separator MinimumSize Cat String
    | Terminator MinimumSize Cat String
    | Coercions Ident Integer
    | Rules Ident ([RHS])
    | Function Ident ([Arg]) Exp
    | External Ident HsTyp
    | AntiQuote String String String
    | Derive ([Ident])
    | Foreground Color
    | Background Color
    | Style Style
    | Layout ([String])
    | LayoutStop ([String])
    | LayoutTop
    deriving (Show, Eq, Ord)
data RHS
    = RHS ([Item])
    | TRHS Reg
    deriving (Show, Eq, Ord)
data Item
    = Terminal String MAnnotations
    | NTerminal Cat MAnnotations
    deriving (Show, Eq, Ord)
data MAnnotations
    = JAnnotations ([Annotation])
    | NoAnnotations
    deriving (Show, Eq, Ord)
data Annotation
    = Annotation_Linebreak
    | Annotation_Empty
    | Annotation_3 Integer
    | Annotation_4 Integer
    | Annotation_5 Color
    | Annotation_6 Color
    | AnnotationStyle Style
    deriving (Show, Eq, Ord)
data Color
    = Color_Red
    | Color_Blue
    | Color_Green
    | Color_Yellow
    | Color_Cyan
    | Color_Magenta
    | Color_White
    | Color_Black
    | Color_9 Integer Integer Integer
    deriving (Show, Eq, Ord)
data Style
    = Style_Bold
    | Style_Italic
    | Style_Underline
    | Style_Invert
    | Style_Blink
    deriving (Show, Eq, Ord)
data Cat
    = OptCat Cat
    | ListCat Cat
    | IdCat Ident
    deriving (Show, Eq, Ord)
data Label
    = Id Ident
    | Wild
    | ListE
    | ListCons
    | ListOne
    | Aq MIdent
    deriving (Show, Eq, Ord)
data MIdent
    = JIdent Ident
    | NIdent
    deriving (Show, Eq, Ord)
data HsTyp
    = HsApp HsTyp HsTyp
    | HsCon Ident
    | HsTup ([HsTyp])
    | HsList HsTyp
    deriving (Show, Eq, Ord)
data Arg = Arg Ident deriving (Show, Eq, Ord)
data Exp
    = Cons Exp Exp
    | App Ident ([Exp])
    | Var Ident
    | LitInt Integer
    | LitChar Char
    | LitString String
    | LitDouble Double
    | List ([Exp])
    deriving (Show, Eq, Ord)
data MinimumSize
    = MNonempty
    | MEmpty
    deriving (Show, Eq, Ord)
data Reg
    = RSeq Reg Reg
    | RAlt Reg Reg
    | RMinus Reg Reg
    | RStar Reg
    | RPlus Reg
    | ROpt Reg
    | REps
    | RChar Char
    | RAlts String
    | RSeqs String
    | RDigit
    | RLetter
    | RUpper
    | RLower
    | RAny
    deriving (Show, Eq, Ord)
newtype Ident = Ident String deriving (Show, Eq, Ord)
instance Print Ident
    where {prt _ (Ident i_0) = doc (showString i_0)}
instance Print Grammar
    where {prt i_1 x_2 = case x_2 of
                                               {Grammar defs -> prPrec i_1 0 (concatD [prt 0 defs])}}
instance Print Def
    where {prt i_3 x_4 = case x_4 of
                                               {Rule label
                                                     cat
                                                     rhs -> prPrec i_3 0 (concatD [prt 0 label,
                                                                                                                               doc (showString ['.']),
                                                                                                                               prt 0 cat,
                                                                                                                               doc (showString [':',
                                                                                                                                                                               ':',
                                                                                                                                                                               '=']),
                                                                                                                               prt 0 rhs]);
                                                Comment str -> prPrec i_3 0 (concatD [doc (showString ['c',
                                                                                                                                                                                  'o',
                                                                                                                                                                                  'm',
                                                                                                                                                                                  'm',
                                                                                                                                                                                  'e',
                                                                                                                                                                                  'n',
                                                                                                                                                                                  't']),
                                                                                                                                  prt 0 str]);
                                                Comments str0
                                                         str -> prPrec i_3 0 (concatD [doc (showString ['c',
                                                                                                                                                                                   'o',
                                                                                                                                                                                   'm',
                                                                                                                                                                                   'm',
                                                                                                                                                                                   'e',
                                                                                                                                                                                   'n',
                                                                                                                                                                                   't']),
                                                                                                                                   prt 0 str0,
                                                                                                                                   prt 0 str]);
                                                Internal label
                                                         cat
                                                         items -> prPrec i_3 0 (concatD [doc (showString ['i',
                                                                                                                                                                                     'n',
                                                                                                                                                                                     't',
                                                                                                                                                                                     'e',
                                                                                                                                                                                     'r',
                                                                                                                                                                                     'n',
                                                                                                                                                                                     'a',
                                                                                                                                                                                     'l']),
                                                                                                                                     prt 0 label,
                                                                                                                                     doc (showString ['.']),
                                                                                                                                     prt 0 cat,
                                                                                                                                     doc (showString [':',
                                                                                                                                                                                     ':',
                                                                                                                                                                                     '=']),
                                                                                                                                     prt 0 items]);
                                                Token id
                                                      reg -> prPrec i_3 0 (concatD [doc (showString ['t',
                                                                                                                                                                                'o',
                                                                                                                                                                                'k',
                                                                                                                                                                                'e',
                                                                                                                                                                                'n']),
                                                                                                                                prt 0 id,
                                                                                                                                prt 0 reg]);
                                                PosToken id
                                                         reg -> prPrec i_3 0 (concatD [doc (showString ['p',
                                                                                                                                                                                   'o',
                                                                                                                                                                                   's',
                                                                                                                                                                                   'i',
                                                                                                                                                                                   't',
                                                                                                                                                                                   'i',
                                                                                                                                                                                   'o',
                                                                                                                                                                                   'n']),
                                                                                                                                   doc (showString ['t',
                                                                                                                                                                                   'o',
                                                                                                                                                                                   'k',
                                                                                                                                                                                   'e',
                                                                                                                                                                                   'n']),
                                                                                                                                   prt 0 id,
                                                                                                                                   prt 0 reg]);
                                                Entryp ids -> prPrec i_3 0 (concatD [doc (showString ['e',
                                                                                                                                                                                 'n',
                                                                                                                                                                                 't',
                                                                                                                                                                                 'r',
                                                                                                                                                                                 'y',
                                                                                                                                                                                 'p',
                                                                                                                                                                                 'o',
                                                                                                                                                                                 'i',
                                                                                                                                                                                 'n',
                                                                                                                                                                                 't',
                                                                                                                                                                                 's']),
                                                                                                                                 prt 0 ids]);
                                                Separator minimumsize
                                                          cat
                                                          str -> prPrec i_3 0 (concatD [doc (showString ['s',
                                                                                                                                                                                    'e',
                                                                                                                                                                                    'p',
                                                                                                                                                                                    'a',
                                                                                                                                                                                    'r',
                                                                                                                                                                                    'a',
                                                                                                                                                                                    't',
                                                                                                                                                                                    'o',
                                                                                                                                                                                    'r']),
                                                                                                                                    prt 0 minimumsize,
                                                                                                                                    prt 0 cat,
                                                                                                                                    prt 0 str]);
                                                Terminator minimumsize
                                                           cat
                                                           str -> prPrec i_3 0 (concatD [doc (showString ['t',
                                                                                                                                                                                     'e',
                                                                                                                                                                                     'r',
                                                                                                                                                                                     'm',
                                                                                                                                                                                     'i',
                                                                                                                                                                                     'n',
                                                                                                                                                                                     'a',
                                                                                                                                                                                     't',
                                                                                                                                                                                     'o',
                                                                                                                                                                                     'r']),
                                                                                                                                     prt 0 minimumsize,
                                                                                                                                     prt 0 cat,
                                                                                                                                     prt 0 str]);
                                                Coercions id
                                                          n -> prPrec i_3 0 (concatD [doc (showString ['c',
                                                                                                                                                                                  'o',
                                                                                                                                                                                  'e',
                                                                                                                                                                                  'r',
                                                                                                                                                                                  'c',
                                                                                                                                                                                  'i',
                                                                                                                                                                                  'o',
                                                                                                                                                                                  'n',
                                                                                                                                                                                  's']),
                                                                                                                                  prt 0 id,
                                                                                                                                  prt 0 n]);
                                                Rules id
                                                      rhss -> prPrec i_3 0 (concatD [doc (showString ['r',
                                                                                                                                                                                 'u',
                                                                                                                                                                                 'l',
                                                                                                                                                                                 'e',
                                                                                                                                                                                 's']),
                                                                                                                                 prt 0 id,
                                                                                                                                 doc (showString [':',
                                                                                                                                                                                 ':',
                                                                                                                                                                                 '=']),
                                                                                                                                 prt 0 rhss]);
                                                Function id
                                                         args
                                                         exp -> prPrec i_3 0 (concatD [doc (showString ['d',
                                                                                                                                                                                   'e',
                                                                                                                                                                                   'f',
                                                                                                                                                                                   'i',
                                                                                                                                                                                   'n',
                                                                                                                                                                                   'e']),
                                                                                                                                   prt 0 id,
                                                                                                                                   prt 0 args,
                                                                                                                                   doc (showString ['=']),
                                                                                                                                   prt 0 exp]);
                                                External id
                                                         hstyp -> prPrec i_3 0 (concatD [doc (showString ['e',
                                                                                                                                                                                     'x',
                                                                                                                                                                                     't',
                                                                                                                                                                                     'e',
                                                                                                                                                                                     'r',
                                                                                                                                                                                     'n',
                                                                                                                                                                                     'a',
                                                                                                                                                                                     'l']),
                                                                                                                                     prt 0 id,
                                                                                                                                     doc (showString ['=']),
                                                                                                                                     prt 0 hstyp]);
                                                AntiQuote str0
                                                          str1
                                                          str -> prPrec i_3 0 (concatD [doc (showString ['a',
                                                                                                                                                                                    'n',
                                                                                                                                                                                    't',
                                                                                                                                                                                    'i',
                                                                                                                                                                                    'q',
                                                                                                                                                                                    'u',
                                                                                                                                                                                    'o',
                                                                                                                                                                                    't',
                                                                                                                                                                                    'e']),
                                                                                                                                    prt 0 str0,
                                                                                                                                    prt 0 str1,
                                                                                                                                    prt 0 str]);
                                                Derive ids -> prPrec i_3 0 (concatD [doc (showString ['d',
                                                                                                                                                                                 'e',
                                                                                                                                                                                 'r',
                                                                                                                                                                                 'i',
                                                                                                                                                                                 'v',
                                                                                                                                                                                 'e']),
                                                                                                                                 prt 0 ids]);
                                                Foreground color -> prPrec i_3 0 (concatD [doc (showString ['D',
                                                                                                                                                                                       'e',
                                                                                                                                                                                       'f',
                                                                                                                                                                                       'a',
                                                                                                                                                                                       'u',
                                                                                                                                                                                       'l',
                                                                                                                                                                                       't',
                                                                                                                                                                                       'F',
                                                                                                                                                                                       'g']),
                                                                                                                                       prt 0 color]);
                                                Background color -> prPrec i_3 0 (concatD [doc (showString ['D',
                                                                                                                                                                                       'e',
                                                                                                                                                                                       'f',
                                                                                                                                                                                       'a',
                                                                                                                                                                                       'u',
                                                                                                                                                                                       'l',
                                                                                                                                                                                       't',
                                                                                                                                                                                       'B',
                                                                                                                                                                                       'g']),
                                                                                                                                       prt 0 color]);
                                                Style style -> prPrec i_3 0 (concatD [doc (showString ['D',
                                                                                                                                                                                  'e',
                                                                                                                                                                                  'f',
                                                                                                                                                                                  'a',
                                                                                                                                                                                  'u',
                                                                                                                                                                                  'l',
                                                                                                                                                                                  't',
                                                                                                                                                                                  'S',
                                                                                                                                                                                  't',
                                                                                                                                                                                  'y',
                                                                                                                                                                                  'l',
                                                                                                                                                                                  'e']),
                                                                                                                                  prt 0 style]);
                                                Layout strs -> prPrec i_3 0 (concatD [doc (showString ['l',
                                                                                                                                                                                  'a',
                                                                                                                                                                                  'y',
                                                                                                                                                                                  'o',
                                                                                                                                                                                  'u',
                                                                                                                                                                                  't']),
                                                                                                                                  prt 0 strs]);
                                                LayoutStop strs -> prPrec i_3 0 (concatD [doc (showString ['l',
                                                                                                                                                                                      'a',
                                                                                                                                                                                      'y',
                                                                                                                                                                                      'o',
                                                                                                                                                                                      'u',
                                                                                                                                                                                      't']),
                                                                                                                                      doc (showString ['s',
                                                                                                                                                                                      't',
                                                                                                                                                                                      'o',
                                                                                                                                                                                      'p']),
                                                                                                                                      prt 0 strs]);
                                                LayoutTop -> prPrec i_3 0 (concatD [doc (showString ['l',
                                                                                                                                                                                'a',
                                                                                                                                                                                'y',
                                                                                                                                                                                'o',
                                                                                                                                                                                'u',
                                                                                                                                                                                't']),
                                                                                                                                doc (showString ['t',
                                                                                                                                                                                'o',
                                                                                                                                                                                'p',
                                                                                                                                                                                'l',
                                                                                                                                                                                'e',
                                                                                                                                                                                'v',
                                                                                                                                                                                'e',
                                                                                                                                                                                'l'])])};
           prtList es_5 = case es_5 of
                                                {[] -> concatD [];
                                                 [x] -> concatD [prt 0 x];
                                                 (:) x
                                                               xs -> concatD [prt 0 x,
                                                                                                    doc (showString [';']),
                                                                                                    prt 0 xs]}}
instance Print RHS
    where {prt i_6 x_7 = case x_7 of
                                               {RHS items -> prPrec i_6 0 (concatD [prt 0 items]);
                                                TRHS reg -> prPrec i_6 0 (concatD [doc (showString ['@']),
                                                                                                                               prt 0 reg])};
           prtList es_8 = case es_8 of
                                                {[x] -> concatD [prt 0 x];
                                                 (:) x
                                                               xs -> concatD [prt 0 x,
                                                                                                    doc (showString ['|']),
                                                                                                    prt 0 xs]}}
instance Print Item
    where {prt i_9 x_10 = case x_10 of
                                                {Terminal str
                                                          mannotations -> prPrec i_9 0 (concatD [prt 0 str,
                                                                                                                                             prt 0 mannotations]);
                                                 NTerminal cat
                                                           mannotations -> prPrec i_9 0 (concatD [prt 0 cat,
                                                                                                                                              prt 0 mannotations])};
           prtList es_11 = case es_11 of
                                                 {[] -> concatD [];
                                                  (:) x
                                                                xs -> concatD [prt 0 x,
                                                                                                     prt 0 xs]}}
instance Print MAnnotations
    where {prt i_12 x_13 = case x_13 of
                                                 {JAnnotations annotations -> prPrec i_12 0 (concatD [doc (showString ['(']),
                                                                                                                                                  prt 0 annotations,
                                                                                                                                                  doc (showString [')'])]);
                                                  NoAnnotations -> prPrec i_12 0 (concatD [])}}
instance Print Annotation
    where {prt i_14 x_15 = case x_15 of
                                                 {Annotation_Linebreak -> prPrec i_14 0 (concatD [doc (showString ['L',
                                                                                                                                                                                              'i',
                                                                                                                                                                                              'n',
                                                                                                                                                                                              'e',
                                                                                                                                                                                              'b',
                                                                                                                                                                                              'r',
                                                                                                                                                                                              'e',
                                                                                                                                                                                              'a',
                                                                                                                                                                                              'k'])]);
                                                  Annotation_Empty -> prPrec i_14 0 (concatD [doc (showString ['E',
                                                                                                                                                                                          'm',
                                                                                                                                                                                          'p',
                                                                                                                                                                                          't',
                                                                                                                                                                                          'y'])]);
                                                  Annotation_3 n -> prPrec i_14 0 (concatD [doc (showString ['S',
                                                                                                                                                                                        'p',
                                                                                                                                                                                        'a',
                                                                                                                                                                                        'c',
                                                                                                                                                                                        'e']),
                                                                                                                                        prt 0 n]);
                                                  Annotation_4 n -> prPrec i_14 0 (concatD [doc (showString ['N',
                                                                                                                                                                                        'e',
                                                                                                                                                                                        's',
                                                                                                                                                                                        't']),
                                                                                                                                        prt 0 n]);
                                                  Annotation_5 color -> prPrec i_14 0 (concatD [doc (showString ['F',
                                                                                                                                                                                            'g']),
                                                                                                                                            prt 0 color]);
                                                  Annotation_6 color -> prPrec i_14 0 (concatD [doc (showString ['B',
                                                                                                                                                                                            'g']),
                                                                                                                                            prt 0 color]);
                                                  AnnotationStyle style -> prPrec i_14 0 (concatD [prt 0 style])};
           prtList es_16 = case es_16 of
                                                 {[x] -> concatD [prt 0 x];
                                                  (:) x
                                                                xs -> concatD [prt 0 x,
                                                                                                     doc (showString [',']),
                                                                                                     prt 0 xs]}}
instance Print Color
    where {prt i_17 x_18 = case x_18 of
                                                 {Color_Red -> prPrec i_17 0 (concatD [doc (showString ['R',
                                                                                                                                                                                   'e',
                                                                                                                                                                                   'd'])]);
                                                  Color_Blue -> prPrec i_17 0 (concatD [doc (showString ['B',
                                                                                                                                                                                    'l',
                                                                                                                                                                                    'u',
                                                                                                                                                                                    'e'])]);
                                                  Color_Green -> prPrec i_17 0 (concatD [doc (showString ['G',
                                                                                                                                                                                     'r',
                                                                                                                                                                                     'e',
                                                                                                                                                                                     'e',
                                                                                                                                                                                     'n'])]);
                                                  Color_Yellow -> prPrec i_17 0 (concatD [doc (showString ['Y',
                                                                                                                                                                                      'e',
                                                                                                                                                                                      'l',
                                                                                                                                                                                      'l',
                                                                                                                                                                                      'o',
                                                                                                                                                                                      'w'])]);
                                                  Color_Cyan -> prPrec i_17 0 (concatD [doc (showString ['C',
                                                                                                                                                                                    'y',
                                                                                                                                                                                    'a',
                                                                                                                                                                                    'n'])]);
                                                  Color_Magenta -> prPrec i_17 0 (concatD [doc (showString ['M',
                                                                                                                                                                                       'a',
                                                                                                                                                                                       'g',
                                                                                                                                                                                       'e',
                                                                                                                                                                                       'n',
                                                                                                                                                                                       't',
                                                                                                                                                                                       'a'])]);
                                                  Color_White -> prPrec i_17 0 (concatD [doc (showString ['W',
                                                                                                                                                                                     'h',
                                                                                                                                                                                     'i',
                                                                                                                                                                                     't',
                                                                                                                                                                                     'e'])]);
                                                  Color_Black -> prPrec i_17 0 (concatD [doc (showString ['B',
                                                                                                                                                                                     'l',
                                                                                                                                                                                     'a',
                                                                                                                                                                                     'c',
                                                                                                                                                                                     'k'])]);
                                                  Color_9 n0
                                                          n1
                                                          n -> prPrec i_17 0 (concatD [doc (showString ['R',
                                                                                                                                                                                   'G',
                                                                                                                                                                                   'B']),
                                                                                                                                   prt 0 n0,
                                                                                                                                   prt 0 n1,
                                                                                                                                   prt 0 n])}}
instance Print Style
    where {prt i_19 x_20 = case x_20 of
                                                 {Style_Bold -> prPrec i_19 0 (concatD [doc (showString ['B',
                                                                                                                                                                                    'o',
                                                                                                                                                                                    'l',
                                                                                                                                                                                    'd'])]);
                                                  Style_Italic -> prPrec i_19 0 (concatD [doc (showString ['I',
                                                                                                                                                                                      't',
                                                                                                                                                                                      'a',
                                                                                                                                                                                      'l',
                                                                                                                                                                                      'i',
                                                                                                                                                                                      'c'])]);
                                                  Style_Underline -> prPrec i_19 0 (concatD [doc (showString ['U',
                                                                                                                                                                                         'n',
                                                                                                                                                                                         'd',
                                                                                                                                                                                         'e',
                                                                                                                                                                                         'r',
                                                                                                                                                                                         'l',
                                                                                                                                                                                         'i',
                                                                                                                                                                                         'n',
                                                                                                                                                                                         'e'])]);
                                                  Style_Invert -> prPrec i_19 0 (concatD [doc (showString ['I',
                                                                                                                                                                                      'n',
                                                                                                                                                                                      'v',
                                                                                                                                                                                      'e',
                                                                                                                                                                                      'r',
                                                                                                                                                                                      't'])]);
                                                  Style_Blink -> prPrec i_19 0 (concatD [doc (showString ['B',
                                                                                                                                                                                     'l',
                                                                                                                                                                                     'i',
                                                                                                                                                                                     'n',
                                                                                                                                                                                     'k'])])}}
instance Print Cat
    where {prt i_21 x_22 = case x_22 of
                                                 {OptCat cat -> prPrec i_21 0 (concatD [doc (showString ['?']),
                                                                                                                                    prt 1 cat]);
                                                  ListCat cat -> prPrec i_21 1 (concatD [doc (showString ['[']),
                                                                                                                                     prt 0 cat,
                                                                                                                                     doc (showString [']'])]);
                                                  IdCat id -> prPrec i_21 1 (concatD [prt 0 id])}}
instance Print Label
    where {prt i_23 x_24 = case x_24 of
                                                 {Id id -> prPrec i_23 0 (concatD [prt 0 id]);
                                                  Wild -> prPrec i_23 0 (concatD [doc (showString ['_'])]);
                                                  ListE -> prPrec i_23 0 (concatD [doc (showString ['[']),
                                                                                                                               doc (showString [']'])]);
                                                  ListCons -> prPrec i_23 0 (concatD [doc (showString ['(']),
                                                                                                                                  doc (showString [':']),
                                                                                                                                  doc (showString [')'])]);
                                                  ListOne -> prPrec i_23 0 (concatD [doc (showString ['(']),
                                                                                                                                 doc (showString [':']),
                                                                                                                                 doc (showString ['[']),
                                                                                                                                 doc (showString [']']),
                                                                                                                                 doc (showString [')'])]);
                                                  Aq mident -> prPrec i_23 0 (concatD [doc (showString ['$']),
                                                                                                                                   prt 0 mident])}}
instance Print MIdent
    where {prt i_25 x_26 = case x_26 of
                                                 {JIdent id -> prPrec i_25 0 (concatD [prt 0 id]);
                                                  NIdent -> prPrec i_25 0 (concatD [])}}
instance Print HsTyp
    where {prt i_27 x_28 = case x_28 of
                                                 {HsApp hstyp0
                                                        hstyp -> prPrec i_27 0 (concatD [prt 0 hstyp0,
                                                                                                                                     prt 1 hstyp]);
                                                  HsCon id -> prPrec i_27 1 (concatD [prt 0 id]);
                                                  HsTup hstyps -> prPrec i_27 1 (concatD [doc (showString ['(']),
                                                                                                                                      prt 0 hstyps,
                                                                                                                                      doc (showString [')'])]);
                                                  HsList hstyp -> prPrec i_27 1 (concatD [doc (showString ['[']),
                                                                                                                                      prt 0 hstyp,
                                                                                                                                      doc (showString [']'])])};
           prtList es_29 = case es_29 of
                                                 {[x] -> concatD [prt 0 x];
                                                  (:) x
                                                                xs -> concatD [prt 0 x,
                                                                                                     doc (showString [',']),
                                                                                                     prt 0 xs]}}
instance Print Arg
    where {prt i_30 x_31 = case x_31 of
                                                 {Arg id -> prPrec i_30 0 (concatD [prt 0 id])};
           prtList es_32 = case es_32 of
                                                 {[] -> concatD [];
                                                  (:) x
                                                                xs -> concatD [prt 0 x,
                                                                                                     prt 0 xs]}}
instance Print Exp
    where {prt i_33 x_34 = case x_34 of
                                                 {Cons exp0
                                                       exp -> prPrec i_33 0 (concatD [prt 1 exp0,
                                                                                                                                  doc (showString [':']),
                                                                                                                                  prt 0 exp]);
                                                  App id
                                                      exps -> prPrec i_33 1 (concatD [prt 0 id,
                                                                                                                                  prt 2 exps]);
                                                  Var id -> prPrec i_33 2 (concatD [prt 0 id]);
                                                  LitInt n -> prPrec i_33 2 (concatD [prt 0 n]);
                                                  LitChar c -> prPrec i_33 2 (concatD [prt 0 c]);
                                                  LitString str -> prPrec i_33 2 (concatD [prt 0 str]);
                                                  LitDouble d -> prPrec i_33 2 (concatD [prt 0 d]);
                                                  List exps -> prPrec i_33 2 (concatD [doc (showString ['[']),
                                                                                                                                   prt 0 exps,
                                                                                                                                   doc (showString [']'])])};
           prtList es_35 = case es_35 of
                                                 {[] -> concatD [];
                                                  [x] -> concatD [prt 2 x];
                                                  [x] -> concatD [prt 0 x];
                                                  (:) x
                                                                xs -> concatD [prt 2 x,
                                                                                                     prt 2 xs];
                                                  (:) x
                                                                xs -> concatD [prt 0 x,
                                                                                                     doc (showString [',']),
                                                                                                     prt 0 xs]}}
instance Print MinimumSize
    where {prt i_36 x_37 = case x_37 of
                                                 {MNonempty -> prPrec i_36 0 (concatD [doc (showString ['n',
                                                                                                                                                                                   'o',
                                                                                                                                                                                   'n',
                                                                                                                                                                                   'e',
                                                                                                                                                                                   'm',
                                                                                                                                                                                   'p',
                                                                                                                                                                                   't',
                                                                                                                                                                                   'y'])]);
                                                  MEmpty -> prPrec i_36 0 (concatD [])}}
instance Print Reg
    where {prt i_38 x_39 = case x_39 of
                                                 {RSeq reg0
                                                       reg -> prPrec i_38 2 (concatD [prt 2 reg0,
                                                                                                                                  prt 3 reg]);
                                                  RAlt reg0
                                                       reg -> prPrec i_38 1 (concatD [prt 1 reg0,
                                                                                                                                  doc (showString ['|']),
                                                                                                                                  prt 2 reg]);
                                                  RMinus reg0
                                                         reg -> prPrec i_38 1 (concatD [prt 2 reg0,
                                                                                                                                    doc (showString ['-']),
                                                                                                                                    prt 2 reg]);
                                                  RStar reg -> prPrec i_38 3 (concatD [prt 3 reg,
                                                                                                                                   doc (showString ['*'])]);
                                                  RPlus reg -> prPrec i_38 3 (concatD [prt 3 reg,
                                                                                                                                   doc (showString ['+'])]);
                                                  ROpt reg -> prPrec i_38 3 (concatD [prt 3 reg,
                                                                                                                                  doc (showString ['?'])]);
                                                  REps -> prPrec i_38 3 (concatD [doc (showString ['e',
                                                                                                                                                                              'p',
                                                                                                                                                                              's'])]);
                                                  RChar c -> prPrec i_38 3 (concatD [prt 0 c]);
                                                  RAlts str -> prPrec i_38 3 (concatD [doc (showString ['[']),
                                                                                                                                   prt 0 str,
                                                                                                                                   doc (showString [']'])]);
                                                  RSeqs str -> prPrec i_38 3 (concatD [doc (showString ['{']),
                                                                                                                                   prt 0 str,
                                                                                                                                   doc (showString ['}'])]);
                                                  RDigit -> prPrec i_38 3 (concatD [doc (showString ['d',
                                                                                                                                                                                'i',
                                                                                                                                                                                'g',
                                                                                                                                                                                'i',
                                                                                                                                                                                't'])]);
                                                  RLetter -> prPrec i_38 3 (concatD [doc (showString ['l',
                                                                                                                                                                                 'e',
                                                                                                                                                                                 't',
                                                                                                                                                                                 't',
                                                                                                                                                                                 'e',
                                                                                                                                                                                 'r'])]);
                                                  RUpper -> prPrec i_38 3 (concatD [doc (showString ['u',
                                                                                                                                                                                'p',
                                                                                                                                                                                'p',
                                                                                                                                                                                'e',
                                                                                                                                                                                'r'])]);
                                                  RLower -> prPrec i_38 3 (concatD [doc (showString ['l',
                                                                                                                                                                                'o',
                                                                                                                                                                                'w',
                                                                                                                                                                                'e',
                                                                                                                                                                                'r'])]);
                                                  RAny -> prPrec i_38 3 (concatD [doc (showString ['c',
                                                                                                                                                                              'h',
                                                                                                                                                                              'a',
																																											  'r'])])}} 

grammar = Language.LBNF.Compiletime.parseToQuoter (qGrammar . myLexer)
listDef = Language.LBNF.Compiletime.parseToQuoter (qListDef . myLexer)
listItem = Language.LBNF.Compiletime.parseToQuoter (qListItem . myLexer)
def = Language.LBNF.Compiletime.parseToQuoter (qDef . myLexer)
rHS = Language.LBNF.Compiletime.parseToQuoter (qRHS . myLexer)
listRHS = Language.LBNF.Compiletime.parseToQuoter (qListRHS . myLexer)
item = Language.LBNF.Compiletime.parseToQuoter (qItem . myLexer)
mAnnotations = Language.LBNF.Compiletime.parseToQuoter (qMAnnotations . myLexer)
listAnnotation = Language.LBNF.Compiletime.parseToQuoter (qListAnnotation . myLexer)
annotation = Language.LBNF.Compiletime.parseToQuoter (qAnnotation . myLexer)
color = Language.LBNF.Compiletime.parseToQuoter (qColor . myLexer)
style = Language.LBNF.Compiletime.parseToQuoter (qStyle . myLexer)
cat = Language.LBNF.Compiletime.parseToQuoter (qCat . myLexer)
cat1 = Language.LBNF.Compiletime.parseToQuoter (qCat1 . myLexer)
label = Language.LBNF.Compiletime.parseToQuoter (qLabel . myLexer)
mIdent = Language.LBNF.Compiletime.parseToQuoter (qMIdent . myLexer)
hsTyp = Language.LBNF.Compiletime.parseToQuoter (qHsTyp . myLexer)
hsTyp1 = Language.LBNF.Compiletime.parseToQuoter (qHsTyp1 . myLexer)
listHsTyp = Language.LBNF.Compiletime.parseToQuoter (qListHsTyp . myLexer)
arg = Language.LBNF.Compiletime.parseToQuoter (qArg . myLexer)
listArg = Language.LBNF.Compiletime.parseToQuoter (qListArg . myLexer)
exp = Language.LBNF.Compiletime.parseToQuoter (qExp . myLexer)
exp1 = Language.LBNF.Compiletime.parseToQuoter (qExp1 . myLexer)
exp2 = Language.LBNF.Compiletime.parseToQuoter (qExp2 . myLexer)
listExp2 = Language.LBNF.Compiletime.parseToQuoter (qListExp2 . myLexer)
listExp = Language.LBNF.Compiletime.parseToQuoter (qListExp . myLexer)
listString = Language.LBNF.Compiletime.parseToQuoter (qListString . myLexer)
minimumSize = Language.LBNF.Compiletime.parseToQuoter (qMinimumSize . myLexer)
reg2 = Language.LBNF.Compiletime.parseToQuoter (qReg2 . myLexer)
reg1 = Language.LBNF.Compiletime.parseToQuoter (qReg1 . myLexer)
reg3 = Language.LBNF.Compiletime.parseToQuoter (qReg3 . myLexer)
reg = Language.LBNF.Compiletime.parseToQuoter (qReg . myLexer)
listIdent = Language.LBNF.Compiletime.parseToQuoter (qListIdent . myLexer)
alex_base :: Array Int Int
alex_base = listArray (0,63) [-8,74,320,-55,-32,448,694,47,822,950,1078,1206,1334,1462,1575,0,1703,0,1816,0,1929,0,175,0,549,0,1994,2250,2186,0,0,2299,2555,2673,2737,0,2993,84,2929,0,0,2994,-37,76,92,3240,2561,3496,3432,0,3678,3924,0,141,-36,-35,0,-33,4141,0,0,129,3203,283]

alex_table :: Array Int Int
alex_table = listArray (0,4396) [0,53,53,53,53,53,56,60,44,50,6,63,63,63,63,63,63,63,63,63,63,0,0,0,53,3,45,0,56,0,0,33,56,56,56,56,56,54,56,0,61,61,61,61,61,61,61,61,61,61,57,56,0,56,0,56,56,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,56,-1,56,4,56,0,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,55,56,56,45,2,0,0,0,45,62,62,62,62,62,62,62,62,62,62,0,0,0,0,0,4,63,63,63,63,63,63,63,63,63,63,53,53,53,53,53,0,0,4,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,53,0,43,45,61,61,61,61,61,61,61,61,61,61,46,0,0,0,0,0,0,45,0,0,0,0,52,45,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,16,15,15,15,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,63,63,63,63,63,63,63,63,63,63,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,28,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,51,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,16,15,15,15,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,47,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,34,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,16,15,15,15,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,36,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,32,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,9,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,10,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,24,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,26,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,41,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,-1,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,0,58,58,58,58,58,58,58,58,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,32,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,34,10,23,23,23,23,23,23,23,23,23,23,23,23,23,23,24,13,17,17,17,18,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,48,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,62,62,62,62,62,62,62,62,62,0,0,0,0,0,0,0,0,0,0,0,0,0,59,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,37,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,36,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,38,9,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,12,19,19,19,20,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,47,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,48,5,40,40,40,40,40,40,40,40,40,40,40,40,40,40,41,8,30,30,30,31,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,16,15,15,15,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,58,0,0,0,0,0,0,0,0,58,58,58,58,58,58,58,58,58,58,0,0,0,0,0,0,0,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,0,0,0,0,58,0,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,46,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,4396) [-1,9,10,11,12,13,61,39,45,45,45,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,32,58,34,-1,36,-1,-1,39,40,41,42,43,44,45,46,-1,48,49,50,51,52,53,54,55,56,57,58,59,-1,61,-1,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,10,93,39,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,34,45,-1,-1,-1,39,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,92,48,49,50,51,52,53,54,55,56,57,9,10,11,12,13,-1,-1,110,-1,-1,-1,-1,-1,116,-1,-1,-1,-1,-1,-1,-1,-1,-1,32,-1,46,92,48,49,50,51,52,53,54,55,56,57,195,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,-1,125,116,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,45,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,125,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,45,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,10,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,39,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,-1,184,185,186,187,188,189,190,191,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,34,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,45,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,195,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,63) [-1,6,6,-1,-1,-1,6,-1,-1,-1,-1,-1,-1,-1,21,21,-1,23,23,25,25,29,29,35,35,39,39,6,6,6,40,40,4,4,4,4,45,-1,45,45,49,49,-1,-1,-1,45,-1,50,50,50,50,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,63) [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_8))],[(AlexAcc (alex_action_8))]]
alex_action_3 =  tok (\p s -> PT p (eitherResIdent (TV . share) s)) 
alex_action_4 =  tok (\p s -> PT p (eitherResIdent (TV . share) s)) 
alex_action_5 =  tok (\p s -> PT p (TL $ share $ unescapeInitTail s)) 
alex_action_6 =  tok (\p s -> PT p (TC $ share s))  
alex_action_7 =  tok (\p s -> PT p (TI $ share s))    
alex_action_8 =  tok (\p s -> PT p (TD $ share s)) 


tok f p s = f p s

share :: String -> String
share = id

data Tok =
   TS !String !Int    -- reserved words and symbols
 | TL !String         -- string literals
 | TI !String         -- integer literals
 | TV !String         -- identifiers
 | TD !String         -- double precision float literals
 | TC !String         -- character literals

 deriving (Eq,Show,Ord)

data Token = 
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

posLineCol (Pn _ l c) = (l,c)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken t = case t of
  PT _ (TS s _) -> s
  PT _ (TI s) -> s
  PT _ (TV s) -> s
  PT _ (TD s) -> s
  PT _ (TC s) -> s

  _ -> show t

data BTree = N | B String Tok BTree BTree deriving (Show)

eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords = b "Space" 34 (b "Blink" 17 (b ":" 9 (b "+" 5 (b ")" 3 (b "(" 2 (b "$" 1 N N) N) (b "*" 4 N N)) (b "-" 7 (b "," 6 N N) (b "." 8 N N))) (b "?" 13 (b ";" 11 (b "::=" 10 N N) (b "=" 12 N N)) (b "Bg" 15 (b "@" 14 N N) (b "Black" 16 N N)))) (b "Green" 26 (b "DefaultFg" 22 (b "Cyan" 20 (b "Bold" 19 (b "Blue" 18 N N) N) (b "DefaultBg" 21 N N)) (b "Empty" 24 (b "DefaultStyle" 23 N N) (b "Fg" 25 N N))) (b "Magenta" 30 (b "Italic" 28 (b "Invert" 27 N N) (b "Linebreak" 29 N N)) (b "RGB" 32 (b "Nest" 31 N N) (b "Red" 33 N N))))) (b "internal" 51 (b "coercions" 43 (b "]" 39 (b "Yellow" 37 (b "White" 36 (b "Underline" 35 N N) N) (b "[" 38 N N)) (b "antiquote" 41 (b "_" 40 N N) (b "char" 42 N N))) (b "digit" 47 (b "define" 45 (b "comment" 44 N N) (b "derive" 46 N N)) (b "eps" 49 (b "entrypoints" 48 N N) (b "external" 50 N N)))) (b "stop" 59 (b "nonempty" 55 (b "letter" 53 (b "layout" 52 N N) (b "lower" 54 N N)) (b "rules" 57 (b "position" 56 N N) (b "separator" 58 N N))) (b "upper" 63 (b "token" 61 (b "terminator" 60 N N) (b "toplevel" 62 N N)) (b "|" 65 (b "{" 64 N N) (b "}" 66 N N)))))
   where b s n = let bs = s
                  in B bs (TS bs n)

unescapeInitTail :: String -> String
unescapeInitTail = unesc . tail where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------


alexStartPos :: Posn
alexStartPos = Pn 0 1 1

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : (go inp')


alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, _, s) = c

alexIndexInt16OffAddr arr off = arr ! off
alexIndexInt32OffAddr arr off = arr ! off
quickIndex arr i = arr ! i
-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetByte input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input'' len, _) ->



		AlexSkip input'' len

	(AlexLastAcc k input''' len, _) ->



		AlexToken input''' len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		(base) = alexIndexInt32OffAddr alex_base s
		((ord_c)) = fromIntegral c
		(offset) = (base + ord_c)
		(check)  = alexIndexInt16OffAddr alex_check offset
		
		(new_s) = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a predx : rest)
	   | predx user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred predx : rest)
	   | predx user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

instance Functor AlexLastAcc where
    fmap f AlexNone = AlexNone
    fmap f (AlexLastAcc x y z) = AlexLastAcc (f x) y z
    fmap f (AlexLastSkip x y) = AlexLastSkip x y

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i

{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- parser produced by Happy 

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn69 (String)
	| HappyAbsSyn70 (BNFC_QQType)
	| HappyAbsSyn71 (Integer)
	| HappyAbsSyn73 (Ident)
	| HappyAbsSyn75 (Char)
	| HappyAbsSyn77 (Double)
	| HappyAbsSyn79 (Grammar)
	| HappyAbsSyn81 ([Def])
	| HappyAbsSyn83 ([Item])
	| HappyAbsSyn85 (Def)
	| HappyAbsSyn87 (RHS)
	| HappyAbsSyn89 ([RHS])
	| HappyAbsSyn91 (Item)
	| HappyAbsSyn93 (MAnnotations)
	| HappyAbsSyn95 ([Annotation])
	| HappyAbsSyn97 (Annotation)
	| HappyAbsSyn99 (Color)
	| HappyAbsSyn101 (Style)
	| HappyAbsSyn103 (Cat)
	| HappyAbsSyn107 (Label)
	| HappyAbsSyn109 (MIdent)
	| HappyAbsSyn111 (HsTyp)
	| HappyAbsSyn115 ([HsTyp])
	| HappyAbsSyn117 (Arg)
	| HappyAbsSyn119 ([Arg])
	| HappyAbsSyn121 (Exp)
	| HappyAbsSyn127 ([Exp])
	| HappyAbsSyn131 ([String])
	| HappyAbsSyn133 (MinimumSize)
	| HappyAbsSyn135 (Reg)
	| HappyAbsSyn143 ([Ident])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340,
 action_341,
 action_342,
 action_343,
 action_344,
 action_345,
 action_346,
 action_347,
 action_348,
 action_349,
 action_350,
 action_351,
 action_352,
 action_353,
 action_354,
 action_355,
 action_356,
 action_357,
 action_358,
 action_359,
 action_360,
 action_361,
 action_362,
 action_363,
 action_364,
 action_365,
 action_366,
 action_367,
 action_368,
 action_369,
 action_370,
 action_371,
 action_372,
 action_373,
 action_374,
 action_375,
 action_376,
 action_377,
 action_378,
 action_379,
 action_380,
 action_381,
 action_382,
 action_383,
 action_384,
 action_385,
 action_386,
 action_387,
 action_388,
 action_389,
 action_390,
 action_391,
 action_392,
 action_393,
 action_394,
 action_395,
 action_396,
 action_397,
 action_398,
 action_399,
 action_400,
 action_401,
 action_402,
 action_403,
 action_404,
 action_405,
 action_406,
 action_407,
 action_408,
 action_409,
 action_410,
 action_411,
 action_412,
 action_413,
 action_414,
 action_415,
 action_416,
 action_417,
 action_418,
 action_419,
 action_420,
 action_421,
 action_422,
 action_423,
 action_424,
 action_425,
 action_426,
 action_427,
 action_428,
 action_429,
 action_430,
 action_431,
 action_432,
 action_433,
 action_434,
 action_435,
 action_436,
 action_437,
 action_438,
 action_439,
 action_440,
 action_441,
 action_442,
 action_443,
 action_444,
 action_445,
 action_446,
 action_447,
 action_448,
 action_449,
 action_450,
 action_451,
 action_452,
 action_453,
 action_454,
 action_455,
 action_456,
 action_457,
 action_458,
 action_459,
 action_460,
 action_461,
 action_462,
 action_463,
 action_464,
 action_465,
 action_466,
 action_467,
 action_468,
 action_469,
 action_470,
 action_471,
 action_472,
 action_473,
 action_474,
 action_475,
 action_476,
 action_477,
 action_478,
 action_479,
 action_480,
 action_481,
 action_482,
 action_483,
 action_484,
 action_485,
 action_486,
 action_487,
 action_488,
 action_489,
 action_490,
 action_491,
 action_492,
 action_493,
 action_494,
 action_495,
 action_496,
 action_497,
 action_498,
 action_499,
 action_500,
 action_501,
 action_502,
 action_503,
 action_504,
 action_505,
 action_506,
 action_507,
 action_508,
 action_509,
 action_510,
 action_511,
 action_512,
 action_513,
 action_514,
 action_515,
 action_516,
 action_517,
 action_518,
 action_519,
 action_520,
 action_521,
 action_522,
 action_523,
 action_524,
 action_525,
 action_526,
 action_527,
 action_528,
 action_529,
 action_530,
 action_531,
 action_532,
 action_533,
 action_534,
 action_535,
 action_536,
 action_537,
 action_538,
 action_539,
 action_540,
 action_541,
 action_542,
 action_543,
 action_544,
 action_545,
 action_546 :: () => Int -> ({-HappyReduction (ParseMonad) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (ParseMonad) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (ParseMonad) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (ParseMonad) HappyAbsSyn)

happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176,
 happyReduce_177,
 happyReduce_178,
 happyReduce_179,
 happyReduce_180,
 happyReduce_181,
 happyReduce_182,
 happyReduce_183,
 happyReduce_184,
 happyReduce_185,
 happyReduce_186,
 happyReduce_187,
 happyReduce_188,
 happyReduce_189,
 happyReduce_190,
 happyReduce_191,
 happyReduce_192,
 happyReduce_193,
 happyReduce_194,
 happyReduce_195,
 happyReduce_196,
 happyReduce_197,
 happyReduce_198,
 happyReduce_199,
 happyReduce_200,
 happyReduce_201,
 happyReduce_202,
 happyReduce_203,
 happyReduce_204,
 happyReduce_205,
 happyReduce_206,
 happyReduce_207,
 happyReduce_208,
 happyReduce_209,
 happyReduce_210,
 happyReduce_211,
 happyReduce_212,
 happyReduce_213,
 happyReduce_214,
 happyReduce_215,
 happyReduce_216,
 happyReduce_217,
 happyReduce_218,
 happyReduce_219,
 happyReduce_220,
 happyReduce_221,
 happyReduce_222,
 happyReduce_223,
 happyReduce_224,
 happyReduce_225,
 happyReduce_226,
 happyReduce_227,
 happyReduce_228,
 happyReduce_229,
 happyReduce_230,
 happyReduce_231,
 happyReduce_232,
 happyReduce_233,
 happyReduce_234,
 happyReduce_235,
 happyReduce_236,
 happyReduce_237,
 happyReduce_238,
 happyReduce_239,
 happyReduce_240,
 happyReduce_241,
 happyReduce_242,
 happyReduce_243,
 happyReduce_244,
 happyReduce_245,
 happyReduce_246,
 happyReduce_247,
 happyReduce_248,
 happyReduce_249,
 happyReduce_250,
 happyReduce_251,
 happyReduce_252,
 happyReduce_253,
 happyReduce_254,
 happyReduce_255,
 happyReduce_256,
 happyReduce_257,
 happyReduce_258,
 happyReduce_259,
 happyReduce_260,
 happyReduce_261,
 happyReduce_262,
 happyReduce_263,
 happyReduce_264,
 happyReduce_265,
 happyReduce_266,
 happyReduce_267,
 happyReduce_268,
 happyReduce_269,
 happyReduce_270,
 happyReduce_271,
 happyReduce_272,
 happyReduce_273,
 happyReduce_274,
 happyReduce_275,
 happyReduce_276,
 happyReduce_277,
 happyReduce_278,
 happyReduce_279,
 happyReduce_280,
 happyReduce_281,
 happyReduce_282,
 happyReduce_283,
 happyReduce_284,
 happyReduce_285,
 happyReduce_286,
 happyReduce_287,
 happyReduce_288,
 happyReduce_289,
 happyReduce_290,
 happyReduce_291,
 happyReduce_292,
 happyReduce_293,
 happyReduce_294,
 happyReduce_295,
 happyReduce_296,
 happyReduce_297,
 happyReduce_298,
 happyReduce_299,
 happyReduce_300,
 happyReduce_301,
 happyReduce_302,
 happyReduce_303,
 happyReduce_304,
 happyReduce_305,
 happyReduce_306,
 happyReduce_307,
 happyReduce_308,
 happyReduce_309,
 happyReduce_310,
 happyReduce_311,
 happyReduce_312,
 happyReduce_313,
 happyReduce_314,
 happyReduce_315 :: () => ({-HappyReduction (ParseMonad) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (ParseMonad) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (ParseMonad) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (ParseMonad) HappyAbsSyn)

action_0 (145) = happyShift action_189
action_0 (146) = happyShift action_190
action_0 (165) = happyShift action_298
action_0 (166) = happyShift action_299
action_0 (167) = happyShift action_300
action_0 (182) = happyShift action_191
action_0 (184) = happyShift action_192
action_0 (185) = happyShift action_301
action_0 (187) = happyShift action_302
action_0 (188) = happyShift action_303
action_0 (189) = happyShift action_304
action_0 (190) = happyShift action_305
action_0 (192) = happyShift action_306
action_0 (194) = happyShift action_307
action_0 (195) = happyShift action_308
action_0 (196) = happyShift action_309
action_0 (200) = happyShift action_310
action_0 (201) = happyShift action_311
action_0 (202) = happyShift action_312
action_0 (204) = happyShift action_313
action_0 (205) = happyShift action_314
action_0 (213) = happyShift action_73
action_0 (73) = happyGoto action_187
action_0 (79) = happyGoto action_323
action_0 (81) = happyGoto action_324
action_0 (85) = happyGoto action_320
action_0 (107) = happyGoto action_297
action_0 _ = happyReduce_78

action_1 (145) = happyShift action_183
action_1 (146) = happyShift action_184
action_1 (165) = happyShift action_279
action_1 (166) = happyShift action_280
action_1 (167) = happyShift action_281
action_1 (182) = happyShift action_185
action_1 (184) = happyShift action_186
action_1 (185) = happyShift action_282
action_1 (187) = happyShift action_283
action_1 (188) = happyShift action_284
action_1 (189) = happyShift action_285
action_1 (190) = happyShift action_286
action_1 (192) = happyShift action_287
action_1 (194) = happyShift action_288
action_1 (195) = happyShift action_289
action_1 (196) = happyShift action_290
action_1 (200) = happyShift action_291
action_1 (201) = happyShift action_292
action_1 (202) = happyShift action_293
action_1 (204) = happyShift action_294
action_1 (205) = happyShift action_295
action_1 (213) = happyShift action_70
action_1 (74) = happyGoto action_181
action_1 (80) = happyGoto action_321
action_1 (82) = happyGoto action_322
action_1 (86) = happyGoto action_318
action_1 (108) = happyGoto action_278
action_1 _ = happyReduce_81

action_2 (145) = happyShift action_189
action_2 (146) = happyShift action_190
action_2 (165) = happyShift action_298
action_2 (166) = happyShift action_299
action_2 (167) = happyShift action_300
action_2 (182) = happyShift action_191
action_2 (184) = happyShift action_192
action_2 (185) = happyShift action_301
action_2 (187) = happyShift action_302
action_2 (188) = happyShift action_303
action_2 (189) = happyShift action_304
action_2 (190) = happyShift action_305
action_2 (192) = happyShift action_306
action_2 (194) = happyShift action_307
action_2 (195) = happyShift action_308
action_2 (196) = happyShift action_309
action_2 (200) = happyShift action_310
action_2 (201) = happyShift action_311
action_2 (202) = happyShift action_312
action_2 (204) = happyShift action_313
action_2 (205) = happyShift action_314
action_2 (213) = happyShift action_73
action_2 (73) = happyGoto action_187
action_2 (81) = happyGoto action_319
action_2 (85) = happyGoto action_320
action_2 (107) = happyGoto action_297
action_2 _ = happyReduce_78

action_3 (145) = happyShift action_183
action_3 (146) = happyShift action_184
action_3 (165) = happyShift action_279
action_3 (166) = happyShift action_280
action_3 (167) = happyShift action_281
action_3 (182) = happyShift action_185
action_3 (184) = happyShift action_186
action_3 (185) = happyShift action_282
action_3 (187) = happyShift action_283
action_3 (188) = happyShift action_284
action_3 (189) = happyShift action_285
action_3 (190) = happyShift action_286
action_3 (192) = happyShift action_287
action_3 (194) = happyShift action_288
action_3 (195) = happyShift action_289
action_3 (196) = happyShift action_290
action_3 (200) = happyShift action_291
action_3 (201) = happyShift action_292
action_3 (202) = happyShift action_293
action_3 (204) = happyShift action_294
action_3 (205) = happyShift action_295
action_3 (213) = happyShift action_70
action_3 (74) = happyGoto action_181
action_3 (82) = happyGoto action_317
action_3 (86) = happyGoto action_318
action_3 (108) = happyGoto action_278
action_3 _ = happyReduce_81

action_4 (83) = happyGoto action_316
action_4 _ = happyReduce_84

action_5 (84) = happyGoto action_315
action_5 _ = happyReduce_86

action_6 (145) = happyShift action_189
action_6 (146) = happyShift action_190
action_6 (165) = happyShift action_298
action_6 (166) = happyShift action_299
action_6 (167) = happyShift action_300
action_6 (182) = happyShift action_191
action_6 (184) = happyShift action_192
action_6 (185) = happyShift action_301
action_6 (187) = happyShift action_302
action_6 (188) = happyShift action_303
action_6 (189) = happyShift action_304
action_6 (190) = happyShift action_305
action_6 (192) = happyShift action_306
action_6 (194) = happyShift action_307
action_6 (195) = happyShift action_308
action_6 (196) = happyShift action_309
action_6 (200) = happyShift action_310
action_6 (201) = happyShift action_311
action_6 (202) = happyShift action_312
action_6 (204) = happyShift action_313
action_6 (205) = happyShift action_314
action_6 (213) = happyShift action_73
action_6 (73) = happyGoto action_187
action_6 (85) = happyGoto action_296
action_6 (107) = happyGoto action_297
action_6 _ = happyFail

action_7 (145) = happyShift action_183
action_7 (146) = happyShift action_184
action_7 (165) = happyShift action_279
action_7 (166) = happyShift action_280
action_7 (167) = happyShift action_281
action_7 (182) = happyShift action_185
action_7 (184) = happyShift action_186
action_7 (185) = happyShift action_282
action_7 (187) = happyShift action_283
action_7 (188) = happyShift action_284
action_7 (189) = happyShift action_285
action_7 (190) = happyShift action_286
action_7 (192) = happyShift action_287
action_7 (194) = happyShift action_288
action_7 (195) = happyShift action_289
action_7 (196) = happyShift action_290
action_7 (200) = happyShift action_291
action_7 (201) = happyShift action_292
action_7 (202) = happyShift action_293
action_7 (204) = happyShift action_294
action_7 (205) = happyShift action_295
action_7 (213) = happyShift action_70
action_7 (74) = happyGoto action_181
action_7 (86) = happyGoto action_277
action_7 (108) = happyGoto action_278
action_7 _ = happyFail

action_8 (158) = happyShift action_274
action_8 (83) = happyGoto action_271
action_8 (87) = happyGoto action_276
action_8 _ = happyReduce_84

action_9 (158) = happyShift action_270
action_9 (84) = happyGoto action_267
action_9 (88) = happyGoto action_275
action_9 _ = happyReduce_86

action_10 (158) = happyShift action_274
action_10 (83) = happyGoto action_271
action_10 (87) = happyGoto action_272
action_10 (89) = happyGoto action_273
action_10 _ = happyReduce_84

action_11 (158) = happyShift action_270
action_11 (84) = happyGoto action_267
action_11 (88) = happyGoto action_268
action_11 (90) = happyGoto action_269
action_11 _ = happyReduce_86

action_12 (157) = happyShift action_204
action_12 (182) = happyShift action_198
action_12 (211) = happyShift action_67
action_12 (213) = happyShift action_73
action_12 (69) = happyGoto action_264
action_12 (73) = happyGoto action_196
action_12 (91) = happyGoto action_265
action_12 (103) = happyGoto action_266
action_12 (105) = happyGoto action_203
action_12 _ = happyFail

action_13 (157) = happyShift action_201
action_13 (182) = happyShift action_195
action_13 (211) = happyShift action_116
action_13 (213) = happyShift action_70
action_13 (70) = happyGoto action_261
action_13 (74) = happyGoto action_193
action_13 (92) = happyGoto action_262
action_13 (104) = happyGoto action_263
action_13 (106) = happyGoto action_200
action_13 _ = happyFail

action_14 (146) = happyShift action_260
action_14 (93) = happyGoto action_259
action_14 _ = happyReduce_143

action_15 (146) = happyShift action_258
action_15 (94) = happyGoto action_257
action_15 _ = happyReduce_145

action_16 (159) = happyShift action_247
action_16 (161) = happyShift action_212
action_16 (163) = happyShift action_213
action_16 (168) = happyShift action_248
action_16 (169) = happyShift action_249
action_16 (171) = happyShift action_214
action_16 (172) = happyShift action_215
action_16 (173) = happyShift action_250
action_16 (175) = happyShift action_251
action_16 (178) = happyShift action_252
action_16 (179) = happyShift action_216
action_16 (95) = happyGoto action_255
action_16 (97) = happyGoto action_256
action_16 (101) = happyGoto action_246
action_16 _ = happyFail

action_17 (159) = happyShift action_239
action_17 (161) = happyShift action_206
action_17 (163) = happyShift action_207
action_17 (168) = happyShift action_240
action_17 (169) = happyShift action_241
action_17 (171) = happyShift action_208
action_17 (172) = happyShift action_209
action_17 (173) = happyShift action_242
action_17 (175) = happyShift action_243
action_17 (178) = happyShift action_244
action_17 (179) = happyShift action_210
action_17 (96) = happyGoto action_253
action_17 (98) = happyGoto action_254
action_17 (102) = happyGoto action_238
action_17 _ = happyFail

action_18 (159) = happyShift action_247
action_18 (161) = happyShift action_212
action_18 (163) = happyShift action_213
action_18 (168) = happyShift action_248
action_18 (169) = happyShift action_249
action_18 (171) = happyShift action_214
action_18 (172) = happyShift action_215
action_18 (173) = happyShift action_250
action_18 (175) = happyShift action_251
action_18 (178) = happyShift action_252
action_18 (179) = happyShift action_216
action_18 (97) = happyGoto action_245
action_18 (101) = happyGoto action_246
action_18 _ = happyFail

action_19 (159) = happyShift action_239
action_19 (161) = happyShift action_206
action_19 (163) = happyShift action_207
action_19 (168) = happyShift action_240
action_19 (169) = happyShift action_241
action_19 (171) = happyShift action_208
action_19 (172) = happyShift action_209
action_19 (173) = happyShift action_242
action_19 (175) = happyShift action_243
action_19 (178) = happyShift action_244
action_19 (179) = happyShift action_210
action_19 (98) = happyGoto action_237
action_19 (102) = happyGoto action_238
action_19 _ = happyFail

action_20 (160) = happyShift action_228
action_20 (162) = happyShift action_229
action_20 (164) = happyShift action_230
action_20 (170) = happyShift action_231
action_20 (174) = happyShift action_232
action_20 (176) = happyShift action_233
action_20 (177) = happyShift action_234
action_20 (180) = happyShift action_235
action_20 (181) = happyShift action_236
action_20 (99) = happyGoto action_227
action_20 _ = happyFail

action_21 (160) = happyShift action_218
action_21 (162) = happyShift action_219
action_21 (164) = happyShift action_220
action_21 (170) = happyShift action_221
action_21 (174) = happyShift action_222
action_21 (176) = happyShift action_223
action_21 (177) = happyShift action_224
action_21 (180) = happyShift action_225
action_21 (181) = happyShift action_226
action_21 (100) = happyGoto action_217
action_21 _ = happyFail

action_22 (161) = happyShift action_212
action_22 (163) = happyShift action_213
action_22 (171) = happyShift action_214
action_22 (172) = happyShift action_215
action_22 (179) = happyShift action_216
action_22 (101) = happyGoto action_211
action_22 _ = happyFail

action_23 (161) = happyShift action_206
action_23 (163) = happyShift action_207
action_23 (171) = happyShift action_208
action_23 (172) = happyShift action_209
action_23 (179) = happyShift action_210
action_23 (102) = happyGoto action_205
action_23 _ = happyFail

action_24 (157) = happyShift action_204
action_24 (182) = happyShift action_198
action_24 (213) = happyShift action_73
action_24 (73) = happyGoto action_196
action_24 (103) = happyGoto action_202
action_24 (105) = happyGoto action_203
action_24 _ = happyFail

action_25 (157) = happyShift action_201
action_25 (182) = happyShift action_195
action_25 (213) = happyShift action_70
action_25 (74) = happyGoto action_193
action_25 (104) = happyGoto action_199
action_25 (106) = happyGoto action_200
action_25 _ = happyFail

action_26 (182) = happyShift action_198
action_26 (213) = happyShift action_73
action_26 (73) = happyGoto action_196
action_26 (105) = happyGoto action_197
action_26 _ = happyFail

action_27 (182) = happyShift action_195
action_27 (213) = happyShift action_70
action_27 (74) = happyGoto action_193
action_27 (106) = happyGoto action_194
action_27 _ = happyFail

action_28 (145) = happyShift action_189
action_28 (146) = happyShift action_190
action_28 (182) = happyShift action_191
action_28 (184) = happyShift action_192
action_28 (213) = happyShift action_73
action_28 (73) = happyGoto action_187
action_28 (107) = happyGoto action_188
action_28 _ = happyFail

action_29 (145) = happyShift action_183
action_29 (146) = happyShift action_184
action_29 (182) = happyShift action_185
action_29 (184) = happyShift action_186
action_29 (213) = happyShift action_70
action_29 (74) = happyGoto action_181
action_29 (108) = happyGoto action_182
action_29 _ = happyFail

action_30 (213) = happyShift action_73
action_30 (73) = happyGoto action_179
action_30 (109) = happyGoto action_180
action_30 _ = happyReduce_213

action_31 (213) = happyShift action_70
action_31 (74) = happyGoto action_177
action_31 (110) = happyGoto action_178
action_31 _ = happyReduce_215

action_32 (111) = happyGoto action_176
action_32 _ = happyFail

action_33 (112) = happyGoto action_175
action_33 _ = happyFail

action_34 (146) = happyShift action_173
action_34 (182) = happyShift action_174
action_34 (213) = happyShift action_73
action_34 (73) = happyGoto action_171
action_34 (113) = happyGoto action_172
action_34 _ = happyFail

action_35 (146) = happyShift action_169
action_35 (182) = happyShift action_170
action_35 (213) = happyShift action_70
action_35 (74) = happyGoto action_167
action_35 (114) = happyGoto action_168
action_35 _ = happyFail

action_36 (111) = happyGoto action_165
action_36 (115) = happyGoto action_166
action_36 _ = happyFail

action_37 (112) = happyGoto action_163
action_37 (116) = happyGoto action_164
action_37 _ = happyFail

action_38 (213) = happyShift action_73
action_38 (73) = happyGoto action_161
action_38 (117) = happyGoto action_162
action_38 _ = happyFail

action_39 (213) = happyShift action_70
action_39 (74) = happyGoto action_159
action_39 (118) = happyGoto action_160
action_39 _ = happyFail

action_40 (119) = happyGoto action_158
action_40 _ = happyReduce_230

action_41 (120) = happyGoto action_157
action_41 _ = happyReduce_232

action_42 (146) = happyShift action_141
action_42 (182) = happyShift action_142
action_42 (211) = happyShift action_67
action_42 (212) = happyShift action_143
action_42 (213) = happyShift action_73
action_42 (214) = happyShift action_103
action_42 (215) = happyShift action_144
action_42 (69) = happyGoto action_132
action_42 (71) = happyGoto action_133
action_42 (73) = happyGoto action_134
action_42 (75) = happyGoto action_135
action_42 (77) = happyGoto action_136
action_42 (121) = happyGoto action_156
action_42 (123) = happyGoto action_138
action_42 (125) = happyGoto action_139
action_42 _ = happyFail

action_43 (146) = happyShift action_128
action_43 (182) = happyShift action_129
action_43 (211) = happyShift action_116
action_43 (212) = happyShift action_130
action_43 (213) = happyShift action_70
action_43 (214) = happyShift action_88
action_43 (215) = happyShift action_131
action_43 (70) = happyGoto action_119
action_43 (72) = happyGoto action_120
action_43 (74) = happyGoto action_121
action_43 (76) = happyGoto action_122
action_43 (78) = happyGoto action_123
action_43 (122) = happyGoto action_155
action_43 (124) = happyGoto action_125
action_43 (126) = happyGoto action_126
action_43 _ = happyFail

action_44 (146) = happyShift action_141
action_44 (182) = happyShift action_142
action_44 (211) = happyShift action_67
action_44 (212) = happyShift action_143
action_44 (213) = happyShift action_73
action_44 (214) = happyShift action_103
action_44 (215) = happyShift action_144
action_44 (69) = happyGoto action_132
action_44 (71) = happyGoto action_133
action_44 (73) = happyGoto action_134
action_44 (75) = happyGoto action_135
action_44 (77) = happyGoto action_136
action_44 (123) = happyGoto action_154
action_44 (125) = happyGoto action_139
action_44 _ = happyFail

action_45 (146) = happyShift action_128
action_45 (182) = happyShift action_129
action_45 (211) = happyShift action_116
action_45 (212) = happyShift action_130
action_45 (213) = happyShift action_70
action_45 (214) = happyShift action_88
action_45 (215) = happyShift action_131
action_45 (70) = happyGoto action_119
action_45 (72) = happyGoto action_120
action_45 (74) = happyGoto action_121
action_45 (76) = happyGoto action_122
action_45 (78) = happyGoto action_123
action_45 (124) = happyGoto action_153
action_45 (126) = happyGoto action_126
action_45 _ = happyFail

action_46 (146) = happyShift action_141
action_46 (182) = happyShift action_142
action_46 (211) = happyShift action_67
action_46 (212) = happyShift action_143
action_46 (213) = happyShift action_73
action_46 (214) = happyShift action_103
action_46 (215) = happyShift action_144
action_46 (69) = happyGoto action_132
action_46 (71) = happyGoto action_133
action_46 (73) = happyGoto action_148
action_46 (75) = happyGoto action_135
action_46 (77) = happyGoto action_136
action_46 (125) = happyGoto action_152
action_46 _ = happyFail

action_47 (146) = happyShift action_128
action_47 (182) = happyShift action_129
action_47 (211) = happyShift action_116
action_47 (212) = happyShift action_130
action_47 (213) = happyShift action_70
action_47 (214) = happyShift action_88
action_47 (215) = happyShift action_131
action_47 (70) = happyGoto action_119
action_47 (72) = happyGoto action_120
action_47 (74) = happyGoto action_145
action_47 (76) = happyGoto action_122
action_47 (78) = happyGoto action_123
action_47 (126) = happyGoto action_151
action_47 _ = happyFail

action_48 (146) = happyShift action_141
action_48 (182) = happyShift action_142
action_48 (211) = happyShift action_67
action_48 (212) = happyShift action_143
action_48 (213) = happyShift action_73
action_48 (214) = happyShift action_103
action_48 (215) = happyShift action_144
action_48 (69) = happyGoto action_132
action_48 (71) = happyGoto action_133
action_48 (73) = happyGoto action_148
action_48 (75) = happyGoto action_135
action_48 (77) = happyGoto action_136
action_48 (125) = happyGoto action_149
action_48 (127) = happyGoto action_150
action_48 _ = happyFail

action_49 (146) = happyShift action_128
action_49 (182) = happyShift action_129
action_49 (211) = happyShift action_116
action_49 (212) = happyShift action_130
action_49 (213) = happyShift action_70
action_49 (214) = happyShift action_88
action_49 (215) = happyShift action_131
action_49 (70) = happyGoto action_119
action_49 (72) = happyGoto action_120
action_49 (74) = happyGoto action_145
action_49 (76) = happyGoto action_122
action_49 (78) = happyGoto action_123
action_49 (126) = happyGoto action_146
action_49 (128) = happyGoto action_147
action_49 _ = happyFail

action_50 (146) = happyShift action_141
action_50 (182) = happyShift action_142
action_50 (211) = happyShift action_67
action_50 (212) = happyShift action_143
action_50 (213) = happyShift action_73
action_50 (214) = happyShift action_103
action_50 (215) = happyShift action_144
action_50 (69) = happyGoto action_132
action_50 (71) = happyGoto action_133
action_50 (73) = happyGoto action_134
action_50 (75) = happyGoto action_135
action_50 (77) = happyGoto action_136
action_50 (121) = happyGoto action_137
action_50 (123) = happyGoto action_138
action_50 (125) = happyGoto action_139
action_50 (129) = happyGoto action_140
action_50 _ = happyReduce_260

action_51 (146) = happyShift action_128
action_51 (182) = happyShift action_129
action_51 (211) = happyShift action_116
action_51 (212) = happyShift action_130
action_51 (213) = happyShift action_70
action_51 (214) = happyShift action_88
action_51 (215) = happyShift action_131
action_51 (70) = happyGoto action_119
action_51 (72) = happyGoto action_120
action_51 (74) = happyGoto action_121
action_51 (76) = happyGoto action_122
action_51 (78) = happyGoto action_123
action_51 (122) = happyGoto action_124
action_51 (124) = happyGoto action_125
action_51 (126) = happyGoto action_126
action_51 (130) = happyGoto action_127
action_51 _ = happyReduce_263

action_52 (211) = happyShift action_67
action_52 (69) = happyGoto action_117
action_52 (131) = happyGoto action_118
action_52 _ = happyFail

action_53 (211) = happyShift action_116
action_53 (70) = happyGoto action_114
action_53 (132) = happyGoto action_115
action_53 _ = happyFail

action_54 (199) = happyShift action_113
action_54 (133) = happyGoto action_112
action_54 _ = happyReduce_271

action_55 (199) = happyShift action_111
action_55 (134) = happyGoto action_110
action_55 _ = happyReduce_273

action_56 (146) = happyShift action_94
action_56 (182) = happyShift action_95
action_56 (186) = happyShift action_96
action_56 (191) = happyShift action_97
action_56 (193) = happyShift action_98
action_56 (197) = happyShift action_99
action_56 (198) = happyShift action_100
action_56 (207) = happyShift action_101
action_56 (208) = happyShift action_102
action_56 (214) = happyShift action_103
action_56 (75) = happyGoto action_89
action_56 (135) = happyGoto action_109
action_56 (139) = happyGoto action_92
action_56 _ = happyFail

action_57 (146) = happyShift action_79
action_57 (182) = happyShift action_80
action_57 (186) = happyShift action_81
action_57 (191) = happyShift action_82
action_57 (193) = happyShift action_83
action_57 (197) = happyShift action_84
action_57 (198) = happyShift action_85
action_57 (207) = happyShift action_86
action_57 (208) = happyShift action_87
action_57 (214) = happyShift action_88
action_57 (76) = happyGoto action_74
action_57 (136) = happyGoto action_108
action_57 (140) = happyGoto action_77
action_57 _ = happyFail

action_58 (146) = happyShift action_94
action_58 (182) = happyShift action_95
action_58 (186) = happyShift action_96
action_58 (191) = happyShift action_97
action_58 (193) = happyShift action_98
action_58 (197) = happyShift action_99
action_58 (198) = happyShift action_100
action_58 (207) = happyShift action_101
action_58 (208) = happyShift action_102
action_58 (214) = happyShift action_103
action_58 (75) = happyGoto action_89
action_58 (135) = happyGoto action_90
action_58 (137) = happyGoto action_107
action_58 (139) = happyGoto action_92
action_58 _ = happyFail

action_59 (146) = happyShift action_79
action_59 (182) = happyShift action_80
action_59 (186) = happyShift action_81
action_59 (191) = happyShift action_82
action_59 (193) = happyShift action_83
action_59 (197) = happyShift action_84
action_59 (198) = happyShift action_85
action_59 (207) = happyShift action_86
action_59 (208) = happyShift action_87
action_59 (214) = happyShift action_88
action_59 (76) = happyGoto action_74
action_59 (136) = happyGoto action_75
action_59 (138) = happyGoto action_106
action_59 (140) = happyGoto action_77
action_59 _ = happyFail

action_60 (146) = happyShift action_94
action_60 (182) = happyShift action_95
action_60 (186) = happyShift action_96
action_60 (191) = happyShift action_97
action_60 (193) = happyShift action_98
action_60 (197) = happyShift action_99
action_60 (198) = happyShift action_100
action_60 (207) = happyShift action_101
action_60 (208) = happyShift action_102
action_60 (214) = happyShift action_103
action_60 (75) = happyGoto action_89
action_60 (139) = happyGoto action_105
action_60 _ = happyFail

action_61 (146) = happyShift action_79
action_61 (182) = happyShift action_80
action_61 (186) = happyShift action_81
action_61 (191) = happyShift action_82
action_61 (193) = happyShift action_83
action_61 (197) = happyShift action_84
action_61 (198) = happyShift action_85
action_61 (207) = happyShift action_86
action_61 (208) = happyShift action_87
action_61 (214) = happyShift action_88
action_61 (76) = happyGoto action_74
action_61 (140) = happyGoto action_104
action_61 _ = happyFail

action_62 (146) = happyShift action_94
action_62 (182) = happyShift action_95
action_62 (186) = happyShift action_96
action_62 (191) = happyShift action_97
action_62 (193) = happyShift action_98
action_62 (197) = happyShift action_99
action_62 (198) = happyShift action_100
action_62 (207) = happyShift action_101
action_62 (208) = happyShift action_102
action_62 (214) = happyShift action_103
action_62 (75) = happyGoto action_89
action_62 (135) = happyGoto action_90
action_62 (137) = happyGoto action_91
action_62 (139) = happyGoto action_92
action_62 (141) = happyGoto action_93
action_62 _ = happyFail

action_63 (146) = happyShift action_79
action_63 (182) = happyShift action_80
action_63 (186) = happyShift action_81
action_63 (191) = happyShift action_82
action_63 (193) = happyShift action_83
action_63 (197) = happyShift action_84
action_63 (198) = happyShift action_85
action_63 (207) = happyShift action_86
action_63 (208) = happyShift action_87
action_63 (214) = happyShift action_88
action_63 (76) = happyGoto action_74
action_63 (136) = happyGoto action_75
action_63 (138) = happyGoto action_76
action_63 (140) = happyGoto action_77
action_63 (142) = happyGoto action_78
action_63 _ = happyFail

action_64 (213) = happyShift action_73
action_64 (73) = happyGoto action_71
action_64 (143) = happyGoto action_72
action_64 _ = happyFail

action_65 (213) = happyShift action_70
action_65 (74) = happyGoto action_68
action_65 (144) = happyGoto action_69
action_65 _ = happyFail

action_66 (211) = happyShift action_67
action_66 _ = happyFail

action_67 _ = happyReduce_66

action_68 (150) = happyShift action_444
action_68 _ = happyReduce_314

action_69 (217) = happyAccept
action_69 _ = happyFail

action_70 _ = happyReduce_71

action_71 (150) = happyShift action_443
action_71 _ = happyReduce_312

action_72 (217) = happyAccept
action_72 _ = happyFail

action_73 _ = happyReduce_70

action_74 _ = happyReduce_301

action_75 (146) = happyShift action_79
action_75 (151) = happyShift action_442
action_75 (182) = happyShift action_80
action_75 (186) = happyShift action_81
action_75 (191) = happyShift action_82
action_75 (193) = happyShift action_83
action_75 (197) = happyShift action_84
action_75 (198) = happyShift action_85
action_75 (207) = happyShift action_86
action_75 (208) = happyShift action_87
action_75 (214) = happyShift action_88
action_75 (76) = happyGoto action_74
action_75 (140) = happyGoto action_426
action_75 _ = happyReduce_283

action_76 (209) = happyShift action_428
action_76 _ = happyReduce_311

action_77 (148) = happyShift action_432
action_77 (149) = happyShift action_433
action_77 (157) = happyShift action_434
action_77 _ = happyReduce_277

action_78 (217) = happyAccept
action_78 _ = happyFail

action_79 (146) = happyShift action_79
action_79 (182) = happyShift action_80
action_79 (186) = happyShift action_81
action_79 (191) = happyShift action_82
action_79 (193) = happyShift action_83
action_79 (197) = happyShift action_84
action_79 (198) = happyShift action_85
action_79 (207) = happyShift action_86
action_79 (208) = happyShift action_87
action_79 (214) = happyShift action_88
action_79 (76) = happyGoto action_74
action_79 (136) = happyGoto action_75
action_79 (138) = happyGoto action_76
action_79 (140) = happyGoto action_77
action_79 (142) = happyGoto action_441
action_79 _ = happyFail

action_80 (211) = happyShift action_116
action_80 (70) = happyGoto action_440
action_80 _ = happyFail

action_81 _ = happyReduce_308

action_82 _ = happyReduce_304

action_83 _ = happyReduce_300

action_84 _ = happyReduce_305

action_85 _ = happyReduce_307

action_86 _ = happyReduce_306

action_87 (211) = happyShift action_116
action_87 (70) = happyGoto action_439
action_87 _ = happyFail

action_88 _ = happyReduce_73

action_89 _ = happyReduce_288

action_90 (146) = happyShift action_94
action_90 (151) = happyShift action_438
action_90 (182) = happyShift action_95
action_90 (186) = happyShift action_96
action_90 (191) = happyShift action_97
action_90 (193) = happyShift action_98
action_90 (197) = happyShift action_99
action_90 (198) = happyShift action_100
action_90 (207) = happyShift action_101
action_90 (208) = happyShift action_102
action_90 (214) = happyShift action_103
action_90 (75) = happyGoto action_89
action_90 (139) = happyGoto action_425
action_90 _ = happyReduce_280

action_91 (209) = happyShift action_427
action_91 _ = happyReduce_310

action_92 (148) = happyShift action_429
action_92 (149) = happyShift action_430
action_92 (157) = happyShift action_431
action_92 _ = happyReduce_275

action_93 (217) = happyAccept
action_93 _ = happyFail

action_94 (146) = happyShift action_94
action_94 (182) = happyShift action_95
action_94 (186) = happyShift action_96
action_94 (191) = happyShift action_97
action_94 (193) = happyShift action_98
action_94 (197) = happyShift action_99
action_94 (198) = happyShift action_100
action_94 (207) = happyShift action_101
action_94 (208) = happyShift action_102
action_94 (214) = happyShift action_103
action_94 (75) = happyGoto action_89
action_94 (135) = happyGoto action_90
action_94 (137) = happyGoto action_91
action_94 (139) = happyGoto action_92
action_94 (141) = happyGoto action_437
action_94 _ = happyFail

action_95 (211) = happyShift action_67
action_95 (69) = happyGoto action_436
action_95 _ = happyFail

action_96 _ = happyReduce_295

action_97 _ = happyReduce_291

action_98 _ = happyReduce_287

action_99 _ = happyReduce_292

action_100 _ = happyReduce_294

action_101 _ = happyReduce_293

action_102 (211) = happyShift action_67
action_102 (69) = happyGoto action_435
action_102 _ = happyFail

action_103 _ = happyReduce_72

action_104 (148) = happyShift action_432
action_104 (149) = happyShift action_433
action_104 (157) = happyShift action_434
action_104 (217) = happyAccept
action_104 _ = happyFail

action_105 (148) = happyShift action_429
action_105 (149) = happyShift action_430
action_105 (157) = happyShift action_431
action_105 (217) = happyAccept
action_105 _ = happyFail

action_106 (209) = happyShift action_428
action_106 (217) = happyAccept
action_106 _ = happyFail

action_107 (209) = happyShift action_427
action_107 (217) = happyAccept
action_107 _ = happyFail

action_108 (146) = happyShift action_79
action_108 (182) = happyShift action_80
action_108 (186) = happyShift action_81
action_108 (191) = happyShift action_82
action_108 (193) = happyShift action_83
action_108 (197) = happyShift action_84
action_108 (198) = happyShift action_85
action_108 (207) = happyShift action_86
action_108 (208) = happyShift action_87
action_108 (214) = happyShift action_88
action_108 (217) = happyAccept
action_108 (76) = happyGoto action_74
action_108 (140) = happyGoto action_426
action_108 _ = happyFail

action_109 (146) = happyShift action_94
action_109 (182) = happyShift action_95
action_109 (186) = happyShift action_96
action_109 (191) = happyShift action_97
action_109 (193) = happyShift action_98
action_109 (197) = happyShift action_99
action_109 (198) = happyShift action_100
action_109 (207) = happyShift action_101
action_109 (208) = happyShift action_102
action_109 (214) = happyShift action_103
action_109 (217) = happyAccept
action_109 (75) = happyGoto action_89
action_109 (139) = happyGoto action_425
action_109 _ = happyFail

action_110 (217) = happyAccept
action_110 _ = happyFail

action_111 _ = happyReduce_272

action_112 (217) = happyAccept
action_112 _ = happyFail

action_113 _ = happyReduce_270

action_114 (150) = happyShift action_424
action_114 _ = happyReduce_268

action_115 (217) = happyAccept
action_115 _ = happyFail

action_116 _ = happyReduce_67

action_117 (150) = happyShift action_423
action_117 _ = happyReduce_266

action_118 (217) = happyAccept
action_118 _ = happyFail

action_119 _ = happyReduce_252

action_120 _ = happyReduce_250

action_121 (146) = happyShift action_128
action_121 (182) = happyShift action_129
action_121 (211) = happyShift action_116
action_121 (212) = happyShift action_130
action_121 (213) = happyShift action_70
action_121 (214) = happyShift action_88
action_121 (215) = happyShift action_131
action_121 (70) = happyGoto action_119
action_121 (72) = happyGoto action_120
action_121 (74) = happyGoto action_145
action_121 (76) = happyGoto action_122
action_121 (78) = happyGoto action_123
action_121 (126) = happyGoto action_146
action_121 (128) = happyGoto action_422
action_121 _ = happyReduce_249

action_122 _ = happyReduce_251

action_123 _ = happyReduce_253

action_124 (150) = happyShift action_421
action_124 _ = happyReduce_264

action_125 (153) = happyShift action_420
action_125 _ = happyReduce_237

action_126 _ = happyReduce_241

action_127 (217) = happyAccept
action_127 _ = happyFail

action_128 (146) = happyShift action_128
action_128 (182) = happyShift action_129
action_128 (211) = happyShift action_116
action_128 (212) = happyShift action_130
action_128 (213) = happyShift action_70
action_128 (214) = happyShift action_88
action_128 (215) = happyShift action_131
action_128 (70) = happyGoto action_119
action_128 (72) = happyGoto action_120
action_128 (74) = happyGoto action_121
action_128 (76) = happyGoto action_122
action_128 (78) = happyGoto action_123
action_128 (122) = happyGoto action_419
action_128 (124) = happyGoto action_125
action_128 (126) = happyGoto action_126
action_128 _ = happyFail

action_129 (146) = happyShift action_128
action_129 (182) = happyShift action_129
action_129 (211) = happyShift action_116
action_129 (212) = happyShift action_130
action_129 (213) = happyShift action_70
action_129 (214) = happyShift action_88
action_129 (215) = happyShift action_131
action_129 (70) = happyGoto action_119
action_129 (72) = happyGoto action_120
action_129 (74) = happyGoto action_121
action_129 (76) = happyGoto action_122
action_129 (78) = happyGoto action_123
action_129 (122) = happyGoto action_124
action_129 (124) = happyGoto action_125
action_129 (126) = happyGoto action_126
action_129 (130) = happyGoto action_418
action_129 _ = happyReduce_263

action_130 _ = happyReduce_69

action_131 _ = happyReduce_75

action_132 _ = happyReduce_245

action_133 _ = happyReduce_243

action_134 (146) = happyShift action_141
action_134 (182) = happyShift action_142
action_134 (211) = happyShift action_67
action_134 (212) = happyShift action_143
action_134 (213) = happyShift action_73
action_134 (214) = happyShift action_103
action_134 (215) = happyShift action_144
action_134 (69) = happyGoto action_132
action_134 (71) = happyGoto action_133
action_134 (73) = happyGoto action_148
action_134 (75) = happyGoto action_135
action_134 (77) = happyGoto action_136
action_134 (125) = happyGoto action_149
action_134 (127) = happyGoto action_417
action_134 _ = happyReduce_242

action_135 _ = happyReduce_244

action_136 _ = happyReduce_246

action_137 (150) = happyShift action_416
action_137 _ = happyReduce_261

action_138 (153) = happyShift action_415
action_138 _ = happyReduce_235

action_139 _ = happyReduce_239

action_140 (217) = happyAccept
action_140 _ = happyFail

action_141 (146) = happyShift action_141
action_141 (182) = happyShift action_142
action_141 (211) = happyShift action_67
action_141 (212) = happyShift action_143
action_141 (213) = happyShift action_73
action_141 (214) = happyShift action_103
action_141 (215) = happyShift action_144
action_141 (69) = happyGoto action_132
action_141 (71) = happyGoto action_133
action_141 (73) = happyGoto action_134
action_141 (75) = happyGoto action_135
action_141 (77) = happyGoto action_136
action_141 (121) = happyGoto action_414
action_141 (123) = happyGoto action_138
action_141 (125) = happyGoto action_139
action_141 _ = happyFail

action_142 (146) = happyShift action_141
action_142 (182) = happyShift action_142
action_142 (211) = happyShift action_67
action_142 (212) = happyShift action_143
action_142 (213) = happyShift action_73
action_142 (214) = happyShift action_103
action_142 (215) = happyShift action_144
action_142 (69) = happyGoto action_132
action_142 (71) = happyGoto action_133
action_142 (73) = happyGoto action_134
action_142 (75) = happyGoto action_135
action_142 (77) = happyGoto action_136
action_142 (121) = happyGoto action_137
action_142 (123) = happyGoto action_138
action_142 (125) = happyGoto action_139
action_142 (129) = happyGoto action_413
action_142 _ = happyReduce_260

action_143 _ = happyReduce_68

action_144 _ = happyReduce_74

action_145 _ = happyReduce_249

action_146 (146) = happyShift action_128
action_146 (182) = happyShift action_129
action_146 (211) = happyShift action_116
action_146 (212) = happyShift action_130
action_146 (213) = happyShift action_70
action_146 (214) = happyShift action_88
action_146 (215) = happyShift action_131
action_146 (70) = happyGoto action_119
action_146 (72) = happyGoto action_120
action_146 (74) = happyGoto action_145
action_146 (76) = happyGoto action_122
action_146 (78) = happyGoto action_123
action_146 (126) = happyGoto action_146
action_146 (128) = happyGoto action_412
action_146 _ = happyReduce_258

action_147 (217) = happyAccept
action_147 _ = happyFail

action_148 _ = happyReduce_242

action_149 (146) = happyShift action_141
action_149 (182) = happyShift action_142
action_149 (211) = happyShift action_67
action_149 (212) = happyShift action_143
action_149 (213) = happyShift action_73
action_149 (214) = happyShift action_103
action_149 (215) = happyShift action_144
action_149 (69) = happyGoto action_132
action_149 (71) = happyGoto action_133
action_149 (73) = happyGoto action_148
action_149 (75) = happyGoto action_135
action_149 (77) = happyGoto action_136
action_149 (125) = happyGoto action_149
action_149 (127) = happyGoto action_411
action_149 _ = happyReduce_256

action_150 (217) = happyAccept
action_150 _ = happyFail

action_151 (217) = happyAccept
action_151 _ = happyFail

action_152 (217) = happyAccept
action_152 _ = happyFail

action_153 (217) = happyAccept
action_153 _ = happyFail

action_154 (217) = happyAccept
action_154 _ = happyFail

action_155 (217) = happyAccept
action_155 _ = happyFail

action_156 (217) = happyAccept
action_156 _ = happyFail

action_157 (213) = happyShift action_70
action_157 (217) = happyAccept
action_157 (74) = happyGoto action_159
action_157 (118) = happyGoto action_410
action_157 _ = happyFail

action_158 (213) = happyShift action_73
action_158 (217) = happyAccept
action_158 (73) = happyGoto action_161
action_158 (117) = happyGoto action_409
action_158 _ = happyFail

action_159 _ = happyReduce_229

action_160 (217) = happyAccept
action_160 _ = happyFail

action_161 _ = happyReduce_228

action_162 (217) = happyAccept
action_162 _ = happyFail

action_163 (146) = happyShift action_169
action_163 (150) = happyShift action_408
action_163 (182) = happyShift action_170
action_163 (213) = happyShift action_70
action_163 (74) = happyGoto action_167
action_163 (114) = happyGoto action_402
action_163 _ = happyReduce_226

action_164 (217) = happyAccept
action_164 _ = happyFail

action_165 (146) = happyShift action_173
action_165 (150) = happyShift action_407
action_165 (182) = happyShift action_174
action_165 (213) = happyShift action_73
action_165 (73) = happyGoto action_171
action_165 (113) = happyGoto action_401
action_165 _ = happyReduce_224

action_166 (217) = happyAccept
action_166 _ = happyFail

action_167 _ = happyReduce_221

action_168 (217) = happyAccept
action_168 _ = happyFail

action_169 (112) = happyGoto action_163
action_169 (116) = happyGoto action_406
action_169 _ = happyFail

action_170 (112) = happyGoto action_405
action_170 _ = happyFail

action_171 _ = happyReduce_218

action_172 (217) = happyAccept
action_172 _ = happyFail

action_173 (111) = happyGoto action_165
action_173 (115) = happyGoto action_404
action_173 _ = happyFail

action_174 (111) = happyGoto action_403
action_174 _ = happyFail

action_175 (146) = happyShift action_169
action_175 (182) = happyShift action_170
action_175 (213) = happyShift action_70
action_175 (217) = happyAccept
action_175 (74) = happyGoto action_167
action_175 (114) = happyGoto action_402
action_175 _ = happyFail

action_176 (146) = happyShift action_173
action_176 (182) = happyShift action_174
action_176 (213) = happyShift action_73
action_176 (217) = happyAccept
action_176 (73) = happyGoto action_171
action_176 (113) = happyGoto action_401
action_176 _ = happyFail

action_177 _ = happyReduce_214

action_178 (217) = happyAccept
action_178 _ = happyFail

action_179 _ = happyReduce_212

action_180 (217) = happyAccept
action_180 _ = happyFail

action_181 _ = happyReduce_206

action_182 (217) = happyAccept
action_182 _ = happyFail

action_183 (213) = happyShift action_70
action_183 (74) = happyGoto action_177
action_183 (110) = happyGoto action_400
action_183 _ = happyReduce_215

action_184 (153) = happyShift action_399
action_184 _ = happyFail

action_185 (183) = happyShift action_398
action_185 _ = happyFail

action_186 _ = happyReduce_207

action_187 _ = happyReduce_200

action_188 (217) = happyAccept
action_188 _ = happyFail

action_189 (213) = happyShift action_73
action_189 (73) = happyGoto action_179
action_189 (109) = happyGoto action_397
action_189 _ = happyReduce_213

action_190 (153) = happyShift action_396
action_190 _ = happyFail

action_191 (183) = happyShift action_395
action_191 _ = happyFail

action_192 _ = happyReduce_201

action_193 _ = happyReduce_199

action_194 (217) = happyAccept
action_194 _ = happyFail

action_195 (157) = happyShift action_201
action_195 (182) = happyShift action_195
action_195 (213) = happyShift action_70
action_195 (74) = happyGoto action_193
action_195 (104) = happyGoto action_394
action_195 (106) = happyGoto action_200
action_195 _ = happyFail

action_196 _ = happyReduce_197

action_197 (217) = happyAccept
action_197 _ = happyFail

action_198 (157) = happyShift action_204
action_198 (182) = happyShift action_198
action_198 (213) = happyShift action_73
action_198 (73) = happyGoto action_196
action_198 (103) = happyGoto action_393
action_198 (105) = happyGoto action_203
action_198 _ = happyFail

action_199 (217) = happyAccept
action_199 _ = happyFail

action_200 _ = happyReduce_195

action_201 (182) = happyShift action_195
action_201 (213) = happyShift action_70
action_201 (74) = happyGoto action_193
action_201 (106) = happyGoto action_392
action_201 _ = happyFail

action_202 (217) = happyAccept
action_202 _ = happyFail

action_203 _ = happyReduce_193

action_204 (182) = happyShift action_198
action_204 (213) = happyShift action_73
action_204 (73) = happyGoto action_196
action_204 (105) = happyGoto action_391
action_204 _ = happyFail

action_205 (217) = happyAccept
action_205 _ = happyFail

action_206 _ = happyReduce_191

action_207 _ = happyReduce_187

action_208 _ = happyReduce_190

action_209 _ = happyReduce_188

action_210 _ = happyReduce_189

action_211 (217) = happyAccept
action_211 _ = happyFail

action_212 _ = happyReduce_186

action_213 _ = happyReduce_182

action_214 _ = happyReduce_185

action_215 _ = happyReduce_183

action_216 _ = happyReduce_184

action_217 (217) = happyAccept
action_217 _ = happyFail

action_218 _ = happyReduce_180

action_219 _ = happyReduce_174

action_220 _ = happyReduce_177

action_221 _ = happyReduce_175

action_222 _ = happyReduce_178

action_223 (212) = happyShift action_130
action_223 (72) = happyGoto action_390
action_223 _ = happyFail

action_224 _ = happyReduce_173

action_225 _ = happyReduce_179

action_226 _ = happyReduce_176

action_227 (217) = happyAccept
action_227 _ = happyFail

action_228 _ = happyReduce_171

action_229 _ = happyReduce_165

action_230 _ = happyReduce_168

action_231 _ = happyReduce_166

action_232 _ = happyReduce_169

action_233 (212) = happyShift action_143
action_233 (71) = happyGoto action_389
action_233 _ = happyFail

action_234 _ = happyReduce_164

action_235 _ = happyReduce_170

action_236 _ = happyReduce_167

action_237 (217) = happyAccept
action_237 _ = happyFail

action_238 _ = happyReduce_163

action_239 (160) = happyShift action_218
action_239 (162) = happyShift action_219
action_239 (164) = happyShift action_220
action_239 (170) = happyShift action_221
action_239 (174) = happyShift action_222
action_239 (176) = happyShift action_223
action_239 (177) = happyShift action_224
action_239 (180) = happyShift action_225
action_239 (181) = happyShift action_226
action_239 (100) = happyGoto action_388
action_239 _ = happyFail

action_240 _ = happyReduce_158

action_241 (160) = happyShift action_218
action_241 (162) = happyShift action_219
action_241 (164) = happyShift action_220
action_241 (170) = happyShift action_221
action_241 (174) = happyShift action_222
action_241 (176) = happyShift action_223
action_241 (177) = happyShift action_224
action_241 (180) = happyShift action_225
action_241 (181) = happyShift action_226
action_241 (100) = happyGoto action_387
action_241 _ = happyFail

action_242 _ = happyReduce_157

action_243 (212) = happyShift action_130
action_243 (72) = happyGoto action_386
action_243 _ = happyFail

action_244 (212) = happyShift action_130
action_244 (72) = happyGoto action_385
action_244 _ = happyFail

action_245 (217) = happyAccept
action_245 _ = happyFail

action_246 _ = happyReduce_156

action_247 (160) = happyShift action_228
action_247 (162) = happyShift action_229
action_247 (164) = happyShift action_230
action_247 (170) = happyShift action_231
action_247 (174) = happyShift action_232
action_247 (176) = happyShift action_233
action_247 (177) = happyShift action_234
action_247 (180) = happyShift action_235
action_247 (181) = happyShift action_236
action_247 (99) = happyGoto action_384
action_247 _ = happyFail

action_248 _ = happyReduce_151

action_249 (160) = happyShift action_228
action_249 (162) = happyShift action_229
action_249 (164) = happyShift action_230
action_249 (170) = happyShift action_231
action_249 (174) = happyShift action_232
action_249 (176) = happyShift action_233
action_249 (177) = happyShift action_234
action_249 (180) = happyShift action_235
action_249 (181) = happyShift action_236
action_249 (99) = happyGoto action_383
action_249 _ = happyFail

action_250 _ = happyReduce_150

action_251 (212) = happyShift action_143
action_251 (71) = happyGoto action_382
action_251 _ = happyFail

action_252 (212) = happyShift action_143
action_252 (71) = happyGoto action_381
action_252 _ = happyFail

action_253 (217) = happyAccept
action_253 _ = happyFail

action_254 (150) = happyShift action_380
action_254 _ = happyReduce_148

action_255 (217) = happyAccept
action_255 _ = happyFail

action_256 (150) = happyShift action_379
action_256 _ = happyReduce_146

action_257 (217) = happyAccept
action_257 _ = happyFail

action_258 (159) = happyShift action_239
action_258 (161) = happyShift action_206
action_258 (163) = happyShift action_207
action_258 (168) = happyShift action_240
action_258 (169) = happyShift action_241
action_258 (171) = happyShift action_208
action_258 (172) = happyShift action_209
action_258 (173) = happyShift action_242
action_258 (175) = happyShift action_243
action_258 (178) = happyShift action_244
action_258 (179) = happyShift action_210
action_258 (96) = happyGoto action_378
action_258 (98) = happyGoto action_254
action_258 (102) = happyGoto action_238
action_258 _ = happyFail

action_259 (217) = happyAccept
action_259 _ = happyFail

action_260 (159) = happyShift action_247
action_260 (161) = happyShift action_212
action_260 (163) = happyShift action_213
action_260 (168) = happyShift action_248
action_260 (169) = happyShift action_249
action_260 (171) = happyShift action_214
action_260 (172) = happyShift action_215
action_260 (173) = happyShift action_250
action_260 (175) = happyShift action_251
action_260 (178) = happyShift action_252
action_260 (179) = happyShift action_216
action_260 (95) = happyGoto action_377
action_260 (97) = happyGoto action_256
action_260 (101) = happyGoto action_246
action_260 _ = happyFail

action_261 (146) = happyShift action_258
action_261 (94) = happyGoto action_376
action_261 _ = happyReduce_145

action_262 (217) = happyAccept
action_262 _ = happyFail

action_263 (146) = happyShift action_258
action_263 (94) = happyGoto action_375
action_263 _ = happyReduce_145

action_264 (146) = happyShift action_260
action_264 (93) = happyGoto action_374
action_264 _ = happyReduce_143

action_265 (217) = happyAccept
action_265 _ = happyFail

action_266 (146) = happyShift action_260
action_266 (93) = happyGoto action_373
action_266 _ = happyReduce_143

action_267 (157) = happyShift action_201
action_267 (182) = happyShift action_195
action_267 (211) = happyShift action_116
action_267 (213) = happyShift action_70
action_267 (70) = happyGoto action_261
action_267 (74) = happyGoto action_193
action_267 (92) = happyGoto action_328
action_267 (104) = happyGoto action_263
action_267 (106) = happyGoto action_200
action_267 _ = happyReduce_132

action_268 (209) = happyShift action_372
action_268 _ = happyReduce_136

action_269 (217) = happyAccept
action_269 _ = happyFail

action_270 (146) = happyShift action_79
action_270 (182) = happyShift action_80
action_270 (186) = happyShift action_81
action_270 (191) = happyShift action_82
action_270 (193) = happyShift action_83
action_270 (197) = happyShift action_84
action_270 (198) = happyShift action_85
action_270 (207) = happyShift action_86
action_270 (208) = happyShift action_87
action_270 (214) = happyShift action_88
action_270 (76) = happyGoto action_74
action_270 (136) = happyGoto action_75
action_270 (138) = happyGoto action_76
action_270 (140) = happyGoto action_77
action_270 (142) = happyGoto action_371
action_270 _ = happyFail

action_271 (157) = happyShift action_204
action_271 (182) = happyShift action_198
action_271 (211) = happyShift action_67
action_271 (213) = happyShift action_73
action_271 (69) = happyGoto action_264
action_271 (73) = happyGoto action_196
action_271 (91) = happyGoto action_327
action_271 (103) = happyGoto action_266
action_271 (105) = happyGoto action_203
action_271 _ = happyReduce_130

action_272 (209) = happyShift action_370
action_272 _ = happyReduce_134

action_273 (217) = happyAccept
action_273 _ = happyFail

action_274 (146) = happyShift action_94
action_274 (182) = happyShift action_95
action_274 (186) = happyShift action_96
action_274 (191) = happyShift action_97
action_274 (193) = happyShift action_98
action_274 (197) = happyShift action_99
action_274 (198) = happyShift action_100
action_274 (207) = happyShift action_101
action_274 (208) = happyShift action_102
action_274 (214) = happyShift action_103
action_274 (75) = happyGoto action_89
action_274 (135) = happyGoto action_90
action_274 (137) = happyGoto action_91
action_274 (139) = happyGoto action_92
action_274 (141) = happyGoto action_369
action_274 _ = happyFail

action_275 (217) = happyAccept
action_275 _ = happyFail

action_276 (217) = happyAccept
action_276 _ = happyFail

action_277 (217) = happyAccept
action_277 _ = happyFail

action_278 (152) = happyShift action_368
action_278 _ = happyFail

action_279 (160) = happyShift action_218
action_279 (162) = happyShift action_219
action_279 (164) = happyShift action_220
action_279 (170) = happyShift action_221
action_279 (174) = happyShift action_222
action_279 (176) = happyShift action_223
action_279 (177) = happyShift action_224
action_279 (180) = happyShift action_225
action_279 (181) = happyShift action_226
action_279 (100) = happyGoto action_367
action_279 _ = happyFail

action_280 (160) = happyShift action_218
action_280 (162) = happyShift action_219
action_280 (164) = happyShift action_220
action_280 (170) = happyShift action_221
action_280 (174) = happyShift action_222
action_280 (176) = happyShift action_223
action_280 (177) = happyShift action_224
action_280 (180) = happyShift action_225
action_280 (181) = happyShift action_226
action_280 (100) = happyGoto action_366
action_280 _ = happyFail

action_281 (161) = happyShift action_206
action_281 (163) = happyShift action_207
action_281 (171) = happyShift action_208
action_281 (172) = happyShift action_209
action_281 (179) = happyShift action_210
action_281 (102) = happyGoto action_365
action_281 _ = happyFail

action_282 (211) = happyShift action_116
action_282 (70) = happyGoto action_364
action_282 _ = happyFail

action_283 (213) = happyShift action_70
action_283 (74) = happyGoto action_363
action_283 _ = happyFail

action_284 (211) = happyShift action_116
action_284 (70) = happyGoto action_362
action_284 _ = happyFail

action_285 (213) = happyShift action_70
action_285 (74) = happyGoto action_361
action_285 _ = happyFail

action_286 (213) = happyShift action_70
action_286 (74) = happyGoto action_68
action_286 (144) = happyGoto action_360
action_286 _ = happyFail

action_287 (213) = happyShift action_70
action_287 (74) = happyGoto action_68
action_287 (144) = happyGoto action_359
action_287 _ = happyFail

action_288 (213) = happyShift action_70
action_288 (74) = happyGoto action_358
action_288 _ = happyFail

action_289 (145) = happyShift action_183
action_289 (146) = happyShift action_184
action_289 (182) = happyShift action_185
action_289 (184) = happyShift action_186
action_289 (213) = happyShift action_70
action_289 (74) = happyGoto action_181
action_289 (108) = happyGoto action_357
action_289 _ = happyFail

action_290 (203) = happyShift action_355
action_290 (206) = happyShift action_356
action_290 (211) = happyShift action_116
action_290 (70) = happyGoto action_114
action_290 (132) = happyGoto action_354
action_290 _ = happyFail

action_291 (205) = happyShift action_353
action_291 _ = happyFail

action_292 (213) = happyShift action_70
action_292 (74) = happyGoto action_352
action_292 _ = happyFail

action_293 (199) = happyShift action_111
action_293 (134) = happyGoto action_351
action_293 _ = happyReduce_273

action_294 (199) = happyShift action_111
action_294 (134) = happyGoto action_350
action_294 _ = happyReduce_273

action_295 (213) = happyShift action_70
action_295 (74) = happyGoto action_349
action_295 _ = happyFail

action_296 (217) = happyAccept
action_296 _ = happyFail

action_297 (152) = happyShift action_348
action_297 _ = happyFail

action_298 (160) = happyShift action_228
action_298 (162) = happyShift action_229
action_298 (164) = happyShift action_230
action_298 (170) = happyShift action_231
action_298 (174) = happyShift action_232
action_298 (176) = happyShift action_233
action_298 (177) = happyShift action_234
action_298 (180) = happyShift action_235
action_298 (181) = happyShift action_236
action_298 (99) = happyGoto action_347
action_298 _ = happyFail

action_299 (160) = happyShift action_228
action_299 (162) = happyShift action_229
action_299 (164) = happyShift action_230
action_299 (170) = happyShift action_231
action_299 (174) = happyShift action_232
action_299 (176) = happyShift action_233
action_299 (177) = happyShift action_234
action_299 (180) = happyShift action_235
action_299 (181) = happyShift action_236
action_299 (99) = happyGoto action_346
action_299 _ = happyFail

action_300 (161) = happyShift action_212
action_300 (163) = happyShift action_213
action_300 (171) = happyShift action_214
action_300 (172) = happyShift action_215
action_300 (179) = happyShift action_216
action_300 (101) = happyGoto action_345
action_300 _ = happyFail

action_301 (211) = happyShift action_67
action_301 (69) = happyGoto action_344
action_301 _ = happyFail

action_302 (213) = happyShift action_73
action_302 (73) = happyGoto action_343
action_302 _ = happyFail

action_303 (211) = happyShift action_67
action_303 (69) = happyGoto action_342
action_303 _ = happyFail

action_304 (213) = happyShift action_73
action_304 (73) = happyGoto action_341
action_304 _ = happyFail

action_305 (213) = happyShift action_73
action_305 (73) = happyGoto action_71
action_305 (143) = happyGoto action_340
action_305 _ = happyFail

action_306 (213) = happyShift action_73
action_306 (73) = happyGoto action_71
action_306 (143) = happyGoto action_339
action_306 _ = happyFail

action_307 (213) = happyShift action_73
action_307 (73) = happyGoto action_338
action_307 _ = happyFail

action_308 (145) = happyShift action_189
action_308 (146) = happyShift action_190
action_308 (182) = happyShift action_191
action_308 (184) = happyShift action_192
action_308 (213) = happyShift action_73
action_308 (73) = happyGoto action_187
action_308 (107) = happyGoto action_337
action_308 _ = happyFail

action_309 (203) = happyShift action_335
action_309 (206) = happyShift action_336
action_309 (211) = happyShift action_67
action_309 (69) = happyGoto action_117
action_309 (131) = happyGoto action_334
action_309 _ = happyFail

action_310 (205) = happyShift action_333
action_310 _ = happyFail

action_311 (213) = happyShift action_73
action_311 (73) = happyGoto action_332
action_311 _ = happyFail

action_312 (199) = happyShift action_113
action_312 (133) = happyGoto action_331
action_312 _ = happyReduce_271

action_313 (199) = happyShift action_113
action_313 (133) = happyGoto action_330
action_313 _ = happyReduce_271

action_314 (213) = happyShift action_73
action_314 (73) = happyGoto action_329
action_314 _ = happyFail

action_315 (157) = happyShift action_201
action_315 (182) = happyShift action_195
action_315 (211) = happyShift action_116
action_315 (213) = happyShift action_70
action_315 (217) = happyAccept
action_315 (70) = happyGoto action_261
action_315 (74) = happyGoto action_193
action_315 (92) = happyGoto action_328
action_315 (104) = happyGoto action_263
action_315 (106) = happyGoto action_200
action_315 _ = happyFail

action_316 (157) = happyShift action_204
action_316 (182) = happyShift action_198
action_316 (211) = happyShift action_67
action_316 (213) = happyShift action_73
action_316 (217) = happyAccept
action_316 (69) = happyGoto action_264
action_316 (73) = happyGoto action_196
action_316 (91) = happyGoto action_327
action_316 (103) = happyGoto action_266
action_316 (105) = happyGoto action_203
action_316 _ = happyFail

action_317 (217) = happyAccept
action_317 _ = happyFail

action_318 (155) = happyShift action_326
action_318 _ = happyReduce_82

action_319 (217) = happyAccept
action_319 _ = happyFail

action_320 (155) = happyShift action_325
action_320 _ = happyReduce_79

action_321 (217) = happyAccept
action_321 _ = happyFail

action_322 _ = happyReduce_77

action_323 (217) = happyAccept
action_323 _ = happyFail

action_324 _ = happyReduce_76

action_325 (145) = happyShift action_189
action_325 (146) = happyShift action_190
action_325 (165) = happyShift action_298
action_325 (166) = happyShift action_299
action_325 (167) = happyShift action_300
action_325 (182) = happyShift action_191
action_325 (184) = happyShift action_192
action_325 (185) = happyShift action_301
action_325 (187) = happyShift action_302
action_325 (188) = happyShift action_303
action_325 (189) = happyShift action_304
action_325 (190) = happyShift action_305
action_325 (192) = happyShift action_306
action_325 (194) = happyShift action_307
action_325 (195) = happyShift action_308
action_325 (196) = happyShift action_309
action_325 (200) = happyShift action_310
action_325 (201) = happyShift action_311
action_325 (202) = happyShift action_312
action_325 (204) = happyShift action_313
action_325 (205) = happyShift action_314
action_325 (213) = happyShift action_73
action_325 (73) = happyGoto action_187
action_325 (81) = happyGoto action_514
action_325 (85) = happyGoto action_320
action_325 (107) = happyGoto action_297
action_325 _ = happyReduce_78

action_326 (145) = happyShift action_183
action_326 (146) = happyShift action_184
action_326 (165) = happyShift action_279
action_326 (166) = happyShift action_280
action_326 (167) = happyShift action_281
action_326 (182) = happyShift action_185
action_326 (184) = happyShift action_186
action_326 (185) = happyShift action_282
action_326 (187) = happyShift action_283
action_326 (188) = happyShift action_284
action_326 (189) = happyShift action_285
action_326 (190) = happyShift action_286
action_326 (192) = happyShift action_287
action_326 (194) = happyShift action_288
action_326 (195) = happyShift action_289
action_326 (196) = happyShift action_290
action_326 (200) = happyShift action_291
action_326 (201) = happyShift action_292
action_326 (202) = happyShift action_293
action_326 (204) = happyShift action_294
action_326 (205) = happyShift action_295
action_326 (213) = happyShift action_70
action_326 (74) = happyGoto action_181
action_326 (82) = happyGoto action_513
action_326 (86) = happyGoto action_318
action_326 (108) = happyGoto action_278
action_326 _ = happyReduce_81

action_327 _ = happyReduce_85

action_328 _ = happyReduce_87

action_329 (146) = happyShift action_94
action_329 (182) = happyShift action_95
action_329 (186) = happyShift action_96
action_329 (191) = happyShift action_97
action_329 (193) = happyShift action_98
action_329 (197) = happyShift action_99
action_329 (198) = happyShift action_100
action_329 (207) = happyShift action_101
action_329 (208) = happyShift action_102
action_329 (214) = happyShift action_103
action_329 (75) = happyGoto action_89
action_329 (135) = happyGoto action_90
action_329 (137) = happyGoto action_91
action_329 (139) = happyGoto action_92
action_329 (141) = happyGoto action_512
action_329 _ = happyFail

action_330 (157) = happyShift action_204
action_330 (182) = happyShift action_198
action_330 (213) = happyShift action_73
action_330 (73) = happyGoto action_196
action_330 (103) = happyGoto action_511
action_330 (105) = happyGoto action_203
action_330 _ = happyFail

action_331 (157) = happyShift action_204
action_331 (182) = happyShift action_198
action_331 (213) = happyShift action_73
action_331 (73) = happyGoto action_196
action_331 (103) = happyGoto action_510
action_331 (105) = happyGoto action_203
action_331 _ = happyFail

action_332 (154) = happyShift action_509
action_332 _ = happyFail

action_333 (213) = happyShift action_73
action_333 (73) = happyGoto action_508
action_333 _ = happyFail

action_334 _ = happyReduce_106

action_335 (211) = happyShift action_67
action_335 (69) = happyGoto action_117
action_335 (131) = happyGoto action_507
action_335 _ = happyFail

action_336 _ = happyReduce_108

action_337 (152) = happyShift action_506
action_337 _ = happyFail

action_338 (156) = happyShift action_505
action_338 _ = happyFail

action_339 _ = happyReduce_94

action_340 _ = happyReduce_102

action_341 (119) = happyGoto action_504
action_341 _ = happyReduce_230

action_342 (211) = happyShift action_67
action_342 (69) = happyGoto action_503
action_342 _ = happyReduce_89

action_343 (212) = happyShift action_143
action_343 (71) = happyGoto action_502
action_343 _ = happyFail

action_344 (211) = happyShift action_67
action_344 (69) = happyGoto action_501
action_344 _ = happyFail

action_345 _ = happyReduce_105

action_346 _ = happyReduce_103

action_347 _ = happyReduce_104

action_348 (157) = happyShift action_204
action_348 (182) = happyShift action_198
action_348 (213) = happyShift action_73
action_348 (73) = happyGoto action_196
action_348 (103) = happyGoto action_500
action_348 (105) = happyGoto action_203
action_348 _ = happyFail

action_349 (146) = happyShift action_79
action_349 (182) = happyShift action_80
action_349 (186) = happyShift action_81
action_349 (191) = happyShift action_82
action_349 (193) = happyShift action_83
action_349 (197) = happyShift action_84
action_349 (198) = happyShift action_85
action_349 (207) = happyShift action_86
action_349 (208) = happyShift action_87
action_349 (214) = happyShift action_88
action_349 (76) = happyGoto action_74
action_349 (136) = happyGoto action_75
action_349 (138) = happyGoto action_76
action_349 (140) = happyGoto action_77
action_349 (142) = happyGoto action_499
action_349 _ = happyFail

action_350 (157) = happyShift action_201
action_350 (182) = happyShift action_195
action_350 (213) = happyShift action_70
action_350 (74) = happyGoto action_193
action_350 (104) = happyGoto action_498
action_350 (106) = happyGoto action_200
action_350 _ = happyFail

action_351 (157) = happyShift action_201
action_351 (182) = happyShift action_195
action_351 (213) = happyShift action_70
action_351 (74) = happyGoto action_193
action_351 (104) = happyGoto action_497
action_351 (106) = happyGoto action_200
action_351 _ = happyFail

action_352 (154) = happyShift action_496
action_352 _ = happyFail

action_353 (213) = happyShift action_70
action_353 (74) = happyGoto action_495
action_353 _ = happyFail

action_354 _ = happyReduce_127

action_355 (211) = happyShift action_116
action_355 (70) = happyGoto action_114
action_355 (132) = happyGoto action_494
action_355 _ = happyFail

action_356 _ = happyReduce_129

action_357 (152) = happyShift action_493
action_357 _ = happyFail

action_358 (156) = happyShift action_492
action_358 _ = happyFail

action_359 _ = happyReduce_115

action_360 _ = happyReduce_123

action_361 (120) = happyGoto action_491
action_361 _ = happyReduce_232

action_362 (211) = happyShift action_116
action_362 (70) = happyGoto action_490
action_362 _ = happyReduce_110

action_363 (212) = happyShift action_130
action_363 (72) = happyGoto action_489
action_363 _ = happyFail

action_364 (211) = happyShift action_116
action_364 (70) = happyGoto action_488
action_364 _ = happyFail

action_365 _ = happyReduce_126

action_366 _ = happyReduce_124

action_367 _ = happyReduce_125

action_368 (157) = happyShift action_201
action_368 (182) = happyShift action_195
action_368 (213) = happyShift action_70
action_368 (74) = happyGoto action_193
action_368 (104) = happyGoto action_487
action_368 (106) = happyGoto action_200
action_368 _ = happyFail

action_369 _ = happyReduce_131

action_370 (158) = happyShift action_274
action_370 (83) = happyGoto action_271
action_370 (87) = happyGoto action_272
action_370 (89) = happyGoto action_486
action_370 _ = happyReduce_84

action_371 _ = happyReduce_133

action_372 (158) = happyShift action_270
action_372 (84) = happyGoto action_267
action_372 (88) = happyGoto action_268
action_372 (90) = happyGoto action_485
action_372 _ = happyReduce_86

action_373 _ = happyReduce_139

action_374 _ = happyReduce_138

action_375 _ = happyReduce_141

action_376 _ = happyReduce_140

action_377 (147) = happyShift action_484
action_377 _ = happyFail

action_378 (147) = happyShift action_483
action_378 _ = happyFail

action_379 (159) = happyShift action_247
action_379 (161) = happyShift action_212
action_379 (163) = happyShift action_213
action_379 (168) = happyShift action_248
action_379 (169) = happyShift action_249
action_379 (171) = happyShift action_214
action_379 (172) = happyShift action_215
action_379 (173) = happyShift action_250
action_379 (175) = happyShift action_251
action_379 (178) = happyShift action_252
action_379 (179) = happyShift action_216
action_379 (95) = happyGoto action_482
action_379 (97) = happyGoto action_256
action_379 (101) = happyGoto action_246
action_379 _ = happyFail

action_380 (159) = happyShift action_239
action_380 (161) = happyShift action_206
action_380 (163) = happyShift action_207
action_380 (168) = happyShift action_240
action_380 (169) = happyShift action_241
action_380 (171) = happyShift action_208
action_380 (172) = happyShift action_209
action_380 (173) = happyShift action_242
action_380 (175) = happyShift action_243
action_380 (178) = happyShift action_244
action_380 (179) = happyShift action_210
action_380 (96) = happyGoto action_481
action_380 (98) = happyGoto action_254
action_380 (102) = happyGoto action_238
action_380 _ = happyFail

action_381 _ = happyReduce_152

action_382 _ = happyReduce_153

action_383 _ = happyReduce_154

action_384 _ = happyReduce_155

action_385 _ = happyReduce_159

action_386 _ = happyReduce_160

action_387 _ = happyReduce_161

action_388 _ = happyReduce_162

action_389 (212) = happyShift action_143
action_389 (71) = happyGoto action_480
action_389 _ = happyFail

action_390 (212) = happyShift action_130
action_390 (72) = happyGoto action_479
action_390 _ = happyFail

action_391 _ = happyReduce_192

action_392 _ = happyReduce_194

action_393 (183) = happyShift action_478
action_393 _ = happyFail

action_394 (183) = happyShift action_477
action_394 _ = happyFail

action_395 _ = happyReduce_202

action_396 (147) = happyShift action_475
action_396 (182) = happyShift action_476
action_396 _ = happyFail

action_397 _ = happyReduce_205

action_398 _ = happyReduce_208

action_399 (147) = happyShift action_473
action_399 (182) = happyShift action_474
action_399 _ = happyFail

action_400 _ = happyReduce_211

action_401 _ = happyReduce_216

action_402 _ = happyReduce_217

action_403 (146) = happyShift action_173
action_403 (182) = happyShift action_174
action_403 (183) = happyShift action_472
action_403 (213) = happyShift action_73
action_403 (73) = happyGoto action_171
action_403 (113) = happyGoto action_401
action_403 _ = happyFail

action_404 (147) = happyShift action_471
action_404 _ = happyFail

action_405 (146) = happyShift action_169
action_405 (182) = happyShift action_170
action_405 (183) = happyShift action_470
action_405 (213) = happyShift action_70
action_405 (74) = happyGoto action_167
action_405 (114) = happyGoto action_402
action_405 _ = happyFail

action_406 (147) = happyShift action_469
action_406 _ = happyFail

action_407 (111) = happyGoto action_165
action_407 (115) = happyGoto action_468
action_407 _ = happyFail

action_408 (112) = happyGoto action_163
action_408 (116) = happyGoto action_467
action_408 _ = happyFail

action_409 _ = happyReduce_231

action_410 _ = happyReduce_233

action_411 _ = happyReduce_257

action_412 _ = happyReduce_259

action_413 (183) = happyShift action_466
action_413 _ = happyFail

action_414 (147) = happyShift action_465
action_414 _ = happyFail

action_415 (146) = happyShift action_141
action_415 (182) = happyShift action_142
action_415 (211) = happyShift action_67
action_415 (212) = happyShift action_143
action_415 (213) = happyShift action_73
action_415 (214) = happyShift action_103
action_415 (215) = happyShift action_144
action_415 (69) = happyGoto action_132
action_415 (71) = happyGoto action_133
action_415 (73) = happyGoto action_134
action_415 (75) = happyGoto action_135
action_415 (77) = happyGoto action_136
action_415 (121) = happyGoto action_464
action_415 (123) = happyGoto action_138
action_415 (125) = happyGoto action_139
action_415 _ = happyFail

action_416 (146) = happyShift action_141
action_416 (182) = happyShift action_142
action_416 (211) = happyShift action_67
action_416 (212) = happyShift action_143
action_416 (213) = happyShift action_73
action_416 (214) = happyShift action_103
action_416 (215) = happyShift action_144
action_416 (69) = happyGoto action_132
action_416 (71) = happyGoto action_133
action_416 (73) = happyGoto action_134
action_416 (75) = happyGoto action_135
action_416 (77) = happyGoto action_136
action_416 (121) = happyGoto action_137
action_416 (123) = happyGoto action_138
action_416 (125) = happyGoto action_139
action_416 (129) = happyGoto action_463
action_416 _ = happyReduce_260

action_417 _ = happyReduce_238

action_418 (183) = happyShift action_462
action_418 _ = happyFail

action_419 (147) = happyShift action_461
action_419 _ = happyFail

action_420 (146) = happyShift action_128
action_420 (182) = happyShift action_129
action_420 (211) = happyShift action_116
action_420 (212) = happyShift action_130
action_420 (213) = happyShift action_70
action_420 (214) = happyShift action_88
action_420 (215) = happyShift action_131
action_420 (70) = happyGoto action_119
action_420 (72) = happyGoto action_120
action_420 (74) = happyGoto action_121
action_420 (76) = happyGoto action_122
action_420 (78) = happyGoto action_123
action_420 (122) = happyGoto action_460
action_420 (124) = happyGoto action_125
action_420 (126) = happyGoto action_126
action_420 _ = happyFail

action_421 (146) = happyShift action_128
action_421 (182) = happyShift action_129
action_421 (211) = happyShift action_116
action_421 (212) = happyShift action_130
action_421 (213) = happyShift action_70
action_421 (214) = happyShift action_88
action_421 (215) = happyShift action_131
action_421 (70) = happyGoto action_119
action_421 (72) = happyGoto action_120
action_421 (74) = happyGoto action_121
action_421 (76) = happyGoto action_122
action_421 (78) = happyGoto action_123
action_421 (122) = happyGoto action_124
action_421 (124) = happyGoto action_125
action_421 (126) = happyGoto action_126
action_421 (130) = happyGoto action_459
action_421 _ = happyReduce_263

action_422 _ = happyReduce_240

action_423 (211) = happyShift action_67
action_423 (69) = happyGoto action_117
action_423 (131) = happyGoto action_458
action_423 _ = happyFail

action_424 (211) = happyShift action_116
action_424 (70) = happyGoto action_114
action_424 (132) = happyGoto action_457
action_424 _ = happyFail

action_425 (148) = happyShift action_429
action_425 (149) = happyShift action_430
action_425 (157) = happyShift action_431
action_425 _ = happyReduce_274

action_426 (148) = happyShift action_432
action_426 (149) = happyShift action_433
action_426 (157) = happyShift action_434
action_426 _ = happyReduce_276

action_427 (146) = happyShift action_94
action_427 (182) = happyShift action_95
action_427 (186) = happyShift action_96
action_427 (191) = happyShift action_97
action_427 (193) = happyShift action_98
action_427 (197) = happyShift action_99
action_427 (198) = happyShift action_100
action_427 (207) = happyShift action_101
action_427 (208) = happyShift action_102
action_427 (214) = happyShift action_103
action_427 (75) = happyGoto action_89
action_427 (135) = happyGoto action_456
action_427 (139) = happyGoto action_92
action_427 _ = happyFail

action_428 (146) = happyShift action_79
action_428 (182) = happyShift action_80
action_428 (186) = happyShift action_81
action_428 (191) = happyShift action_82
action_428 (193) = happyShift action_83
action_428 (197) = happyShift action_84
action_428 (198) = happyShift action_85
action_428 (207) = happyShift action_86
action_428 (208) = happyShift action_87
action_428 (214) = happyShift action_88
action_428 (76) = happyGoto action_74
action_428 (136) = happyGoto action_455
action_428 (140) = happyGoto action_77
action_428 _ = happyFail

action_429 _ = happyReduce_284

action_430 _ = happyReduce_285

action_431 _ = happyReduce_286

action_432 _ = happyReduce_297

action_433 _ = happyReduce_298

action_434 _ = happyReduce_299

action_435 (210) = happyShift action_454
action_435 _ = happyFail

action_436 (183) = happyShift action_453
action_436 _ = happyFail

action_437 (147) = happyShift action_452
action_437 _ = happyFail

action_438 (146) = happyShift action_94
action_438 (182) = happyShift action_95
action_438 (186) = happyShift action_96
action_438 (191) = happyShift action_97
action_438 (193) = happyShift action_98
action_438 (197) = happyShift action_99
action_438 (198) = happyShift action_100
action_438 (207) = happyShift action_101
action_438 (208) = happyShift action_102
action_438 (214) = happyShift action_103
action_438 (75) = happyGoto action_89
action_438 (135) = happyGoto action_451
action_438 (139) = happyGoto action_92
action_438 _ = happyFail

action_439 (210) = happyShift action_450
action_439 _ = happyFail

action_440 (183) = happyShift action_449
action_440 _ = happyFail

action_441 (147) = happyShift action_448
action_441 _ = happyFail

action_442 (146) = happyShift action_79
action_442 (182) = happyShift action_80
action_442 (186) = happyShift action_81
action_442 (191) = happyShift action_82
action_442 (193) = happyShift action_83
action_442 (197) = happyShift action_84
action_442 (198) = happyShift action_85
action_442 (207) = happyShift action_86
action_442 (208) = happyShift action_87
action_442 (214) = happyShift action_88
action_442 (76) = happyGoto action_74
action_442 (136) = happyGoto action_447
action_442 (140) = happyGoto action_77
action_442 _ = happyFail

action_443 (213) = happyShift action_73
action_443 (73) = happyGoto action_71
action_443 (143) = happyGoto action_446
action_443 _ = happyFail

action_444 (213) = happyShift action_70
action_444 (74) = happyGoto action_68
action_444 (144) = happyGoto action_445
action_444 _ = happyFail

action_445 _ = happyReduce_315

action_446 _ = happyReduce_313

action_447 (146) = happyShift action_79
action_447 (182) = happyShift action_80
action_447 (186) = happyShift action_81
action_447 (191) = happyShift action_82
action_447 (193) = happyShift action_83
action_447 (197) = happyShift action_84
action_447 (198) = happyShift action_85
action_447 (207) = happyShift action_86
action_447 (208) = happyShift action_87
action_447 (214) = happyShift action_88
action_447 (76) = happyGoto action_74
action_447 (140) = happyGoto action_426
action_447 _ = happyReduce_282

action_448 _ = happyReduce_309

action_449 _ = happyReduce_302

action_450 _ = happyReduce_303

action_451 (146) = happyShift action_94
action_451 (182) = happyShift action_95
action_451 (186) = happyShift action_96
action_451 (191) = happyShift action_97
action_451 (193) = happyShift action_98
action_451 (197) = happyShift action_99
action_451 (198) = happyShift action_100
action_451 (207) = happyShift action_101
action_451 (208) = happyShift action_102
action_451 (214) = happyShift action_103
action_451 (75) = happyGoto action_89
action_451 (139) = happyGoto action_425
action_451 _ = happyReduce_279

action_452 _ = happyReduce_296

action_453 _ = happyReduce_289

action_454 _ = happyReduce_290

action_455 (146) = happyShift action_79
action_455 (182) = happyShift action_80
action_455 (186) = happyShift action_81
action_455 (191) = happyShift action_82
action_455 (193) = happyShift action_83
action_455 (197) = happyShift action_84
action_455 (198) = happyShift action_85
action_455 (207) = happyShift action_86
action_455 (208) = happyShift action_87
action_455 (214) = happyShift action_88
action_455 (76) = happyGoto action_74
action_455 (140) = happyGoto action_426
action_455 _ = happyReduce_281

action_456 (146) = happyShift action_94
action_456 (182) = happyShift action_95
action_456 (186) = happyShift action_96
action_456 (191) = happyShift action_97
action_456 (193) = happyShift action_98
action_456 (197) = happyShift action_99
action_456 (198) = happyShift action_100
action_456 (207) = happyShift action_101
action_456 (208) = happyShift action_102
action_456 (214) = happyShift action_103
action_456 (75) = happyGoto action_89
action_456 (139) = happyGoto action_425
action_456 _ = happyReduce_278

action_457 _ = happyReduce_269

action_458 _ = happyReduce_267

action_459 _ = happyReduce_265

action_460 _ = happyReduce_236

action_461 _ = happyReduce_255

action_462 _ = happyReduce_254

action_463 _ = happyReduce_262

action_464 _ = happyReduce_234

action_465 _ = happyReduce_248

action_466 _ = happyReduce_247

action_467 _ = happyReduce_227

action_468 _ = happyReduce_225

action_469 _ = happyReduce_222

action_470 _ = happyReduce_223

action_471 _ = happyReduce_219

action_472 _ = happyReduce_220

action_473 _ = happyReduce_209

action_474 (183) = happyShift action_536
action_474 _ = happyFail

action_475 _ = happyReduce_203

action_476 (183) = happyShift action_535
action_476 _ = happyFail

action_477 _ = happyReduce_198

action_478 _ = happyReduce_196

action_479 (212) = happyShift action_130
action_479 (72) = happyGoto action_534
action_479 _ = happyFail

action_480 (212) = happyShift action_143
action_480 (71) = happyGoto action_533
action_480 _ = happyFail

action_481 _ = happyReduce_149

action_482 _ = happyReduce_147

action_483 _ = happyReduce_144

action_484 _ = happyReduce_142

action_485 _ = happyReduce_137

action_486 _ = happyReduce_135

action_487 (154) = happyShift action_532
action_487 _ = happyFail

action_488 (211) = happyShift action_116
action_488 (70) = happyGoto action_531
action_488 _ = happyFail

action_489 _ = happyReduce_118

action_490 _ = happyReduce_111

action_491 (156) = happyShift action_530
action_491 (213) = happyShift action_70
action_491 (74) = happyGoto action_159
action_491 (118) = happyGoto action_410
action_491 _ = happyFail

action_492 (112) = happyGoto action_529
action_492 _ = happyFail

action_493 (157) = happyShift action_201
action_493 (182) = happyShift action_195
action_493 (213) = happyShift action_70
action_493 (74) = happyGoto action_193
action_493 (104) = happyGoto action_528
action_493 (106) = happyGoto action_200
action_493 _ = happyFail

action_494 _ = happyReduce_128

action_495 (146) = happyShift action_79
action_495 (182) = happyShift action_80
action_495 (186) = happyShift action_81
action_495 (191) = happyShift action_82
action_495 (193) = happyShift action_83
action_495 (197) = happyShift action_84
action_495 (198) = happyShift action_85
action_495 (207) = happyShift action_86
action_495 (208) = happyShift action_87
action_495 (214) = happyShift action_88
action_495 (76) = happyGoto action_74
action_495 (136) = happyGoto action_75
action_495 (138) = happyGoto action_76
action_495 (140) = happyGoto action_77
action_495 (142) = happyGoto action_527
action_495 _ = happyFail

action_496 (158) = happyShift action_270
action_496 (84) = happyGoto action_267
action_496 (88) = happyGoto action_268
action_496 (90) = happyGoto action_526
action_496 _ = happyReduce_86

action_497 (211) = happyShift action_116
action_497 (70) = happyGoto action_525
action_497 _ = happyFail

action_498 (211) = happyShift action_116
action_498 (70) = happyGoto action_524
action_498 _ = happyFail

action_499 _ = happyReduce_113

action_500 (154) = happyShift action_523
action_500 _ = happyFail

action_501 (211) = happyShift action_67
action_501 (69) = happyGoto action_522
action_501 _ = happyFail

action_502 _ = happyReduce_97

action_503 _ = happyReduce_90

action_504 (156) = happyShift action_521
action_504 (213) = happyShift action_73
action_504 (73) = happyGoto action_161
action_504 (117) = happyGoto action_409
action_504 _ = happyFail

action_505 (111) = happyGoto action_520
action_505 _ = happyFail

action_506 (157) = happyShift action_204
action_506 (182) = happyShift action_198
action_506 (213) = happyShift action_73
action_506 (73) = happyGoto action_196
action_506 (103) = happyGoto action_519
action_506 (105) = happyGoto action_203
action_506 _ = happyFail

action_507 _ = happyReduce_107

action_508 (146) = happyShift action_94
action_508 (182) = happyShift action_95
action_508 (186) = happyShift action_96
action_508 (191) = happyShift action_97
action_508 (193) = happyShift action_98
action_508 (197) = happyShift action_99
action_508 (198) = happyShift action_100
action_508 (207) = happyShift action_101
action_508 (208) = happyShift action_102
action_508 (214) = happyShift action_103
action_508 (75) = happyGoto action_89
action_508 (135) = happyGoto action_90
action_508 (137) = happyGoto action_91
action_508 (139) = happyGoto action_92
action_508 (141) = happyGoto action_518
action_508 _ = happyFail

action_509 (158) = happyShift action_274
action_509 (83) = happyGoto action_271
action_509 (87) = happyGoto action_272
action_509 (89) = happyGoto action_517
action_509 _ = happyReduce_84

action_510 (211) = happyShift action_67
action_510 (69) = happyGoto action_516
action_510 _ = happyFail

action_511 (211) = happyShift action_67
action_511 (69) = happyGoto action_515
action_511 _ = happyFail

action_512 _ = happyReduce_92

action_513 _ = happyReduce_83

action_514 _ = happyReduce_80

action_515 _ = happyReduce_96

action_516 _ = happyReduce_95

action_517 _ = happyReduce_98

action_518 _ = happyReduce_93

action_519 (154) = happyShift action_544
action_519 _ = happyFail

action_520 (146) = happyShift action_173
action_520 (182) = happyShift action_174
action_520 (213) = happyShift action_73
action_520 (73) = happyGoto action_171
action_520 (113) = happyGoto action_401
action_520 _ = happyReduce_100

action_521 (146) = happyShift action_141
action_521 (182) = happyShift action_142
action_521 (211) = happyShift action_67
action_521 (212) = happyShift action_143
action_521 (213) = happyShift action_73
action_521 (214) = happyShift action_103
action_521 (215) = happyShift action_144
action_521 (69) = happyGoto action_132
action_521 (71) = happyGoto action_133
action_521 (73) = happyGoto action_134
action_521 (75) = happyGoto action_135
action_521 (77) = happyGoto action_136
action_521 (121) = happyGoto action_543
action_521 (123) = happyGoto action_138
action_521 (125) = happyGoto action_139
action_521 _ = happyFail

action_522 _ = happyReduce_101

action_523 (158) = happyShift action_274
action_523 (83) = happyGoto action_271
action_523 (87) = happyGoto action_542
action_523 _ = happyReduce_84

action_524 _ = happyReduce_117

action_525 _ = happyReduce_116

action_526 _ = happyReduce_119

action_527 _ = happyReduce_114

action_528 (154) = happyShift action_541
action_528 _ = happyFail

action_529 (146) = happyShift action_169
action_529 (182) = happyShift action_170
action_529 (213) = happyShift action_70
action_529 (74) = happyGoto action_167
action_529 (114) = happyGoto action_402
action_529 _ = happyReduce_121

action_530 (146) = happyShift action_128
action_530 (182) = happyShift action_129
action_530 (211) = happyShift action_116
action_530 (212) = happyShift action_130
action_530 (213) = happyShift action_70
action_530 (214) = happyShift action_88
action_530 (215) = happyShift action_131
action_530 (70) = happyGoto action_119
action_530 (72) = happyGoto action_120
action_530 (74) = happyGoto action_121
action_530 (76) = happyGoto action_122
action_530 (78) = happyGoto action_123
action_530 (122) = happyGoto action_540
action_530 (124) = happyGoto action_125
action_530 (126) = happyGoto action_126
action_530 _ = happyFail

action_531 _ = happyReduce_122

action_532 (158) = happyShift action_270
action_532 (84) = happyGoto action_267
action_532 (88) = happyGoto action_539
action_532 _ = happyReduce_86

action_533 _ = happyReduce_172

action_534 _ = happyReduce_181

action_535 (147) = happyShift action_538
action_535 _ = happyFail

action_536 (147) = happyShift action_537
action_536 _ = happyFail

action_537 _ = happyReduce_210

action_538 _ = happyReduce_204

action_539 _ = happyReduce_109

action_540 _ = happyReduce_120

action_541 (84) = happyGoto action_546
action_541 _ = happyReduce_86

action_542 _ = happyReduce_88

action_543 _ = happyReduce_99

action_544 (83) = happyGoto action_545
action_544 _ = happyReduce_84

action_545 (157) = happyShift action_204
action_545 (182) = happyShift action_198
action_545 (211) = happyShift action_67
action_545 (213) = happyShift action_73
action_545 (69) = happyGoto action_264
action_545 (73) = happyGoto action_196
action_545 (91) = happyGoto action_327
action_545 (103) = happyGoto action_266
action_545 (105) = happyGoto action_203
action_545 _ = happyReduce_91

action_546 (157) = happyShift action_201
action_546 (182) = happyShift action_195
action_546 (211) = happyShift action_116
action_546 (213) = happyShift action_70
action_546 (70) = happyGoto action_261
action_546 (74) = happyGoto action_193
action_546 (92) = happyGoto action_328
action_546 (104) = happyGoto action_263
action_546 (106) = happyGoto action_200
action_546 _ = happyReduce_112

happyReduce_66 = happySpecReduce_1  69 happyReduction_66
happyReduction_66 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn69
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  70 happyReduction_67
happyReduction_67 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn70
		 (fromString myLocation happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  71 happyReduction_68
happyReduction_68 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn71
		 ((read happy_var_1) :: Integer
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  72 happyReduction_69
happyReduction_69 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn70
		 (fromLit myLocation (read happy_var_1 :: Integer)
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  73 happyReduction_70
happyReduction_70 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn73
		 (Ident happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  74 happyReduction_71
happyReduction_71 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn70
		 (fromToken myLocation "Ident" happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  75 happyReduction_72
happyReduction_72 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn75
		 ((read happy_var_1) :: Char
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  76 happyReduction_73
happyReduction_73 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn70
		 (fromLit myLocation  (read happy_var_1 :: Char)
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  77 happyReduction_74
happyReduction_74 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn77
		 ((read happy_var_1) :: Double
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  78 happyReduction_75
happyReduction_75 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn70
		 (fromLit myLocation  (read happy_var_1 :: Double)
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  79 happyReduction_76
happyReduction_76 (HappyAbsSyn81  happy_var_1)
	 =  HappyAbsSyn79
		 (Grammar (happy_var_1)
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  80 happyReduction_77
happyReduction_77 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Grammar"  [happy_var_1]
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_0  81 happyReduction_78
happyReduction_78  =  HappyAbsSyn81
		 ([]
	)

happyReduce_79 = happySpecReduce_1  81 happyReduction_79
happyReduction_79 (HappyAbsSyn85  happy_var_1)
	 =  HappyAbsSyn81
		 ((:[]) (happy_var_1)
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  81 happyReduction_80
happyReduction_80 (HappyAbsSyn81  happy_var_3)
	_
	(HappyAbsSyn85  happy_var_1)
	 =  HappyAbsSyn81
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_0  82 happyReduction_81
happyReduction_81  =  HappyAbsSyn70
		 (appEPAll myLocation  "[]" []
	)

happyReduce_82 = happySpecReduce_1  82 happyReduction_82
happyReduction_82 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  82 happyReduction_83
happyReduction_83 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_0  83 happyReduction_84
happyReduction_84  =  HappyAbsSyn83
		 ([]
	)

happyReduce_85 = happySpecReduce_2  83 happyReduction_85
happyReduction_85 (HappyAbsSyn91  happy_var_2)
	(HappyAbsSyn83  happy_var_1)
	 =  HappyAbsSyn83
		 (flip (:) (happy_var_1) (happy_var_2)
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_0  84 happyReduction_86
happyReduction_86  =  HappyAbsSyn70
		 (appEPAll myLocation  "[]" []
	)

happyReduce_87 = happySpecReduce_2  84 happyReduction_87
happyReduction_87 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1,happy_var_2]
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happyReduce 5 85 happyReduction_88
happyReduction_88 ((HappyAbsSyn87  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn103  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn107  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (Rule (happy_var_1) (happy_var_3) (happy_var_5)
	) `HappyStk` happyRest

happyReduce_89 = happySpecReduce_2  85 happyReduction_89
happyReduction_89 (HappyAbsSyn69  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Comment (happy_var_2)
	)
happyReduction_89 _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  85 happyReduction_90
happyReduction_90 (HappyAbsSyn69  happy_var_3)
	(HappyAbsSyn69  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Comments (happy_var_2) (happy_var_3)
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happyReduce 6 85 happyReduction_91
happyReduction_91 ((HappyAbsSyn83  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn103  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn107  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (Internal (happy_var_2) (happy_var_4) (reverse $ happy_var_6)
	) `HappyStk` happyRest

happyReduce_92 = happySpecReduce_3  85 happyReduction_92
happyReduction_92 (HappyAbsSyn135  happy_var_3)
	(HappyAbsSyn73  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Token (happy_var_2) (happy_var_3)
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happyReduce 4 85 happyReduction_93
happyReduction_93 ((HappyAbsSyn135  happy_var_4) `HappyStk`
	(HappyAbsSyn73  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (PosToken (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_94 = happySpecReduce_2  85 happyReduction_94
happyReduction_94 (HappyAbsSyn143  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Entryp (happy_var_2)
	)
happyReduction_94 _ _  = notHappyAtAll 

happyReduce_95 = happyReduce 4 85 happyReduction_95
happyReduction_95 ((HappyAbsSyn69  happy_var_4) `HappyStk`
	(HappyAbsSyn103  happy_var_3) `HappyStk`
	(HappyAbsSyn133  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (Separator (happy_var_2) (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_96 = happyReduce 4 85 happyReduction_96
happyReduction_96 ((HappyAbsSyn69  happy_var_4) `HappyStk`
	(HappyAbsSyn103  happy_var_3) `HappyStk`
	(HappyAbsSyn133  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (Terminator (happy_var_2) (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_97 = happySpecReduce_3  85 happyReduction_97
happyReduction_97 (HappyAbsSyn71  happy_var_3)
	(HappyAbsSyn73  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Coercions (happy_var_2) (happy_var_3)
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happyReduce 4 85 happyReduction_98
happyReduction_98 ((HappyAbsSyn89  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (Rules (happy_var_2) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_99 = happyReduce 5 85 happyReduction_99
happyReduction_99 ((HappyAbsSyn121  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn119  happy_var_3) `HappyStk`
	(HappyAbsSyn73  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (Function (happy_var_2) (reverse $ happy_var_3) (happy_var_5)
	) `HappyStk` happyRest

happyReduce_100 = happyReduce 4 85 happyReduction_100
happyReduction_100 ((HappyAbsSyn111  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (External (happy_var_2) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_101 = happyReduce 4 85 happyReduction_101
happyReduction_101 ((HappyAbsSyn69  happy_var_4) `HappyStk`
	(HappyAbsSyn69  happy_var_3) `HappyStk`
	(HappyAbsSyn69  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn85
		 (AntiQuote (happy_var_2) (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_102 = happySpecReduce_2  85 happyReduction_102
happyReduction_102 (HappyAbsSyn143  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Derive (happy_var_2)
	)
happyReduction_102 _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_2  85 happyReduction_103
happyReduction_103 (HappyAbsSyn99  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Foreground (happy_var_2)
	)
happyReduction_103 _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_2  85 happyReduction_104
happyReduction_104 (HappyAbsSyn99  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Background (happy_var_2)
	)
happyReduction_104 _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_2  85 happyReduction_105
happyReduction_105 (HappyAbsSyn101  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Style (happy_var_2)
	)
happyReduction_105 _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_2  85 happyReduction_106
happyReduction_106 (HappyAbsSyn131  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (Layout (happy_var_2)
	)
happyReduction_106 _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  85 happyReduction_107
happyReduction_107 (HappyAbsSyn131  happy_var_3)
	_
	_
	 =  HappyAbsSyn85
		 (LayoutStop (happy_var_3)
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_2  85 happyReduction_108
happyReduction_108 _
	_
	 =  HappyAbsSyn85
		 (LayoutTop
	)

happyReduce_109 = happyReduce 5 86 happyReduction_109
happyReduction_109 ((HappyAbsSyn70  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn70  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "Rule"  [happy_var_1,happy_var_3,happy_var_5]
	) `HappyStk` happyRest

happyReduce_110 = happySpecReduce_2  86 happyReduction_110
happyReduction_110 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Comment"  [happy_var_2]
	)
happyReduction_110 _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_3  86 happyReduction_111
happyReduction_111 (HappyAbsSyn70  happy_var_3)
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Comments"  [happy_var_2,happy_var_3]
	)
happyReduction_111 _ _ _  = notHappyAtAll 

happyReduce_112 = happyReduce 6 86 happyReduction_112
happyReduction_112 ((HappyAbsSyn70  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn70  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn70  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "Internal"  [happy_var_2,happy_var_4,happy_var_6]
	) `HappyStk` happyRest

happyReduce_113 = happySpecReduce_3  86 happyReduction_113
happyReduction_113 (HappyAbsSyn70  happy_var_3)
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Token"  [happy_var_2,happy_var_3]
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happyReduce 4 86 happyReduction_114
happyReduction_114 ((HappyAbsSyn70  happy_var_4) `HappyStk`
	(HappyAbsSyn70  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "PosToken"  [happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_115 = happySpecReduce_2  86 happyReduction_115
happyReduction_115 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Entryp"  [happy_var_2]
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happyReduce 4 86 happyReduction_116
happyReduction_116 ((HappyAbsSyn70  happy_var_4) `HappyStk`
	(HappyAbsSyn70  happy_var_3) `HappyStk`
	(HappyAbsSyn70  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "Separator"  [happy_var_2,happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_117 = happyReduce 4 86 happyReduction_117
happyReduction_117 ((HappyAbsSyn70  happy_var_4) `HappyStk`
	(HappyAbsSyn70  happy_var_3) `HappyStk`
	(HappyAbsSyn70  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "Terminator"  [happy_var_2,happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_118 = happySpecReduce_3  86 happyReduction_118
happyReduction_118 (HappyAbsSyn70  happy_var_3)
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Coercions"  [happy_var_2,happy_var_3]
	)
happyReduction_118 _ _ _  = notHappyAtAll 

happyReduce_119 = happyReduce 4 86 happyReduction_119
happyReduction_119 ((HappyAbsSyn70  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn70  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "Rules"  [happy_var_2,happy_var_4]
	) `HappyStk` happyRest

happyReduce_120 = happyReduce 5 86 happyReduction_120
happyReduction_120 ((HappyAbsSyn70  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn70  happy_var_3) `HappyStk`
	(HappyAbsSyn70  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "Function"  [happy_var_2,happy_var_3,happy_var_5]
	) `HappyStk` happyRest

happyReduce_121 = happyReduce 4 86 happyReduction_121
happyReduction_121 ((HappyAbsSyn70  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn70  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "External"  [happy_var_2,happy_var_4]
	) `HappyStk` happyRest

happyReduce_122 = happyReduce 4 86 happyReduction_122
happyReduction_122 ((HappyAbsSyn70  happy_var_4) `HappyStk`
	(HappyAbsSyn70  happy_var_3) `HappyStk`
	(HappyAbsSyn70  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "AntiQuote"  [happy_var_2,happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_123 = happySpecReduce_2  86 happyReduction_123
happyReduction_123 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Derive"  [happy_var_2]
	)
happyReduction_123 _ _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_2  86 happyReduction_124
happyReduction_124 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Foreground"  [happy_var_2]
	)
happyReduction_124 _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_2  86 happyReduction_125
happyReduction_125 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Background"  [happy_var_2]
	)
happyReduction_125 _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_2  86 happyReduction_126
happyReduction_126 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Style"  [happy_var_2]
	)
happyReduction_126 _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_2  86 happyReduction_127
happyReduction_127 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Layout"  [happy_var_2]
	)
happyReduction_127 _ _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_3  86 happyReduction_128
happyReduction_128 (HappyAbsSyn70  happy_var_3)
	_
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "LayoutStop"  [happy_var_3]
	)
happyReduction_128 _ _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_2  86 happyReduction_129
happyReduction_129 _
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "LayoutTop" []
	)

happyReduce_130 = happySpecReduce_1  87 happyReduction_130
happyReduction_130 (HappyAbsSyn83  happy_var_1)
	 =  HappyAbsSyn87
		 (RHS (reverse $ happy_var_1)
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_2  87 happyReduction_131
happyReduction_131 (HappyAbsSyn135  happy_var_2)
	_
	 =  HappyAbsSyn87
		 (TRHS (happy_var_2)
	)
happyReduction_131 _ _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_1  88 happyReduction_132
happyReduction_132 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RHS"  [happy_var_1]
	)
happyReduction_132 _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_2  88 happyReduction_133
happyReduction_133 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "TRHS"  [happy_var_2]
	)
happyReduction_133 _ _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_1  89 happyReduction_134
happyReduction_134 (HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn89
		 ((:[]) (happy_var_1)
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_3  89 happyReduction_135
happyReduction_135 (HappyAbsSyn89  happy_var_3)
	_
	(HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn89
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_135 _ _ _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_1  90 happyReduction_136
happyReduction_136 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_136 _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_3  90 happyReduction_137
happyReduction_137 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_137 _ _ _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_2  91 happyReduction_138
happyReduction_138 (HappyAbsSyn93  happy_var_2)
	(HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn91
		 (Terminal (happy_var_1) (happy_var_2)
	)
happyReduction_138 _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_2  91 happyReduction_139
happyReduction_139 (HappyAbsSyn93  happy_var_2)
	(HappyAbsSyn103  happy_var_1)
	 =  HappyAbsSyn91
		 (NTerminal (happy_var_1) (happy_var_2)
	)
happyReduction_139 _ _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_2  92 happyReduction_140
happyReduction_140 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Terminal"  [happy_var_1,happy_var_2]
	)
happyReduction_140 _ _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_2  92 happyReduction_141
happyReduction_141 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "NTerminal"  [happy_var_1,happy_var_2]
	)
happyReduction_141 _ _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_3  93 happyReduction_142
happyReduction_142 _
	(HappyAbsSyn95  happy_var_2)
	_
	 =  HappyAbsSyn93
		 (JAnnotations (happy_var_2)
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_0  93 happyReduction_143
happyReduction_143  =  HappyAbsSyn93
		 (NoAnnotations
	)

happyReduce_144 = happySpecReduce_3  94 happyReduction_144
happyReduction_144 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "JAnnotations"  [happy_var_2]
	)
happyReduction_144 _ _ _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_0  94 happyReduction_145
happyReduction_145  =  HappyAbsSyn70
		 (appEPAll myLocation  "NoAnnotations" []
	)

happyReduce_146 = happySpecReduce_1  95 happyReduction_146
happyReduction_146 (HappyAbsSyn97  happy_var_1)
	 =  HappyAbsSyn95
		 ((:[]) (happy_var_1)
	)
happyReduction_146 _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_3  95 happyReduction_147
happyReduction_147 (HappyAbsSyn95  happy_var_3)
	_
	(HappyAbsSyn97  happy_var_1)
	 =  HappyAbsSyn95
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_147 _ _ _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_1  96 happyReduction_148
happyReduction_148 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_148 _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_3  96 happyReduction_149
happyReduction_149 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_149 _ _ _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_1  97 happyReduction_150
happyReduction_150 _
	 =  HappyAbsSyn97
		 (Annotation_Linebreak
	)

happyReduce_151 = happySpecReduce_1  97 happyReduction_151
happyReduction_151 _
	 =  HappyAbsSyn97
		 (Annotation_Empty
	)

happyReduce_152 = happySpecReduce_2  97 happyReduction_152
happyReduction_152 (HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn97
		 (Annotation_3 (happy_var_2)
	)
happyReduction_152 _ _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_2  97 happyReduction_153
happyReduction_153 (HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn97
		 (Annotation_4 (happy_var_2)
	)
happyReduction_153 _ _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_2  97 happyReduction_154
happyReduction_154 (HappyAbsSyn99  happy_var_2)
	_
	 =  HappyAbsSyn97
		 (Annotation_5 (happy_var_2)
	)
happyReduction_154 _ _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_2  97 happyReduction_155
happyReduction_155 (HappyAbsSyn99  happy_var_2)
	_
	 =  HappyAbsSyn97
		 (Annotation_6 (happy_var_2)
	)
happyReduction_155 _ _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_1  97 happyReduction_156
happyReduction_156 (HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn97
		 (AnnotationStyle (happy_var_1)
	)
happyReduction_156 _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1  98 happyReduction_157
happyReduction_157 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Annotation_Linebreak" []
	)

happyReduce_158 = happySpecReduce_1  98 happyReduction_158
happyReduction_158 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Annotation_Empty" []
	)

happyReduce_159 = happySpecReduce_2  98 happyReduction_159
happyReduction_159 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Annotation_3"  [happy_var_2]
	)
happyReduction_159 _ _  = notHappyAtAll 

happyReduce_160 = happySpecReduce_2  98 happyReduction_160
happyReduction_160 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Annotation_4"  [happy_var_2]
	)
happyReduction_160 _ _  = notHappyAtAll 

happyReduce_161 = happySpecReduce_2  98 happyReduction_161
happyReduction_161 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Annotation_5"  [happy_var_2]
	)
happyReduction_161 _ _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_2  98 happyReduction_162
happyReduction_162 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Annotation_6"  [happy_var_2]
	)
happyReduction_162 _ _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_1  98 happyReduction_163
happyReduction_163 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "AnnotationStyle"  [happy_var_1]
	)
happyReduction_163 _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_1  99 happyReduction_164
happyReduction_164 _
	 =  HappyAbsSyn99
		 (Color_Red
	)

happyReduce_165 = happySpecReduce_1  99 happyReduction_165
happyReduction_165 _
	 =  HappyAbsSyn99
		 (Color_Blue
	)

happyReduce_166 = happySpecReduce_1  99 happyReduction_166
happyReduction_166 _
	 =  HappyAbsSyn99
		 (Color_Green
	)

happyReduce_167 = happySpecReduce_1  99 happyReduction_167
happyReduction_167 _
	 =  HappyAbsSyn99
		 (Color_Yellow
	)

happyReduce_168 = happySpecReduce_1  99 happyReduction_168
happyReduction_168 _
	 =  HappyAbsSyn99
		 (Color_Cyan
	)

happyReduce_169 = happySpecReduce_1  99 happyReduction_169
happyReduction_169 _
	 =  HappyAbsSyn99
		 (Color_Magenta
	)

happyReduce_170 = happySpecReduce_1  99 happyReduction_170
happyReduction_170 _
	 =  HappyAbsSyn99
		 (Color_White
	)

happyReduce_171 = happySpecReduce_1  99 happyReduction_171
happyReduction_171 _
	 =  HappyAbsSyn99
		 (Color_Black
	)

happyReduce_172 = happyReduce 4 99 happyReduction_172
happyReduction_172 ((HappyAbsSyn71  happy_var_4) `HappyStk`
	(HappyAbsSyn71  happy_var_3) `HappyStk`
	(HappyAbsSyn71  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn99
		 (Color_9 (happy_var_2) (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_173 = happySpecReduce_1  100 happyReduction_173
happyReduction_173 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Color_Red" []
	)

happyReduce_174 = happySpecReduce_1  100 happyReduction_174
happyReduction_174 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Color_Blue" []
	)

happyReduce_175 = happySpecReduce_1  100 happyReduction_175
happyReduction_175 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Color_Green" []
	)

happyReduce_176 = happySpecReduce_1  100 happyReduction_176
happyReduction_176 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Color_Yellow" []
	)

happyReduce_177 = happySpecReduce_1  100 happyReduction_177
happyReduction_177 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Color_Cyan" []
	)

happyReduce_178 = happySpecReduce_1  100 happyReduction_178
happyReduction_178 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Color_Magenta" []
	)

happyReduce_179 = happySpecReduce_1  100 happyReduction_179
happyReduction_179 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Color_White" []
	)

happyReduce_180 = happySpecReduce_1  100 happyReduction_180
happyReduction_180 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Color_Black" []
	)

happyReduce_181 = happyReduce 4 100 happyReduction_181
happyReduction_181 ((HappyAbsSyn70  happy_var_4) `HappyStk`
	(HappyAbsSyn70  happy_var_3) `HappyStk`
	(HappyAbsSyn70  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation "Color_9"  [happy_var_2,happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_182 = happySpecReduce_1  101 happyReduction_182
happyReduction_182 _
	 =  HappyAbsSyn101
		 (Style_Bold
	)

happyReduce_183 = happySpecReduce_1  101 happyReduction_183
happyReduction_183 _
	 =  HappyAbsSyn101
		 (Style_Italic
	)

happyReduce_184 = happySpecReduce_1  101 happyReduction_184
happyReduction_184 _
	 =  HappyAbsSyn101
		 (Style_Underline
	)

happyReduce_185 = happySpecReduce_1  101 happyReduction_185
happyReduction_185 _
	 =  HappyAbsSyn101
		 (Style_Invert
	)

happyReduce_186 = happySpecReduce_1  101 happyReduction_186
happyReduction_186 _
	 =  HappyAbsSyn101
		 (Style_Blink
	)

happyReduce_187 = happySpecReduce_1  102 happyReduction_187
happyReduction_187 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Style_Bold" []
	)

happyReduce_188 = happySpecReduce_1  102 happyReduction_188
happyReduction_188 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Style_Italic" []
	)

happyReduce_189 = happySpecReduce_1  102 happyReduction_189
happyReduction_189 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Style_Underline" []
	)

happyReduce_190 = happySpecReduce_1  102 happyReduction_190
happyReduction_190 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Style_Invert" []
	)

happyReduce_191 = happySpecReduce_1  102 happyReduction_191
happyReduction_191 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Style_Blink" []
	)

happyReduce_192 = happySpecReduce_2  103 happyReduction_192
happyReduction_192 (HappyAbsSyn103  happy_var_2)
	_
	 =  HappyAbsSyn103
		 (OptCat (happy_var_2)
	)
happyReduction_192 _ _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_1  103 happyReduction_193
happyReduction_193 (HappyAbsSyn103  happy_var_1)
	 =  HappyAbsSyn103
		 (happy_var_1
	)
happyReduction_193 _  = notHappyAtAll 

happyReduce_194 = happySpecReduce_2  104 happyReduction_194
happyReduction_194 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "OptCat"  [happy_var_2]
	)
happyReduction_194 _ _  = notHappyAtAll 

happyReduce_195 = happySpecReduce_1  104 happyReduction_195
happyReduction_195 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (happy_var_1
	)
happyReduction_195 _  = notHappyAtAll 

happyReduce_196 = happySpecReduce_3  105 happyReduction_196
happyReduction_196 _
	(HappyAbsSyn103  happy_var_2)
	_
	 =  HappyAbsSyn103
		 (ListCat (happy_var_2)
	)
happyReduction_196 _ _ _  = notHappyAtAll 

happyReduce_197 = happySpecReduce_1  105 happyReduction_197
happyReduction_197 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn103
		 (IdCat (happy_var_1)
	)
happyReduction_197 _  = notHappyAtAll 

happyReduce_198 = happySpecReduce_3  106 happyReduction_198
happyReduction_198 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "ListCat"  [happy_var_2]
	)
happyReduction_198 _ _ _  = notHappyAtAll 

happyReduce_199 = happySpecReduce_1  106 happyReduction_199
happyReduction_199 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "IdCat"  [happy_var_1]
	)
happyReduction_199 _  = notHappyAtAll 

happyReduce_200 = happySpecReduce_1  107 happyReduction_200
happyReduction_200 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn107
		 (Id (happy_var_1)
	)
happyReduction_200 _  = notHappyAtAll 

happyReduce_201 = happySpecReduce_1  107 happyReduction_201
happyReduction_201 _
	 =  HappyAbsSyn107
		 (Wild
	)

happyReduce_202 = happySpecReduce_2  107 happyReduction_202
happyReduction_202 _
	_
	 =  HappyAbsSyn107
		 (ListE
	)

happyReduce_203 = happySpecReduce_3  107 happyReduction_203
happyReduction_203 _
	_
	_
	 =  HappyAbsSyn107
		 (ListCons
	)

happyReduce_204 = happyReduce 5 107 happyReduction_204
happyReduction_204 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn107
		 (ListOne
	) `HappyStk` happyRest

happyReduce_205 = happySpecReduce_2  107 happyReduction_205
happyReduction_205 (HappyAbsSyn109  happy_var_2)
	_
	 =  HappyAbsSyn107
		 (Aq (happy_var_2)
	)
happyReduction_205 _ _  = notHappyAtAll 

happyReduce_206 = happySpecReduce_1  108 happyReduction_206
happyReduction_206 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Id"  [happy_var_1]
	)
happyReduction_206 _  = notHappyAtAll 

happyReduce_207 = happySpecReduce_1  108 happyReduction_207
happyReduction_207 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "Wild" []
	)

happyReduce_208 = happySpecReduce_2  108 happyReduction_208
happyReduction_208 _
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "ListE" []
	)

happyReduce_209 = happySpecReduce_3  108 happyReduction_209
happyReduction_209 _
	_
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "ListCons" []
	)

happyReduce_210 = happyReduce 5 108 happyReduction_210
happyReduction_210 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (appEPAll myLocation  "ListOne" []
	) `HappyStk` happyRest

happyReduce_211 = happySpecReduce_2  108 happyReduction_211
happyReduction_211 (HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Aq"  [happy_var_2]
	)
happyReduction_211 _ _  = notHappyAtAll 

happyReduce_212 = happySpecReduce_1  109 happyReduction_212
happyReduction_212 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn109
		 (JIdent (happy_var_1)
	)
happyReduction_212 _  = notHappyAtAll 

happyReduce_213 = happySpecReduce_0  109 happyReduction_213
happyReduction_213  =  HappyAbsSyn109
		 (NIdent
	)

happyReduce_214 = happySpecReduce_1  110 happyReduction_214
happyReduction_214 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "JIdent"  [happy_var_1]
	)
happyReduction_214 _  = notHappyAtAll 

happyReduce_215 = happySpecReduce_0  110 happyReduction_215
happyReduction_215  =  HappyAbsSyn70
		 (appEPAll myLocation  "NIdent" []
	)

happyReduce_216 = happySpecReduce_2  111 happyReduction_216
happyReduction_216 (HappyAbsSyn111  happy_var_2)
	(HappyAbsSyn111  happy_var_1)
	 =  HappyAbsSyn111
		 (HsApp (happy_var_1) (happy_var_2)
	)
happyReduction_216 _ _  = notHappyAtAll 

happyReduce_217 = happySpecReduce_2  112 happyReduction_217
happyReduction_217 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "HsApp"  [happy_var_1,happy_var_2]
	)
happyReduction_217 _ _  = notHappyAtAll 

happyReduce_218 = happySpecReduce_1  113 happyReduction_218
happyReduction_218 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn111
		 (HsCon (happy_var_1)
	)
happyReduction_218 _  = notHappyAtAll 

happyReduce_219 = happySpecReduce_3  113 happyReduction_219
happyReduction_219 _
	(HappyAbsSyn115  happy_var_2)
	_
	 =  HappyAbsSyn111
		 (HsTup (happy_var_2)
	)
happyReduction_219 _ _ _  = notHappyAtAll 

happyReduce_220 = happySpecReduce_3  113 happyReduction_220
happyReduction_220 _
	(HappyAbsSyn111  happy_var_2)
	_
	 =  HappyAbsSyn111
		 (HsList (happy_var_2)
	)
happyReduction_220 _ _ _  = notHappyAtAll 

happyReduce_221 = happySpecReduce_1  114 happyReduction_221
happyReduction_221 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "HsCon"  [happy_var_1]
	)
happyReduction_221 _  = notHappyAtAll 

happyReduce_222 = happySpecReduce_3  114 happyReduction_222
happyReduction_222 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "HsTup"  [happy_var_2]
	)
happyReduction_222 _ _ _  = notHappyAtAll 

happyReduce_223 = happySpecReduce_3  114 happyReduction_223
happyReduction_223 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "HsList"  [happy_var_2]
	)
happyReduction_223 _ _ _  = notHappyAtAll 

happyReduce_224 = happySpecReduce_1  115 happyReduction_224
happyReduction_224 (HappyAbsSyn111  happy_var_1)
	 =  HappyAbsSyn115
		 ((:[]) (happy_var_1)
	)
happyReduction_224 _  = notHappyAtAll 

happyReduce_225 = happySpecReduce_3  115 happyReduction_225
happyReduction_225 (HappyAbsSyn115  happy_var_3)
	_
	(HappyAbsSyn111  happy_var_1)
	 =  HappyAbsSyn115
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_225 _ _ _  = notHappyAtAll 

happyReduce_226 = happySpecReduce_1  116 happyReduction_226
happyReduction_226 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_226 _  = notHappyAtAll 

happyReduce_227 = happySpecReduce_3  116 happyReduction_227
happyReduction_227 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_227 _ _ _  = notHappyAtAll 

happyReduce_228 = happySpecReduce_1  117 happyReduction_228
happyReduction_228 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn117
		 (Arg (happy_var_1)
	)
happyReduction_228 _  = notHappyAtAll 

happyReduce_229 = happySpecReduce_1  118 happyReduction_229
happyReduction_229 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Arg"  [happy_var_1]
	)
happyReduction_229 _  = notHappyAtAll 

happyReduce_230 = happySpecReduce_0  119 happyReduction_230
happyReduction_230  =  HappyAbsSyn119
		 ([]
	)

happyReduce_231 = happySpecReduce_2  119 happyReduction_231
happyReduction_231 (HappyAbsSyn117  happy_var_2)
	(HappyAbsSyn119  happy_var_1)
	 =  HappyAbsSyn119
		 (flip (:) (happy_var_1) (happy_var_2)
	)
happyReduction_231 _ _  = notHappyAtAll 

happyReduce_232 = happySpecReduce_0  120 happyReduction_232
happyReduction_232  =  HappyAbsSyn70
		 (appEPAll myLocation  "[]" []
	)

happyReduce_233 = happySpecReduce_2  120 happyReduction_233
happyReduction_233 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1,happy_var_2]
	)
happyReduction_233 _ _  = notHappyAtAll 

happyReduce_234 = happySpecReduce_3  121 happyReduction_234
happyReduction_234 (HappyAbsSyn121  happy_var_3)
	_
	(HappyAbsSyn121  happy_var_1)
	 =  HappyAbsSyn121
		 (Cons (happy_var_1) (happy_var_3)
	)
happyReduction_234 _ _ _  = notHappyAtAll 

happyReduce_235 = happySpecReduce_1  121 happyReduction_235
happyReduction_235 (HappyAbsSyn121  happy_var_1)
	 =  HappyAbsSyn121
		 (happy_var_1
	)
happyReduction_235 _  = notHappyAtAll 

happyReduce_236 = happySpecReduce_3  122 happyReduction_236
happyReduction_236 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Cons"  [happy_var_1,happy_var_3]
	)
happyReduction_236 _ _ _  = notHappyAtAll 

happyReduce_237 = happySpecReduce_1  122 happyReduction_237
happyReduction_237 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (happy_var_1
	)
happyReduction_237 _  = notHappyAtAll 

happyReduce_238 = happySpecReduce_2  123 happyReduction_238
happyReduction_238 (HappyAbsSyn127  happy_var_2)
	(HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn121
		 (App (happy_var_1) (happy_var_2)
	)
happyReduction_238 _ _  = notHappyAtAll 

happyReduce_239 = happySpecReduce_1  123 happyReduction_239
happyReduction_239 (HappyAbsSyn121  happy_var_1)
	 =  HappyAbsSyn121
		 (happy_var_1
	)
happyReduction_239 _  = notHappyAtAll 

happyReduce_240 = happySpecReduce_2  124 happyReduction_240
happyReduction_240 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "App"  [happy_var_1,happy_var_2]
	)
happyReduction_240 _ _  = notHappyAtAll 

happyReduce_241 = happySpecReduce_1  124 happyReduction_241
happyReduction_241 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (happy_var_1
	)
happyReduction_241 _  = notHappyAtAll 

happyReduce_242 = happySpecReduce_1  125 happyReduction_242
happyReduction_242 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn121
		 (Var (happy_var_1)
	)
happyReduction_242 _  = notHappyAtAll 

happyReduce_243 = happySpecReduce_1  125 happyReduction_243
happyReduction_243 (HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn121
		 (LitInt (happy_var_1)
	)
happyReduction_243 _  = notHappyAtAll 

happyReduce_244 = happySpecReduce_1  125 happyReduction_244
happyReduction_244 (HappyAbsSyn75  happy_var_1)
	 =  HappyAbsSyn121
		 (LitChar (happy_var_1)
	)
happyReduction_244 _  = notHappyAtAll 

happyReduce_245 = happySpecReduce_1  125 happyReduction_245
happyReduction_245 (HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn121
		 (LitString (happy_var_1)
	)
happyReduction_245 _  = notHappyAtAll 

happyReduce_246 = happySpecReduce_1  125 happyReduction_246
happyReduction_246 (HappyAbsSyn77  happy_var_1)
	 =  HappyAbsSyn121
		 (LitDouble (happy_var_1)
	)
happyReduction_246 _  = notHappyAtAll 

happyReduce_247 = happySpecReduce_3  125 happyReduction_247
happyReduction_247 _
	(HappyAbsSyn127  happy_var_2)
	_
	 =  HappyAbsSyn121
		 (List (happy_var_2)
	)
happyReduction_247 _ _ _  = notHappyAtAll 

happyReduce_248 = happySpecReduce_3  125 happyReduction_248
happyReduction_248 _
	(HappyAbsSyn121  happy_var_2)
	_
	 =  HappyAbsSyn121
		 (happy_var_2
	)
happyReduction_248 _ _ _  = notHappyAtAll 

happyReduce_249 = happySpecReduce_1  126 happyReduction_249
happyReduction_249 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "Var"  [happy_var_1]
	)
happyReduction_249 _  = notHappyAtAll 

happyReduce_250 = happySpecReduce_1  126 happyReduction_250
happyReduction_250 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "LitInt"  [happy_var_1]
	)
happyReduction_250 _  = notHappyAtAll 

happyReduce_251 = happySpecReduce_1  126 happyReduction_251
happyReduction_251 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "LitChar"  [happy_var_1]
	)
happyReduction_251 _  = notHappyAtAll 

happyReduce_252 = happySpecReduce_1  126 happyReduction_252
happyReduction_252 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "LitString"  [happy_var_1]
	)
happyReduction_252 _  = notHappyAtAll 

happyReduce_253 = happySpecReduce_1  126 happyReduction_253
happyReduction_253 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "LitDouble"  [happy_var_1]
	)
happyReduction_253 _  = notHappyAtAll 

happyReduce_254 = happySpecReduce_3  126 happyReduction_254
happyReduction_254 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "List"  [happy_var_2]
	)
happyReduction_254 _ _ _  = notHappyAtAll 

happyReduce_255 = happySpecReduce_3  126 happyReduction_255
happyReduction_255 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (happy_var_2
	)
happyReduction_255 _ _ _  = notHappyAtAll 

happyReduce_256 = happySpecReduce_1  127 happyReduction_256
happyReduction_256 (HappyAbsSyn121  happy_var_1)
	 =  HappyAbsSyn127
		 ((:[]) (happy_var_1)
	)
happyReduction_256 _  = notHappyAtAll 

happyReduce_257 = happySpecReduce_2  127 happyReduction_257
happyReduction_257 (HappyAbsSyn127  happy_var_2)
	(HappyAbsSyn121  happy_var_1)
	 =  HappyAbsSyn127
		 ((:) (happy_var_1) (happy_var_2)
	)
happyReduction_257 _ _  = notHappyAtAll 

happyReduce_258 = happySpecReduce_1  128 happyReduction_258
happyReduction_258 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_258 _  = notHappyAtAll 

happyReduce_259 = happySpecReduce_2  128 happyReduction_259
happyReduction_259 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_2]
	)
happyReduction_259 _ _  = notHappyAtAll 

happyReduce_260 = happySpecReduce_0  129 happyReduction_260
happyReduction_260  =  HappyAbsSyn127
		 ([]
	)

happyReduce_261 = happySpecReduce_1  129 happyReduction_261
happyReduction_261 (HappyAbsSyn121  happy_var_1)
	 =  HappyAbsSyn127
		 ((:[]) (happy_var_1)
	)
happyReduction_261 _  = notHappyAtAll 

happyReduce_262 = happySpecReduce_3  129 happyReduction_262
happyReduction_262 (HappyAbsSyn127  happy_var_3)
	_
	(HappyAbsSyn121  happy_var_1)
	 =  HappyAbsSyn127
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_262 _ _ _  = notHappyAtAll 

happyReduce_263 = happySpecReduce_0  130 happyReduction_263
happyReduction_263  =  HappyAbsSyn70
		 (appEPAll myLocation  "[]" []
	)

happyReduce_264 = happySpecReduce_1  130 happyReduction_264
happyReduction_264 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_264 _  = notHappyAtAll 

happyReduce_265 = happySpecReduce_3  130 happyReduction_265
happyReduction_265 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_265 _ _ _  = notHappyAtAll 

happyReduce_266 = happySpecReduce_1  131 happyReduction_266
happyReduction_266 (HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn131
		 ((:[]) (happy_var_1)
	)
happyReduction_266 _  = notHappyAtAll 

happyReduce_267 = happySpecReduce_3  131 happyReduction_267
happyReduction_267 (HappyAbsSyn131  happy_var_3)
	_
	(HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn131
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_267 _ _ _  = notHappyAtAll 

happyReduce_268 = happySpecReduce_1  132 happyReduction_268
happyReduction_268 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_268 _  = notHappyAtAll 

happyReduce_269 = happySpecReduce_3  132 happyReduction_269
happyReduction_269 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_269 _ _ _  = notHappyAtAll 

happyReduce_270 = happySpecReduce_1  133 happyReduction_270
happyReduction_270 _
	 =  HappyAbsSyn133
		 (MNonempty
	)

happyReduce_271 = happySpecReduce_0  133 happyReduction_271
happyReduction_271  =  HappyAbsSyn133
		 (MEmpty
	)

happyReduce_272 = happySpecReduce_1  134 happyReduction_272
happyReduction_272 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "MNonempty" []
	)

happyReduce_273 = happySpecReduce_0  134 happyReduction_273
happyReduction_273  =  HappyAbsSyn70
		 (appEPAll myLocation  "MEmpty" []
	)

happyReduce_274 = happySpecReduce_2  135 happyReduction_274
happyReduction_274 (HappyAbsSyn135  happy_var_2)
	(HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (RSeq (happy_var_1) (happy_var_2)
	)
happyReduction_274 _ _  = notHappyAtAll 

happyReduce_275 = happySpecReduce_1  135 happyReduction_275
happyReduction_275 (HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (happy_var_1
	)
happyReduction_275 _  = notHappyAtAll 

happyReduce_276 = happySpecReduce_2  136 happyReduction_276
happyReduction_276 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RSeq"  [happy_var_1,happy_var_2]
	)
happyReduction_276 _ _  = notHappyAtAll 

happyReduce_277 = happySpecReduce_1  136 happyReduction_277
happyReduction_277 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (happy_var_1
	)
happyReduction_277 _  = notHappyAtAll 

happyReduce_278 = happySpecReduce_3  137 happyReduction_278
happyReduction_278 (HappyAbsSyn135  happy_var_3)
	_
	(HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (RAlt (happy_var_1) (happy_var_3)
	)
happyReduction_278 _ _ _  = notHappyAtAll 

happyReduce_279 = happySpecReduce_3  137 happyReduction_279
happyReduction_279 (HappyAbsSyn135  happy_var_3)
	_
	(HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (RMinus (happy_var_1) (happy_var_3)
	)
happyReduction_279 _ _ _  = notHappyAtAll 

happyReduce_280 = happySpecReduce_1  137 happyReduction_280
happyReduction_280 (HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (happy_var_1
	)
happyReduction_280 _  = notHappyAtAll 

happyReduce_281 = happySpecReduce_3  138 happyReduction_281
happyReduction_281 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RAlt"  [happy_var_1,happy_var_3]
	)
happyReduction_281 _ _ _  = notHappyAtAll 

happyReduce_282 = happySpecReduce_3  138 happyReduction_282
happyReduction_282 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RMinus"  [happy_var_1,happy_var_3]
	)
happyReduction_282 _ _ _  = notHappyAtAll 

happyReduce_283 = happySpecReduce_1  138 happyReduction_283
happyReduction_283 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (happy_var_1
	)
happyReduction_283 _  = notHappyAtAll 

happyReduce_284 = happySpecReduce_2  139 happyReduction_284
happyReduction_284 _
	(HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (RStar (happy_var_1)
	)
happyReduction_284 _ _  = notHappyAtAll 

happyReduce_285 = happySpecReduce_2  139 happyReduction_285
happyReduction_285 _
	(HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (RPlus (happy_var_1)
	)
happyReduction_285 _ _  = notHappyAtAll 

happyReduce_286 = happySpecReduce_2  139 happyReduction_286
happyReduction_286 _
	(HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (ROpt (happy_var_1)
	)
happyReduction_286 _ _  = notHappyAtAll 

happyReduce_287 = happySpecReduce_1  139 happyReduction_287
happyReduction_287 _
	 =  HappyAbsSyn135
		 (REps
	)

happyReduce_288 = happySpecReduce_1  139 happyReduction_288
happyReduction_288 (HappyAbsSyn75  happy_var_1)
	 =  HappyAbsSyn135
		 (RChar (happy_var_1)
	)
happyReduction_288 _  = notHappyAtAll 

happyReduce_289 = happySpecReduce_3  139 happyReduction_289
happyReduction_289 _
	(HappyAbsSyn69  happy_var_2)
	_
	 =  HappyAbsSyn135
		 (RAlts (happy_var_2)
	)
happyReduction_289 _ _ _  = notHappyAtAll 

happyReduce_290 = happySpecReduce_3  139 happyReduction_290
happyReduction_290 _
	(HappyAbsSyn69  happy_var_2)
	_
	 =  HappyAbsSyn135
		 (RSeqs (happy_var_2)
	)
happyReduction_290 _ _ _  = notHappyAtAll 

happyReduce_291 = happySpecReduce_1  139 happyReduction_291
happyReduction_291 _
	 =  HappyAbsSyn135
		 (RDigit
	)

happyReduce_292 = happySpecReduce_1  139 happyReduction_292
happyReduction_292 _
	 =  HappyAbsSyn135
		 (RLetter
	)

happyReduce_293 = happySpecReduce_1  139 happyReduction_293
happyReduction_293 _
	 =  HappyAbsSyn135
		 (RUpper
	)

happyReduce_294 = happySpecReduce_1  139 happyReduction_294
happyReduction_294 _
	 =  HappyAbsSyn135
		 (RLower
	)

happyReduce_295 = happySpecReduce_1  139 happyReduction_295
happyReduction_295 _
	 =  HappyAbsSyn135
		 (RAny
	)

happyReduce_296 = happySpecReduce_3  139 happyReduction_296
happyReduction_296 _
	(HappyAbsSyn135  happy_var_2)
	_
	 =  HappyAbsSyn135
		 (happy_var_2
	)
happyReduction_296 _ _ _  = notHappyAtAll 

happyReduce_297 = happySpecReduce_2  140 happyReduction_297
happyReduction_297 _
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RStar"  [happy_var_1]
	)
happyReduction_297 _ _  = notHappyAtAll 

happyReduce_298 = happySpecReduce_2  140 happyReduction_298
happyReduction_298 _
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RPlus"  [happy_var_1]
	)
happyReduction_298 _ _  = notHappyAtAll 

happyReduce_299 = happySpecReduce_2  140 happyReduction_299
happyReduction_299 _
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "ROpt"  [happy_var_1]
	)
happyReduction_299 _ _  = notHappyAtAll 

happyReduce_300 = happySpecReduce_1  140 happyReduction_300
happyReduction_300 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "REps" []
	)

happyReduce_301 = happySpecReduce_1  140 happyReduction_301
happyReduction_301 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RChar"  [happy_var_1]
	)
happyReduction_301 _  = notHappyAtAll 

happyReduce_302 = happySpecReduce_3  140 happyReduction_302
happyReduction_302 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RAlts"  [happy_var_2]
	)
happyReduction_302 _ _ _  = notHappyAtAll 

happyReduce_303 = happySpecReduce_3  140 happyReduction_303
happyReduction_303 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (appEPAll myLocation "RSeqs"  [happy_var_2]
	)
happyReduction_303 _ _ _  = notHappyAtAll 

happyReduce_304 = happySpecReduce_1  140 happyReduction_304
happyReduction_304 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "RDigit" []
	)

happyReduce_305 = happySpecReduce_1  140 happyReduction_305
happyReduction_305 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "RLetter" []
	)

happyReduce_306 = happySpecReduce_1  140 happyReduction_306
happyReduction_306 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "RUpper" []
	)

happyReduce_307 = happySpecReduce_1  140 happyReduction_307
happyReduction_307 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "RLower" []
	)

happyReduce_308 = happySpecReduce_1  140 happyReduction_308
happyReduction_308 _
	 =  HappyAbsSyn70
		 (appEPAll myLocation  "RAny" []
	)

happyReduce_309 = happySpecReduce_3  140 happyReduction_309
happyReduction_309 _
	(HappyAbsSyn70  happy_var_2)
	_
	 =  HappyAbsSyn70
		 (happy_var_2
	)
happyReduction_309 _ _ _  = notHappyAtAll 

happyReduce_310 = happySpecReduce_1  141 happyReduction_310
happyReduction_310 (HappyAbsSyn135  happy_var_1)
	 =  HappyAbsSyn135
		 (happy_var_1
	)
happyReduction_310 _  = notHappyAtAll 

happyReduce_311 = happySpecReduce_1  142 happyReduction_311
happyReduction_311 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (happy_var_1
	)
happyReduction_311 _  = notHappyAtAll 

happyReduce_312 = happySpecReduce_1  143 happyReduction_312
happyReduction_312 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn143
		 ((:[]) (happy_var_1)
	)
happyReduction_312 _  = notHappyAtAll 

happyReduce_313 = happySpecReduce_3  143 happyReduction_313
happyReduction_313 (HappyAbsSyn143  happy_var_3)
	_
	(HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn143
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_313 _ _ _  = notHappyAtAll 

happyReduce_314 = happySpecReduce_1  144 happyReduction_314
happyReduction_314 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_314 _  = notHappyAtAll 

happyReduce_315 = happySpecReduce_3  144 happyReduction_315
happyReduction_315 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn70
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_315 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 217 217 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 145;
	PT _ (TS _ 2) -> cont 146;
	PT _ (TS _ 3) -> cont 147;
	PT _ (TS _ 4) -> cont 148;
	PT _ (TS _ 5) -> cont 149;
	PT _ (TS _ 6) -> cont 150;
	PT _ (TS _ 7) -> cont 151;
	PT _ (TS _ 8) -> cont 152;
	PT _ (TS _ 9) -> cont 153;
	PT _ (TS _ 10) -> cont 154;
	PT _ (TS _ 11) -> cont 155;
	PT _ (TS _ 12) -> cont 156;
	PT _ (TS _ 13) -> cont 157;
	PT _ (TS _ 14) -> cont 158;
	PT _ (TS _ 15) -> cont 159;
	PT _ (TS _ 16) -> cont 160;
	PT _ (TS _ 17) -> cont 161;
	PT _ (TS _ 18) -> cont 162;
	PT _ (TS _ 19) -> cont 163;
	PT _ (TS _ 20) -> cont 164;
	PT _ (TS _ 21) -> cont 165;
	PT _ (TS _ 22) -> cont 166;
	PT _ (TS _ 23) -> cont 167;
	PT _ (TS _ 24) -> cont 168;
	PT _ (TS _ 25) -> cont 169;
	PT _ (TS _ 26) -> cont 170;
	PT _ (TS _ 27) -> cont 171;
	PT _ (TS _ 28) -> cont 172;
	PT _ (TS _ 29) -> cont 173;
	PT _ (TS _ 30) -> cont 174;
	PT _ (TS _ 31) -> cont 175;
	PT _ (TS _ 32) -> cont 176;
	PT _ (TS _ 33) -> cont 177;
	PT _ (TS _ 34) -> cont 178;
	PT _ (TS _ 35) -> cont 179;
	PT _ (TS _ 36) -> cont 180;
	PT _ (TS _ 37) -> cont 181;
	PT _ (TS _ 38) -> cont 182;
	PT _ (TS _ 39) -> cont 183;
	PT _ (TS _ 40) -> cont 184;
	PT _ (TS _ 41) -> cont 185;
	PT _ (TS _ 42) -> cont 186;
	PT _ (TS _ 43) -> cont 187;
	PT _ (TS _ 44) -> cont 188;
	PT _ (TS _ 45) -> cont 189;
	PT _ (TS _ 46) -> cont 190;
	PT _ (TS _ 47) -> cont 191;
	PT _ (TS _ 48) -> cont 192;
	PT _ (TS _ 49) -> cont 193;
	PT _ (TS _ 50) -> cont 194;
	PT _ (TS _ 51) -> cont 195;
	PT _ (TS _ 52) -> cont 196;
	PT _ (TS _ 53) -> cont 197;
	PT _ (TS _ 54) -> cont 198;
	PT _ (TS _ 55) -> cont 199;
	PT _ (TS _ 56) -> cont 200;
	PT _ (TS _ 57) -> cont 201;
	PT _ (TS _ 58) -> cont 202;
	PT _ (TS _ 59) -> cont 203;
	PT _ (TS _ 60) -> cont 204;
	PT _ (TS _ 61) -> cont 205;
	PT _ (TS _ 62) -> cont 206;
	PT _ (TS _ 63) -> cont 207;
	PT _ (TS _ 64) -> cont 208;
	PT _ (TS _ 65) -> cont 209;
	PT _ (TS _ 66) -> cont 210;
	PT _ (TL happy_dollar_dollar) -> cont 211;
	PT _ (TI happy_dollar_dollar) -> cont 212;
	PT _ (TV happy_dollar_dollar) -> cont 213;
	PT _ (TC happy_dollar_dollar) -> cont 214;
	PT _ (TD happy_dollar_dollar) -> cont 215;
	_ -> cont 216;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => ParseMonad a -> (a -> ParseMonad b) -> ParseMonad b
happyThen = (>>=)
happyReturn :: () => a -> ParseMonad a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> ParseMonad a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> ParseMonad a
happyError' = happyError

pGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn79 z -> happyReturn z; _other -> notHappyAtAll })

qGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn81 z -> happyReturn z; _other -> notHappyAtAll })

qListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn83 z -> happyReturn z; _other -> notHappyAtAll })

qListItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn85 z -> happyReturn z; _other -> notHappyAtAll })

qDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn87 z -> happyReturn z; _other -> notHappyAtAll })

qRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn89 z -> happyReturn z; _other -> notHappyAtAll })

qListRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn91 z -> happyReturn z; _other -> notHappyAtAll })

qItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pMAnnotations tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_14 tks) (\x -> case x of {HappyAbsSyn93 z -> happyReturn z; _other -> notHappyAtAll })

qMAnnotations tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_15 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListAnnotation tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_16 tks) (\x -> case x of {HappyAbsSyn95 z -> happyReturn z; _other -> notHappyAtAll })

qListAnnotation tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_17 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pAnnotation tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_18 tks) (\x -> case x of {HappyAbsSyn97 z -> happyReturn z; _other -> notHappyAtAll })

qAnnotation tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_19 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pColor tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_20 tks) (\x -> case x of {HappyAbsSyn99 z -> happyReturn z; _other -> notHappyAtAll })

qColor tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_21 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pStyle tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_22 tks) (\x -> case x of {HappyAbsSyn101 z -> happyReturn z; _other -> notHappyAtAll })

qStyle tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_23 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pCat tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_24 tks) (\x -> case x of {HappyAbsSyn103 z -> happyReturn z; _other -> notHappyAtAll })

qCat tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_25 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pCat1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_26 tks) (\x -> case x of {HappyAbsSyn103 z -> happyReturn z; _other -> notHappyAtAll })

qCat1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_27 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pLabel tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_28 tks) (\x -> case x of {HappyAbsSyn107 z -> happyReturn z; _other -> notHappyAtAll })

qLabel tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_29 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pMIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_30 tks) (\x -> case x of {HappyAbsSyn109 z -> happyReturn z; _other -> notHappyAtAll })

qMIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_31 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pHsTyp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_32 tks) (\x -> case x of {HappyAbsSyn111 z -> happyReturn z; _other -> notHappyAtAll })

qHsTyp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_33 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pHsTyp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_34 tks) (\x -> case x of {HappyAbsSyn111 z -> happyReturn z; _other -> notHappyAtAll })

qHsTyp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_35 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListHsTyp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_36 tks) (\x -> case x of {HappyAbsSyn115 z -> happyReturn z; _other -> notHappyAtAll })

qListHsTyp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_37 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_38 tks) (\x -> case x of {HappyAbsSyn117 z -> happyReturn z; _other -> notHappyAtAll })

qArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_39 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_40 tks) (\x -> case x of {HappyAbsSyn119 z -> happyReturn z; _other -> notHappyAtAll })

qListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_41 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_42 tks) (\x -> case x of {HappyAbsSyn121 z -> happyReturn z; _other -> notHappyAtAll })

qExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_43 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_44 tks) (\x -> case x of {HappyAbsSyn121 z -> happyReturn z; _other -> notHappyAtAll })

qExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_45 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_46 tks) (\x -> case x of {HappyAbsSyn121 z -> happyReturn z; _other -> notHappyAtAll })

qExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_47 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_48 tks) (\x -> case x of {HappyAbsSyn127 z -> happyReturn z; _other -> notHappyAtAll })

qListExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_49 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_50 tks) (\x -> case x of {HappyAbsSyn127 z -> happyReturn z; _other -> notHappyAtAll })

qListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_51 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListString tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_52 tks) (\x -> case x of {HappyAbsSyn131 z -> happyReturn z; _other -> notHappyAtAll })

qListString tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_53 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pMinimumSize tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_54 tks) (\x -> case x of {HappyAbsSyn133 z -> happyReturn z; _other -> notHappyAtAll })

qMinimumSize tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_55 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pReg2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_56 tks) (\x -> case x of {HappyAbsSyn135 z -> happyReturn z; _other -> notHappyAtAll })

qReg2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_57 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pReg1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_58 tks) (\x -> case x of {HappyAbsSyn135 z -> happyReturn z; _other -> notHappyAtAll })

qReg1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_59 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pReg3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_60 tks) (\x -> case x of {HappyAbsSyn135 z -> happyReturn z; _other -> notHappyAtAll })

qReg3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_61 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pReg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_62 tks) (\x -> case x of {HappyAbsSyn135 z -> happyReturn z; _other -> notHappyAtAll })

qReg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_63 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_64 tks) (\x -> case x of {HappyAbsSyn143 z -> happyReturn z; _other -> notHappyAtAll })

qListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_65 tks) (\x -> case x of {HappyAbsSyn70 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> ParseMonad a
happyError ts =
  fail $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens

myLocation = ($(fmap loc_package location >>= lift),"Language.LBNF.Grammar")

{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates\\GenericTemplate.hs" #-}








{-# LINE 49 "templates\\GenericTemplate.hs" #-}

{-# LINE 59 "templates\\GenericTemplate.hs" #-}

{-# LINE 68 "templates\\GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:\n
notHappyAtAll = error "Internal Happy error\n"
-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
