{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Grammars.LabelledBNF where

import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc, dumpCode)

-- Labelled BNF
bnfc [lbnf|
-- A Grammar is a sequence of definitions

MkGrammar . Grammar ::= [Def] ;

[]  . [Def] ::= ;
(:) . [Def] ::= Def ";" [Def] ;

[]  . [Item] ::= ;
(:) . [Item] ::= Item [Item] ;

--The rules of the grammar
Rule . Def ::= Label "." Cat "::=" [Item] ;

-- Items
Terminal  . Item ::= String ;
NTerminal . Item ::= Cat ;

-- Categories
ListCat  . Cat ::= "[" Cat "]" ;
IdCat    . Cat ::= Ident ;

-- labels with or without profiles
LabNoP   . Label ::= LabelId ;
LabP     . Label ::= LabelId [ProfItem] ;
LabPF    . Label ::= LabelId LabelId [ProfItem] ;
LabF     . Label ::= LabelId LabelId ;

-- functional labels
Id       . LabelId ::= Ident ;
Wild     . LabelId ::= "_" ;
ListE    . LabelId ::= "[" "]" ;
ListCons . LabelId ::= "(" ":" ")" ;
ListOne  . LabelId ::= "(" ":" "[" "]" ")" ;

-- profiles (= permutation and binding patterns)
ProfIt   . ProfItem ::= "(" "[" [IntList] "]" "," "[" [Integer] "]" ")" ;

Ints     . IntList ::= "[" [Integer] "]" ;

separator Integer "," ;
separator IntList "," ;
terminator nonempty ProfItem "" ;

-- Pragmas
Comment  .  Def ::= "comment" String ;
Comments .  Def ::= "comment" String String ;
Internal .  Def ::= "internal" Label "." Cat "::=" [Item] ;
Token.      Def ::= "token" Ident Reg ;
PosToken.   Def ::= "position" "token" Ident Reg ;
Entryp.     Def ::= "entrypoints" [Ident] ;
Separator.  Def ::= "separator" MinimumSize Cat String ;
Terminator. Def ::= "terminator" MinimumSize Cat String ;
Coercions.  Def ::= "coercions" Ident Integer ;
Rules.      Def ::= "rules" Ident "::=" [RHS] ;

Layout.     Def ::= "layout" [String] ;
LayoutStop. Def ::= "layout" "stop" [String] ;
LayoutTop.  Def ::= "layout" "toplevel" ;

separator nonempty String "," ;

separator nonempty RHS "|" ;
MkRHS. RHS ::= [Item] ;

-- List size condition
MNonempty.  MinimumSize ::= "nonempty" ;
MEmpty.     MinimumSize ::=  ;

-- regular expressions
RSeq.   Reg2 ::= Reg2 Reg3 ;
RAlt.   Reg1 ::= Reg1 "|" Reg2 ;
RMinus. Reg1 ::= Reg2 "-" Reg2 ;

RStar.  Reg3 ::= Reg3 "*" ;
RPlus.  Reg3 ::= Reg3 "+" ;
ROpt.   Reg3 ::= Reg3 "?" ;

REps.   Reg3 ::= "eps" ;

RChar.  Reg3 ::= Char ;           -- single character
RAlts.  Reg3 ::= "[" String "]" ; -- list of alternative characters
RSeqs.  Reg3 ::= "{" String "}" ; -- character sequence

RDigit.  Reg3 ::= "digit" ;
RLetter. Reg3 ::= "letter" ;
RUpper.  Reg3 ::= "upper" ;
RLower.  Reg3 ::= "lower" ;
RAny.    Reg3 ::= "char" ;

_. Reg  ::= Reg1 ;
_. Reg1 ::= Reg2 ;
_. Reg2 ::= Reg3 ;
_. Reg3 ::= "(" Reg ")" ;

-- list of categories in the entrypoint pragma
(:[]).  [Ident] ::= Ident ;
(:).    [Ident] ::= Ident "," [Ident] ;

-- comments in BNF source
comment "--" ;
comment "{-" "-}" ;
|]