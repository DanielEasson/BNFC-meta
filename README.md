## BNFC-meta and friends

[![GitHub CI][github-shield]][github-ci]

This repo consists of three Haskell packages:

- [![Hackage][hackage-BNFC-meta-shield]][hackage-BNFC-meta] BNFC-meta — the main one
- [![Hackage][hackage-alex-meta-shield]][hackage-alex-meta] alex-meta
- [![Hackage][hackage-happy-meta-shield]][hackage-happy-meta] happy-meta

See the description of the main one in its `.cabal` file.

### GHC Compatibility

This version only works for GHC 8 and (hopefully) later.


[github-shield]: https://github.com/ulysses4ever/BNFC-meta/actions/workflows/ci.yml/badge.svg
[github-ci]: https://github.com/ulysses4ever/BNFC-meta/actions/workflows/ci.yml

[hackage-alex-meta]: http://hackage.haskell.org/package/alex-meta
[hackage-alex-meta-shield]: https://img.shields.io/hackage/v/alex-meta.svg
[hackage-happy-meta]: http://hackage.haskell.org/package/happy-meta
[hackage-happy-meta-shield]: https://img.shields.io/hackage/v/happy-meta.svg
[hackage-BNFC-meta]: http://hackage.haskell.org/package/BNFC-meta
[hackage-BNFC-meta-shield]: https://img.shields.io/hackage/v/BNFC-meta.svg

### Pretty Printing

This fork adds pretty printing capabilities to BNFC-meta.

The generated pretty printer can be customised, using annotations, applied to the right of an item and enclosed in round brackets.

For example a simple lambda calculus grammar would be:

Abs. Expr ::= "\\" Ident "->" Expr ;

App. Expr1 ::= Expr1 Expr2 ;

Var. Expr2 ::= Ident ;

coercions Expr 2 ;

After applying annotations the grammar would be:

Abs. Expr ::= "\\" (Space 1) Ident (Space 1, Nest 2, Fg Blue) "->" (Fg Red) Expr (Fg RGB 128 0 128) ;

App. Expr1 ::= Expr1 Expr2 ;

Var. Expr2 ::= Ident ;

coercions Expr 2 ;

### Pretty Printing Features

The following annotations can be used:

- Space: Enforces a space based on the number given. Space remains after grouping.
- Empty: Enforces no space between items regardless of grouping.
- Linebreak: Replaces regular line with a linebreak that becomes Empty when grouped.
- Nest: Adds indentation based on the number given.
- Fg: Sets foreground color based on preset colours or custom RGB.
- Bg: Sets background color based on preset colours or custom RGB.
- Style: Sets style to italic, bold, invert, underline or blink.

Additionally, defaults can be used for Fg, Bg and Style to apply every item at once likem so:
DefaultFg Blue ;
DefaultBg Red ;
DefaultStyle Invert, Underline ;



