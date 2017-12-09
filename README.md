# Haskelm: Haskell to Elm Translation

[![Build Status](https://travis-ci.org/JoeyEremondi/haskelm.svg)](https://travis-ci.org/JoeyEremondi/haskelm/builds/37910060#)

**NEW** Updated for Elm 0.13

A program for Haskell to Elm translation, as well as
compilation from within Haskell.

Haskelm is a standalone library, though it does require Elm to be installed using Cabal.

Learn about the Elm programming language at [elm-lang.org](http://elm-lang.org/).


## QuickStart

To see some examples, look at tests/Main.hs
    
## Install
To install the modified library, compiler, and haskelm binary, run

    cabal install haskelm

    
## Use

To use `haskelm` as a binary, simply run

    haskelm [infile]
Note that infile must contain Haskell declarations, but not imports,
module declarations, etc. (This should change in the near future)

The haskelm binary will print to stdout an Elm translation of the given haskell file.

## Library
You can also use Haskelm within a Haskell program, via Template Haskell.
These functions are delcared in Language.Elm.TH

There are two stages to translation: converting a Haskell file into a list of
Template Haskell declarations (type DecsQ),
and translation those declarations.

There are 5 ways you can get Haskell declarations
1. Using TemplateHaskell [d| ... |] brackets
2. From a string which contains a list of declarations (no `module` or `import` statements)
3. From a file containin declarations as in (2)
4. From a string which contains a Haskell module (`module` and `import` statements are discarded but allowed)
5. From a file containing a module as in (4)

It's recomended that you use (5) for files which are already in your
Haskell project, and that whenever you use (4) or (5), you do NOT
splice the Haskell declarations into your code (see below).
The imports are ignored, so this is ideal for simply reading in a Haskell
file which gets compiled into your project (without Template Haskell).

If you would like to simultaneously add Haskell and Elm definitions to
your project, you should use (1), (2) or (3), since they will read in declarations
without any import or module statements. You can then use `declareTranslation`
with `declareHaskell=True` to splice the Haskell definitions in, as well as
a definition for a variable containing the translated Elm string.

Once you have a list of declarations, you can then translate them into elm.
To translate them as an expression, use

    elmString1 = $(elmStringExp defaultOptions $ decsFromModuleFile "myfile.hs")

Then, elmString1 will be a String variable which you can use in your Haskell code.
Note that the Haskell declarations can NOT be spliced into code using this method,
even if the declareHaskell option is set to True.
 
To simultaneously declare Haskell and your translated Elm, use
  $(declareTranslation defaultOptions $ decsFromFile "mydecs.hs")
  
In this case, the Haskell declarations can refer to anything imported by
the module in which you call declareTranslation. Thus it is reccomended that
you don't use `decsFromModuleFile` or `decsFromModule`, since any imports will be discarded.
 
Note that in either case,
`defaultOptions` is a record, so you can modify any of its values in the call.


## Translation

Haskelm can currently translate most basic Haskell, including functions, algebraic data types, newtypes, and type synonyms.
Support is now in place for records, guarded-function-bodies, list-ranges, where-declarations, as-patterns, 
and multi-clause function definitions (pattern matching).

Translation of class or instance declarations is not supported, and will not likely be supported in the near future,
as Elm does not support Type classes.

Most GHC extensions are unsupported, with the exception of Multi-Way-If statements,
since they have a direct translation into Elm.

## Json

Haskelm currently derivies toJson and fromJson functions for all Data declarations.
To get around the lack of TypeClasses in Elm, each translated module contains a 
sum type, called BoxedJson, which wraps around any types defined in the module,
as well as lists, integers, floats, bools, and null.

Values of type `FOO` can be boxed using the constructor `BoxedJsonFOO`.
This also applies to `Int`, `Float`, and `String`.
Note that `BoxedJson_List` wraps a list of type `BoxedJson`.

The Haskell versions of these functions will lbe avaliable soon.
A short-term goal of mine is to switch this format to be compatible with Aeson,
or to use a more efficient binary serialization format such as BSON
or Protocol Buffers.

Json translation can be turned off using the options parameter.
Switching off JSON translations in the Haskelm executable will be supported soon.

## Disclaimer

This is VERY much a work in progress, and is not production ready.
Please feel free to open issues for any bugs or feature suggestions.
