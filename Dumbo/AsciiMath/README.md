[![Build Status](https://travis-ci.org/Kerl13/AsciiMath.svg?branch=master)](https://travis-ci.org/Kerl13/AsciiMath)

# AsciiMath

AsciiMath is a compiler from the amazing [AsciiMath](http://asciimath.org/)
language to LaTeX.

The aim of that project is to provide a Haskell Library and a command line tool
making possible to use asciimath everywhere, for example in interaction with
[pandoc](http://pandoc.org/) or for preprocessing on your website.

## Get started

### Quick install with stack

1. Install
   [stack](http://docs.haskellstack.org/en/stable/install_and_upgrade.html).

2. Clone that repository.

3. Run `stack install` (equivalent to `make stack`) to build and install the
   project using [stack](docs.haskellstack.org/). Stack may ask you to run
   `stack setup` if it has not installed its own version on ghc yet.  
   If you want the binaries to be stored in another folder that the default
   `$HOME/.local/bin/` you should add the option
   `--local-bin-path $YOUR_BIN_DIR`.

### Quick install with cabal

1. Make sure you have [cabal](https://www.haskell.org/cabal/) and
   [ghc](https://www.haskell.org/ghc/) installed on your system.

2. Update your package database (`cabal update`).

3. Install [alex](https://www.haskell.org/alex/) and
   [happy](https://www.haskell.org/happy/), for example by running 
   `cabal install alex && cabal install happy`.

4. Run `cabal configure` and `cabal install` to install dependencies.

5. Finally build the project running `cabal build`. Then you can find the
   binaries in `dist/build/asciimath/` and `dist/build/pandoc-asciimath/`.

_NOTE_ : Run `make cabal` or just `make` do steps 3 and 4 and copy the binaries
in the project main folder.

### Custom install 

1. Compile the auxiliary modules. The source files are stored in `src/lib/`.
   The lexer `lexer.x` must by compiled with
   [alex](https://www.haskell.org/alex/) and the parser `parser.y` must be
   compiled with [happy](https://www.haskell.org/happy/).

2. Compile the compiler. The source file is stored in `src/compiler/`, the only
   module required is `AsciiMath`.

3. Compile the pandoc filter. The source file is stored in `src/filter`.
   Modules `Pandoc-types` and `AsciiMath` are required.

## Usage

The executable `asciimath` reads AsciiMath code from its standard input, compile
it and prints the resulting LaTeX code on its standard output. For example :

    > echo "sum_(i=1)^n i^3=((n(n+1))/2)^2" | ./asciimath
    \sum_{i=1}^{n}i^{3}=\left(\frac{n\left(n+1\right)}{2}\right)^{2}

or in "interactive mode"

    > asciimath
    sin(2x) = 2sin(x)cos(x) ^D
    \sin \left(2x\right)=2\sin \left(x\right)\cos \left(x\right)

The executable `pandoc-asciimath` is a [pandoc
filter](http://pandoc.org/scripting.html). An example of use would be

    > pandoc -s -S -t latex --filter pandoc-asciimath file.md -o file.pdf

The `Asciimath` module built by cabal also provide four functions :
* `readAscii :: String -> Either LexicalError Ast.Code`
* `writeTeX :: Ast.Code -> String`
* `compile :: String -> Either LexicalError String`
* `run :: String -> String`

The `run` function do the same work as `compile` but raises a system error if it
fails whereas `compile` returns either a `LexicalError` or the expected compiled
`String` which is much more convenient for error handling. 

which can be used in any Haskell program to play with the AST or anything else.

## Grammar

Here is the grammar used to parse AsciiMath. It is a little different from the
original one defined [here](http://asciimath.org/#grammar) but I think the
changes I made respect the original idea.


c ::= `[a-zA-Z]` | _numbers_ | _greek letters_ | _standard functions_ | `,` |
_other symbols_ (see [list](http://asciimath.org/#syntax))

u ::= `sqrt` | `text` | `bb` | `bbb` | `cc` | `tt` | `fr` | `sf`
| `hat` | `bar` | `ul` | `vec` | `dot` | `ddot`

b ::= `frac` | `root` | `stackrel`

l ::= `(` | `[` | `{` | `(:` | `{:`

r ::= `)` | `]` | `}` | `:)` | `:}`

S ::= c | l Code r | u S | b S S | `"` _any text_ `"`

E ::= S | S `/` S | S `_` S | S `^` S | S `_` S `^` S

Code ::= [ E ]


## Rendering rules

* The constants are converted to their LaTeX equivalent
* `sqrt`, `hat`, `bar`, `vec`, `dot` and `ddot` are prefixed with a `\`
* `text` and `ul` correspond to the `\textrm` and `underline` functions
* `bb`, `bbb`, `cc`, `tt`, `fr` and `sf` correspond to the `\boldsymbol`,
  `\mathbb`, `\mathcal`, `\texttt`, `\mathfrak` and `\textsf` functions
* `frac` is rendered as a fraction, `root n x` as the `n`-th root of `x` and
  `stackrel x y` displays `x` upon `y`
* Any text placed between a pair of `"` is rendered in the same font as normal
  text.
* `/` stands for a fraction. The `_` and `^` tokens have the same behaviour as
  in LaTeX but the subscript must be placed before the superscript if they are
  both present

### Delimiters

Left and right delimiters are preceded by the `\left` and `\right` commands to
be well-sized. `(:` and `:)` are chevrons. `{:` and `:}` are invisible
delimiters like LaTeX's `{`. The other delimiters are rendered as expected.

Useless delimiters are automatically removed in expressions like
* `(...)/(...)`
* `(...)_(...)`, `(...)^(...)` and the combination of sub- and superscript
* `u (...)`, `b (...) (...)` where `u` and `b` are unary and binary operators

If you want them to be rendered, you have to double them, for example :
`((x+y))/2` or `{: (x+y) :}/2`

### Matrices

An expression of the form `((a,b),(c,d),(e,f))` is interpreted and rendered as a
matrix. More precisely :
* The outer brackets must be `(`,`)` or `[`,`]` and they determine whether the
  rendered matrix is a `pamtrix` or a `bmatrix`.
* The inner brackets must be `(`,`)` or `[`,`]` too. They have to be all the
  same but they can be different from the outer brackets.
* The matrix can have any size but cannot be empty
* In the example, `(a,b)`, `(c,d)` and `(e,f)` are the **lines** of the matrix
* The lines must have the same lengths

## License

The code of AsciiMath is released under the MIT license
