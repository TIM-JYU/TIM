[![Build Status](https://travis-ci.org/Kerl13/AsciiMath.svg?branch=master)](https://travis-ci.org/Kerl13/AsciiMath)

# AsciiMath

AsciiMath is a compiler from the amazing [AsciiMath](http://asciimath.org/)
language to LaTeX.

The aim of that project is to provide a Haskell Library and a command line tool
making possible to use asciimath everywhere, for example in interaction with
[pandoc](http://pandoc.org/) or for preprocessing on your website.

## Install

Use [stack](http://docs.haskellstack.org/en/stable/install_and_upgrade.html).

If you already have stack set up and working, you just have to type
`stack install` at the root of the repository.  
If you want the binaries to be stored in a different folder from the default
`$HOME/.local/bin/` you should add the option `--local-bin-path $YOUR_BIN_DIR`.

If you just installed stack, stack may ask you to run `stack setup` first to
install GHC and set up an isolated environment (either globally in your home
or in a dedicated folder just for this repository)


## Usage

The executable `asciimath` reads AsciiMath code from its standard input, compile
it and prints the resulting LaTeX code on its standard output. For example:

    > echo "sum_(i=1)^n i^3=((n(n+1))/2)^2" | ./asciimath
    \sum_{i=1}^{n}i^{3}=\left(\frac{n\left(n+1\right)}{2}\right)^{2}

or in "interactive mode"

    > asciimath
    sin(2x) = 2sin(x)cos(x) ^D
    \sin \left(2x\right)=2\sin \left(x\right)\cos \left(x\right)

The executable `pandoc-asciimath` is a [pandoc
filter](http://pandoc.org/scripting.html). An example of use would be

    > pandoc --standalone -t latex --filter pandoc-asciimath file.md -o file.pdf

The `Asciimath` module also provide four functions:
* `readAscii :: String -> Either LexicalError Ast.Code`
* `writeTeX :: Ast.Code -> String`
* `compile :: String -> Either LexicalError String`
* `run :: String -> String`

which can be used in any Haskell program to play with the AST or anything else.

The `run` function do the same work as `compile` but raises a system error if it
fails whereas `compile` returns either a `LexicalError` or the expected compiled
`String` which is much more convenient for error handling.

## Grammar

Here is the grammar used to parse AsciiMath. It is a little different from the
original one defined [here](http://asciimath.org/#grammar) but I think the
changes I made respect the original idea.


c ::= `[a-zA-Z]` | _numbers_ | _greek letters_ | _standard functions_ | `,` |
_other symbols_ (see [list](http://asciimath.org/#syntax))

u ::= `sqrt` | `text` | `bb` | `bbb` | `cc` | `tt` | `fr` | `sf` | `hat` | `bar` | `ul` | `vec` | `dot` | `ddot`

b ::= `frac` | `root` | `stackrel`

l ::= `(` | `[` | `{` | `(:` | `{:`

r ::= `)` | `]` | `}` | `:)` | `:}`

S ::= c | l Code r | u S | b S S | `"` _any text_ `"`

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
