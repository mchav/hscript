# sabela

Utility for running ad-hoc Haskell/GHCi scripts, generating Haskell markdown documentation, and running reactive notebooks.

The design document for the project can be found [here](https://docs.google.com/document/d/1XQyKdhEbzZRCkyKS6efbFY0NgRkneJTpMQXTwklZgso/edit?tab=t.0).

This cumulatively solves a couple of problems I've had with other Haskell solutions:

* literate Haskell - can't avoid setup so most literate Haskell files have a lot of imports and noise.
* Jupyter notebooks - You always have to open them with jupyter or convert them. Also can't really avoid setup.
* cabal scripts - closer to what I wanted but you can't attach documentation.
* markdown-unit - kind of does what I want but wanted to interleave output so I can geenrate documentation
* doctests - those are for inline documentation.

Will add features as I find useful.

## Quick start

```bash
git clone https://github.com/mchav/sabela
cd sabela
cabal install
```

## Usage

### Reactive notebooks

Run `cabal run sabela-server`, open `localhost:3000/index.html` and explore `./examples/analysis.md` for a quick tutorial.

### Markdown notebook mode

Give sabela a `.md` file and it executes all `` ```haskell `` code blocks in a shared session, inserting results as blockquotes:

````markdown
# Iris analysis

The code below computes the dimensions of the iris dataframe.

```haskell
-- cabal: build-depends: base, dataframe, text
import qualified DataFrame as D
iris <- D.readParquet "data/iris.parquet"
D.dimensions iris
```

That's all it took.
````

Run it:

```bash
sabela notebook.md > output.md
```

Output:

````markdown
# Iris analysis

The code below computes the dimensions of the iris dataframe.

```haskell
-- cabal: build-depends: base, dataframe, text
import qualified DataFrame as D
iris <- D.readParquet "data/iris.parquet"
D.dimensions iris
```
> (150, 5)

That's all it took.
````

State carries across blocks, so later blocks can use bindings from earlier ones. Non-haskell code blocks (e.g. `` ```python ``) are left untouched. But similar to ghci you can also override variables.

### Script mode

Write a `.ghci` or `.hs` file:

```haskell
-- analysis.ghci
-- cabal: build-depends: base, dataframe, text
-- cabal: default-extensions: TemplateHaskell, TypeApplications, OverloadedStrings

import qualified DataFrame as D

iris <- D.readParquet "data/iris.parquet"

import Data.Text (Text)
import DataFrame ((|>))

iris |>
  D.filterWhere (F.col @Text "variety" .== "Setosa") |>
  D.filterWhere (F.col @Double "sepal.length" .> 5.4)

$(F.declareColumns iris)

D.derive "ratio" (sepal_width / sepal_length) iris
```

Run it:

```bash
sabela analysis.ghci
```

Dependencies are resolved similarly to how they are in cabal scripts. Imports, IO binds, expressions, GHCi commands, and Template Haskell splices can appear in any order (note: their order matters relative to each other).


## Cabal metadata

Declare dependencies and extensions in comments anywhere in your script:

```haskell
-- cabal: build-depends: base, text, containers
-- cabal: default-extensions: OverloadedStrings, TypeApplications
-- cabal: ghc-options: -threaded
```

In markdown mode, metadata from any code block is merged — so you can declare deps in the first block and use them everywhere.

## Remote scripts

Pull in shared preambles with `:remoteScript`:

```haskell
import qualified DataFrame as D
iris <- D.readParquet "data/iris.parquet"

:remoteScript https://raw.githubusercontent.com/mchav/ihaskell-dataframe/main/rc.hs

$(F.declareColumns iris)
iris |> D.filterWhere (variety .== "Setosa")
```

The remote file is fetched, parsed, and inlined before execution. Remote scripts can themselves contain `:remoteScript` directives (resolved recursively). This is useful for sharing common imports, `:set` flags, `:def!` macros, and setup code across scripts.

I haven't yet thought about the security implications of doing so. Maybe it'll go behind a flag in future.
