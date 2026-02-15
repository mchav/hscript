# Sabela Example

This is an example of Sabela, a Haskell static notebook-like thing.

```haskell
print (5 + 5)
```

We can embed Haskell computation in markdown documents but we get more flexibility since we can do ghci stuff.

```haskell
-- Main.ghci
-- cabal: build-depends: base, dataframe, text, time
-- cabal: default-extensions: TemplateHaskell, TypeApplications, OverloadedStrings, DataKinds

import qualified DataFrame as D

:remoteScript https://raw.githubusercontent.com/mchav/ihaskell-dataframe/refs/heads/main/rc.hs

df = D.fromNamedColumns [("key", D.fromList ["K0", "K1", "K2", "K3"]), ("A", D.fromList ["A0", "A1", "A2", "A3"])]

$(F.declareColumns df)

other = D.fromNamedColumns [("key", D.fromList ["K0", "K1", "K2"]), ("B", D.fromList ["B0", "B1", "B2"])]

$(F.declareColumns other)

import qualified Data.Text.IO as T

T.putStrLn $ D.toMarkdownTable $ D.innerJoin [F.name key] df other
```

That's it!
