binary-ext package
==================

[![Hackage version](https://img.shields.io/hackage/v/binary-ext.svg)](https://hackage.haskell.org/package/binary-ext)
[![Stackage version](https://www.stackage.org/package/binary-ext/badge/lts?label=stackage)](https://www.stackage.org/package/binary-ext)
[![Stackage nightly version](https://www.stackage.org/package/binary-ext/badge/nightly?label=nightly)](https://www.stackage.org/package/binary-ext)
[![Build status](https://secure.travis-ci.org/A1-Triard/binary-ext.png?branch=master)](http://travis-ci.org/A1-Triard/binary-ext)

An alternate with typed errors and other advantages for the `Data.Binary.Get.Get` monad from the `binary` package
and `Data.Attoparsec.Text.Parser` monad from the `attoparsec` package.

Building binary-ext
-------------------

Here's how to get the latest version of the repository and build.

    $ git clone https://github.com/A1-Triard/binary-ext.git
    $ cd binary-ext
    $ stack build

Run the test suite.

    $ stack test

Using binary-ext for binary serialization/deserialization
---------------------------------------------------------
Import modules:

    import Data.Conduit.Parsers.Binary.Get
    import Data.Conduit.Parsers.Binary.Put

and then use the `Get` and `Put` monads to serialize/deserialize.

More information in the haddock documentation.

Using binary-ext for text serialization/deserialization
---------------------------------------------------------
Import modules:

    import Data.Conduit.Parsers.Text.Parser
    import Data.Conduit.Parsers.Text.Gen

and then use the `Parser` and `TextGen` monads to serialize/deserialize.

More information in the haddock documentation.
