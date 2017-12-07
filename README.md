binary-ext package
==================

An alternate with typed errors for the `Data.Binary.Get` monad from the `binary` package
and adapters from the `binary-conduit` package.

Building binary-ext
-------------------

Here's how to get the latest version of the repository and build.

    $ git clone https://github.com/A1-Triard/binary-ext.git
    $ cd binary-ext
    $ stack build

Run the test suite.

    $ stack test

Using binary-ext
----------------

First:

    import Data.Binary.Conduit.Put
    import Data.Binary.Conduit.Get

and then use the `Get` and `Put` monads to serialize/deserialize.

More information in the haddock documentation.
