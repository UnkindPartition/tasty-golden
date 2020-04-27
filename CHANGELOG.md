Changes
=======

Version 2.3.3.2
---------------

* Fix a bug where the `TASTY_SIZE_CUTOFF` env. variable would be ignored

Version 2.3.3.1
---------------

* Fix a bug with UTF-8 output

Version 2.3.3
-------------

* Expose `createDirectoriesAndWriteFile`
* Add `--size-cutoff` to truncate large golden test output
* Restore support for GHC >= 7.8

Version 2.3.2.1
---------------

Create missing directories when writing golden files

Version 2.3.2
-------------

Add a `--no-create` flag

Version 2.3.1.3
---------------

Make the environment variable `TASTY_ACCEPT=True` work, and make the value
    case-insensitive (so `TASTY_ACCEPT=true` works, too)

Version 2.3.1.2
---------------

Docs: link to an introductory blog post

Version 2.3.1.1
---------------

Fix compatibility with `optparse-applicative-0.13`

Version 2.3.1
-------------

Intercept exceptions thrown by the test, adhering to the new tasty API
contract.

Version 2.3.0.2
---------------

Switch from temporary-rc to temporary

Version 2.3.0.1
---------------

Impose a lower bound version constraint on bytestring.

Version 2.3
-----------

* Accepting tests is no longer done by a separate ingredient; instead it is now
  an option that affects tests themselves.
    * `--accept` used to run only golden tests; now all tests are run, but only
      golden tests are affected by this option
    * when accepting, all the usual options apply (such as `-j`)
    * when accepting, the interace is the same as when running
    * `defaultMain` and `acceptingTests` are kept for compatibility, but do not
      do anything and are obsolete
* When a golden test file does not exist, it is created automatically, even when
  `--accept` is not specified. You'll see a message like

        UnboxedTuples:                 OK (0.04s)
          Golden file did not exist; created

* No longer use lazy IO
    * `ValueGetter` type is gone (replaced by `IO`)
    * Because of that, the type of the primitive `goldenTest` is changed
    * `vgReadFile` function is gone (replaced by `Data.ByteString.readFile`)

Version 2.2.2.4
---------------

* Warn when some tests threw exceptions during `--accept`
* Properly handle exceptions; don't swallow Ctrl-C

Version 2.2.2.3
---------------

Restore compatibility with older compilers

Version 2.2.2.1
---------------

Relax `Cabal` dependency

Version 2.2.2
-------------

Add `findByExtension`

Version 2.2.1.2
---------------

Catch exceptions when accepting golden tests

Version 2.2.1.1
---------------

Switch to `temporary-rc`

Version 2.2.1
-------------

* Fix a bug where the result of the comparison function would reference yet
  unread data from a semiclosed file and the file gets closed, leading to a
  runtime exception
* Export `writeBinaryFile`
* Improve the docs
* Update to work with `tasty-0.8`

Version 2.2.0.2
---------------

Update to work with `tasty-0.7`

Version 2.2.0.1
---------------

Update to work with `tasty-0.5`

Version 2.2
-----------

Migrate to ingredients

Version 2.1
-----------

Add `goldenVsStringDiff`

Version 2.0.1
-------------

Update to work with `tasty-0.2`

Version 2.0
-----------

Initial release of `tasty-golden` (derived from `test-framework-golden-1.1.x`)
