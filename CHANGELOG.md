Changes
=======

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
