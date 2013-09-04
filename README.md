This package provides support for «golden testing».

A golden test is an IO action that writes its result to a file.
To pass the test, this output file should be identical to the corresponding
«golden» file, which contains the correct result for the test.

Status
------

This library is deprecated in favour of [tasty-golden][]. `tasty-golden` has
some additional features, such as test management interface.

However, I will continue to provide support for `test-framework-golden` if requested.

[tasty-golden]: https://github.com/feuerbach/tasty-golden
