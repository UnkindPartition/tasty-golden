This package provides support for «golden testing».

A golden test is an IO action that writes its result to a file.
To pass the test, this output file should be identical to the corresponding
«golden» file, which contains the correct result for the test.

To **get started** with golden testing and this library, see
[Introduction to golden testing](https://ro-che.info/articles/2017-12-04-golden-tests).

Command-line options
--------------------

To see the command-line options, run your test suite with `--help`. Here's an
example output:

```
Mmm... tasty test suite

Usage: test [-p|--pattern PATTERN] [-t|--timeout DURATION] [-l|--list-tests]
            [-j|--num-threads NUMBER] [-q|--quiet] [--hide-successes]
            [--color never|always|auto] [--ansi-tricks ARG] [--accept]
            [--no-create] [--size-cutoff n]
            [--delete-output never|onpass|always]

Available options:
  -h,--help                Show this help text
  -p,--pattern PATTERN     Select only tests which satisfy a pattern or awk
                           expression
  -t,--timeout DURATION    Timeout for individual tests (suffixes: ms,s,m,h;
                           default: s)
  -l,--list-tests          Do not run the tests; just print their names
  -j,--num-threads NUMBER  Number of threads to use for tests
                           execution (default: # of cores/capabilities)
  -q,--quiet               Do not produce any output; indicate success only by
                           the exit code
  --hide-successes         Do not print tests that passed successfully
  --color never|always|auto
                           When to use colored output (default: auto)
  --ansi-tricks ARG        Enable various ANSI terminal tricks. Can be set to
                           'true' or 'false'. (default: true)
  --accept                 Accept current results of golden tests
  --no-create              Error when golden file does not exist
  --size-cutoff n          hide golden test output if it's larger than n
                           bytes (default: 1000)
  --delete-output never|onpass|always
                           If there is a golden file, when to delete output
                           files (default: never)
```

See also [tasty's README](https://github.com/UnkindPartition/tasty/blob/master/README.md#runtime).

Maintainers
-----------

[Roman Cheplyaka](https://github.com/UnkindPartition) is the primary maintainer.

[Oliver Charles](https://github.com/ocharles) is the backup maintainer. Please
get in touch with him if the primary maintainer cannot be reached.
