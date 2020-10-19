---
eleventyNavigation:
  key: Python testing
  order: 4
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

Packages that support implementing tests for Python code
are described below.

## Unit tests

### pytest

#### Overview

The {% aTargetBlank "https://pytest.org", "pytest" %} framework
runs unit tests of Python code and reports test results.
It can also run tests written with the unittest and nose test frameworks.

To install pytest, enter `pip install pytest`.

The names of test source files should begin or end with `test`
and often match the filename of the source file they test.
For example, tests for `math_util.py` could go in `math_util_test.py`.

Inside test files, implement any number of functions
whose names begin with `test_`.
These functions execute the code to be tested
and make assertions about the expected results.

#### Assertions

Assertions are made using the Python language `assert` keyword.
This is followed by an expression that is evaluated as a boolean.
If it evaluates to `True`, nothing happens.
If it evaluates to `False`, an `AssertionError` is raised.
Test frameworks like pytest catch these in order to report test failures.
For example:

```python
def test_average():
    assert average([1, 2, 3, 4]) == 2.5
    assert average([3]) == 3
```

The condition being asserted can be followed by a string
that describes why the assertion might fail.
For example:

```python
assert average(1, 2, 3, 4) == 2.5, 'incorrect result from average'
```

Often added this string is unnecessary because pytest
uses introspection to automatically generate reasonable messages.

To test code that is expected to raise an exception,
use the `pytest.raises` function. For example:

```python
with pytest.raises(ZeroDivisionError):
    average()
```

When the code that should raise an exception is a call to
a single function, `pytest.raises` can instead be called with
the expected exception class, the function,
and the arguments to be passed to the function.
For example:

```python
pytest.raises(statistics.StatisticsError, average, [])
```

#### Skipping tests

To temporarily skip running a test function,
add the following decorator to the function:

```python
@pytest.mark.skip(reason='some reason why')
```

To temporarily skip running all the tests in a file,
add the following near the top of the file:

```python
pytest.skip('some reason why', allow_module_level=True)
```

For additional variations such as conditionally skipping tests,
see {% aTargetBlank "https://docs.pytest.org/en/latest/skipping.html",
"Skip and xfail" %}.

#### Running tests

To run tests, enter `pytest`.
For more brief output, add the `--quiet` (`-q`) option.
To report skipped tests, add the `-rs` option.

By default anything written to stdout is "captured".
To avoid this and make the output is visible,
add the `--captured=no` or `-s` option.

#### Setup and Teardown

To run setup and teardown code, define the following functions
that each have no parameters:

- `setup_module` - run once before all the test functions
- `setup_function` - run once before each test function
- `teardown_function` - run once after each test function
- `teardown_module` - run once after all the test functions

#### Fixtures

Fixtures are special functions that
provide data and functions to test functions.
A function becomes a fixture when it is marked with
the `@pytest.fixture` decorator.

Each fixture has an associated scope that controls
whether a cached value is used after the initial call
when it is used by multiple tests.
The scope value is a string that can be
`function` (default), `class`, `module`, `package`, or `session`.
When all the tests in file can use the same fixture result,
set the scope to `module`.
For example:

```python
@pytest.fixture(scope='module')
def numbers():
    return (1, 2, 3)

def test_add(numbers):
    assert add(numbers) == 6

def test_average(numbers):
    assert average(numbers) == 2

```

To write a test that asserts what a function will write to stdout or stderr,
use the provided `capsys` fixture. For example:

```python
def test_greet(capsys):
    greet('Mark')
    captured = capsys.readouterr()
    assert captured.out = 'Hello, Mark!'
```

For more detail, see {% aTargetBlank
"https://docs.pytest.org/en/stable/fixture.html", "pytest fixtures" %}.

#### Watching files

To watch files for changes and automatically rerun tests, use
{% aTargetBlank "https://github.com/joeyespo/pytest-watch", "pytest-watch" %}.
To install it, enter `pip install pytest-watch`.
To run tests in watch mode, enter `ptw`.
Options after `--` are passed on to `pytest`.
For example, to run tests in quiet mode, enter `ptw -- -q`.

### unittest

unittest is in the Python standard library, so does not need to be installed.

### nose2

TODO: Add content

## Code coverage

TODO: Add content

### codecov

TODO: Add content

### coverage

TODO: Add content

## End-to-end tests

There is nothing Python-specific about implementing end-to-end tests.
For web applications, {% aTargetBlank "https://www.cypress.io/", "Cypress" %}
is recommended.
