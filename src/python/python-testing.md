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

If an `assert` throws, the remainder of the test function is not executed,
but other test functions are executed.

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

#### Running tests

To run tests, enter `pytest`.
By default this runs all the test files found in
the current directory and in subdirectories.
For more brief output, add the `--quiet` (`-q`) option.
To report skipped tests (described later), add the `-rs` option.

To run specific test files, add their file paths
as arguments to the `pytest` command.

By default anything written to stdout is "captured".
To avoid this and make the output is visible,
add the `--captured=no` or `-s` option.

#### Type checking

To make assertions about the types of variables or expressions in tests,
use the `isinstance` built-in function. For example:

```python
assert isinstance(happy, bool)
assert isinstance(name, str)
assert isinstance(age, int)
assert isinstance(temperature, float)
assert isinstance(person, Person) # custom class
```

#### Custom exceptions

Code being tested can define and raise custom errors.
For example, the following is in `math_util.py`:

```python
# Define a custom exception type.
class MathError(Exception):
    pass

# This is a contrived example because we can just use math.sqrt directly.
def square_root(x):
    if x < 0:
        raise MathError('cannot find square root of a negative number')
    return math.sqrt(x)
```

We can write a test that verifies that this error is raised when appropriate.
For example, the following is in `math_util_test.py`:

```python
import pytest
from math_util import MathError, square_root

def test_square_root():
    assert square_root(4) == 2

    with pytest.raises(MathError) as context:
       square_root(-4) == 2
    assert isinstance(context.value, MathError)
    assert str(context.value) == 'cannot find square root of a negative number'
```

#### Setup and Teardown

To run setup and teardown code, define the following functions
that each have no parameters:

- `setup_module` - run once before all the test functions
- `setup_function` - run once before each test function
- `teardown_function` - run once after each test function
- `teardown_module` - run once after all the test functions

#### Parametrized tests

To run a test function repeatedly with
different sets of inputs and expected results,
use the `@pytest.mark.parametrize` decorator. For example:

```python
add_inputs = [
    (1, 2, 3),
    (4, 5, 9)
]
@pytest.mark.parametrize('n1, n2, expected', add_inputs)
def test_add(n1, n2, expected):
    assert add([n1, n2]) == expected
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

Another option is to mark test that are expected to fail,
but should be run anyway, with the `@pytest.mark.xfail` decorator.
These are counted separately from passing, failing, and skipped tests.
It's not clear to me why this is a useful alternative to skipping tests.

For additional variations such as conditionally skipping tests,
see {% aTargetBlank "https://docs.pytest.org/en/latest/skipping.html",
"Skip and xfail" %}.

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

Pytest provides some built-in fixtures.
The `capsys` fixture enables writing test that
asserts what a function will write to stdout or stderr,
For example:

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

### Alternatives

{% aTargetBlank "https://docs.python.org/3/library/unittest.html", "unittest" %}
is another Python testing framework.
It is notable in that it is included in the Python standard library,
so does not need to be installed.
However, it is more complicated and less popular than pytest.

{% aTargetBlank "https://docs.nose2.io/en/latest/", "nose2" %}
is another Python test framework that is less popular than pytest.

## Code coverage

{% aTargetBlank "https://coverage.readthedocs.io/en/coverage-5.3/",
"Coverage.py" %} is a tool for reporting code coverage of Python tests.

To install this tool, enter `pip install coverage`.

To gather coverage information from running pytest tests,
enter `coverage run -m pytest`.

To display a coverage report, enter `coverage report`.
The output will be similar to the following:

```text
Name                  Stmts   Miss  Cover
-----------------------------------------
math_util.py             12      3    75%
math_util_test.py        43      0   100%
string_util.py            4      0   100%
string_util_test.py      10      0   100%
-----------------------------------------
TOTAL                    69      3    96%
```

To create an HTML test coverage report, enter `coverage html`.
This generates files in a directory named `htmlcov`.
To view the report, open the `index.html` in this directory in any web browser.

## End-to-end tests

There is nothing Python-specific about implementing end-to-end tests.
For web applications, {% aTargetBlank "https://www.cypress.io/", "Cypress" %}
is recommended.
