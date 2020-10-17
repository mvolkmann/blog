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

The {% aTargetBlank "https://pytest.org", "pytest" %} framework
runs unit tests of Python code and reports test results.
It can also run tests written with the unittest and nose test frameworks.

To install pytest, enter `pip install pytest`.

The names of test source files should begin with `test_`
and often match the filename of the source file they test.
For example, tests for `math_util.py` should go in `test_math_util.py`.

Tests make assertions using the Python language `assert` keyword.
This is followed by an expression that is evaluated as a boolean.
If it evaluates to `True`, nothing happens.
If it evaluates to `False`, an `AssertionError` is raised.
Test frameworks like pytest catch those in order to report test failures.
For example:

```python
assert average(1, 2, 3, 4) == 2.5
```

To test code that is expected to raise an exception,
use the `pytest.raises` function. For example:

```python
with pytest.raises(ZeroDivisionError):
    average()
```

To run tests, enter `pytest`.
For more brief output, add the `--quiet` (`-q`) option.

By default anything written to stdout is "captured".
To avoid this and make the output is visible,
add the `--captured=no` or `-s` option.

To run setup and teardown code, define the following functions
that each have no parameters:

- `setup_module` - run once before all the test functions are run
- `setup_function` - run once before each test function is run
- `teardown_function` - run once after each test function is run
- `teardown_module` - run once after all the test functions are run

Fixtures are special functions that retrieves or generates data
that is used by multiple tests.
A function becomes a fixture when it is marked with
the `@pytest.fixture` decorator.
Their result is cached in a particular scope
and the function is not run again until ???.

For example:

```python
@pytest.fixture
def numbers():
    return (1, 2, 3)

def test_add_with_fixture(numbers):
    assert add(*numbers) == 6
```

To watch files for changes and automatically rerun tests, use
{% aTargetBlank "https://github.com/joeyespo/pytest-watch", "pytest-watch" %}.
To install it, enter `pip install pytest-watch`.
To run tests in watch mode, enter `ptw`.
Options after `--` are passed on to `pytest`.
To run them in quiet mode, enter `ptw -- -q`.

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
