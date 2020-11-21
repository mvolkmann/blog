---
eleventyNavigation:
  key: PHP
layout: topic-layout.njk
---

<img alt="PHP mascot" class="keep-size" src="/blog/assets/php-mascot.png">

PHP is an interpreted scripting language that is
typically used to implement web applications.
It was created by Ramus Lerdorf in 1994.
PHP version 8 was released in November, 2020.
A large number of PHP sites continue to use PHP version 5 or older.
The most popular implementation is the Zend Engine,
which is the default PHP interpreter in the Apache server.

The name PHP was an acronym for "Personal Home Page",
but now is an acronym for "PHP Hypertext Preprocessor".

PHP source files typically contain HTML and CSS with embedded PHP code.
This files are typically processed in an HTTP server and
produce HTML that is returned to a web browser for rendering.
Unlike JavaScript, PHP code does not execute in web browsers.

Typically data is passed from browsers to the server
using HTTP requests that contain data in HTML forms.
HTML forms can contain `input` elements with `type="hidden"`
to send data that is not rendered.

## Installing

A recommended way to obtain a local HTTP server
is to use XAMPP. I tried this, but failed to get it to work in macOS.

Another option in macOS is to use the built-in support for Apache and PHP.
See {% aTargetBlank
"https://jasonmccreary.me/articles/install-apache-php-mysql-mac-os-x-catalina/",
"Installing Apache, PHP, and MySQL on macOS Catalina" %}.

1. Start the Apache HTTP server.
   - `sudo su`
   - `sudo apachectl start`
1. Browse {% aTargetBlank "http://localhost", "http://localhost" %}
   to verify that it is running.
1. Make a backup of the Apache configuration file.
   - `cd /etc/apache2/`
   - `cp httpd.conf httpd.conf-backup`
1. Enable use of PHP in Apache.
   - Edit httpd.conf.
   - Uncomment the line `LoadModule php7_module libexec/apache2/libphp7.so`.
1. Restart Apache.
   - `apachectl restart`
1. Enable creating new documents as a non-root user.
   - `sudo chmod a+w /Library/WebServer/Documents`
1. Create the file `/Library/WebServer/Documents/phpinfo.php`
   containing `<?php phpinfo();`
1. Browse {% aTargetBlank "http://localhost/phpinfo.php", "http://localhost/phpinfo.php" %}.
1. Create your first PHP page.

   - Create the file `hello.php`
     in the `/Library/WebServer/Documents` directory.
   - Add the following content:

     ```php
     <!DOCTYPE html>
     <html>
       <head>
         <title>PHP Hello World</title>
       </head>
       <body>
       <h1>Verifying PHP</h1>
         <?php
           echo "Hello, World!";
         ?>
         <p>Did you see it?</p>
       </body>
     </html>
     ```

   - Browse `localhost/hello`.

1. See the "Installing" link above for steps to install MySQL.

## Linting

PHP has a builtin code linter.
To run it, enter `php -l {filename}.php` or `php -l *.php`.

## Code Formatting

To format PHP files using Prettier:

- `npm install -g prettier @prettier/plugin-php`
- `prettier {filename}.php --write` or `prettier *.php --write`

For details on the Prettier plugin for PHP, see
{% aTargetBlank "https://github.com/prettier/plugin-php", "here" %}.

To configure Prettier for PHP,
create a `.prettierrc` file similar to the following:

```json
{
  "braceStyle": "1tbs",
  "phpVersion": "7.3",
  "singleQuote": true,
  "tabWidth": 2,
  "trailingCommaPHP": false
}
```

Setting `phpVersion` allows the Prettier plugin for PHP to use
modern language features that were added in the specified version and earlier.

## Syntax

To embed PHP code in HTML and CSS, use the following syntax:

```php
<?php
  // code
?>
```

This code can add to the response using the `echo` command.
For example, `echo "<h1>" . $title . "</h1>";`.
Note that HTML tags can be output.

To embed the value of a PHP variable, use the following syntax:

```php
<?= $variableName ?>
```

PHP statements are separated from each other by semicolons,
which can also appear as statement terminators
with the last statement having an optional terminator.

PHP variable names must begin with `$`
and typical follow that with a lowercase letter.
When the name consists of multiple works, camel-case is preferred.
Variable names are case-sensitive, but class and function names are not.

Names of constants are typically all uppercase.

Much of the syntax of PHP matches that of JavaScript.

## Comments

Single-line comments begin with `//` or `#`.
Multi-line comments are delimited by `/*` and `*/`.

## Builtin Data Types

The PHP builtin data types are
Boolean, Integer, Float, String,
Array, Object, `NULL`, and Resource.

`NULL` represents having no value.
It is the default value of variables that have not be assigned a value.

The boolean values `true` and `false` and the `NULL` value are case insensitive
and are commonly written in all lowercase.

The Resource type is used to store references to
functions and other resources that are defined outside of PHP.

## Strings

Strings can be delimited with single or double quotes.
PHP also supports "heredocs". TODO: Show this syntax!

Strings can be concatenated using the `.` (period) operator.

Double-quoted strings support interpolation of PHP variables.
For example, these lines are equivalent:

```php
echo "color = " . $color . "<br>";
echo "color = $color<br>";
```

Variable names in strings can optionally be surrounded by curly braces
to prevent ambiguity.
However, only variable names can be interpreted.
Expressions, including function calls, in curly braces are not interpolated.

Another way to create a string is a "heredoc"
which can create a multi-line string that
supports interpolation like double quoted strings.
For example:

```php
$address = <<<EOT
123 Some Street
Somewhere, MO
12345
EOT;
```

Any delimiter word can follow `<<<` as long as the same word is used at the end.

The final way to create a string is a "nowdoc".
This is similar to a heredoc,
but like single-quoted strings does not support interpolation.
For example:

```php
$address = <<<'EOT'
123 Some Street
Somewhere, MO
12345
EOT;
```

Note that the first occurrence of the delimiter word is in single quotes.

## Arrays

The `array` function is used to created an array.
For example:

```php
$colors = array("red", "green", "blue");
```

There are three types of arrays: indexed, associative, and multidimensional.

The `count` function takes an array and returns its length.

The `array_filter` function creates a new array that
contains a subset of the elements in an existing array.
It takes an array and a callback.
The callback can be the string name of a function or function definition.
For example:

```php
$r_colors = array_filter($colors, function ($color) {
  return strpos($color, 'r') !== false;
})
```

The `array_map` function creates a new array that
contains the same number of elements as an existing array
that are computed from those element values.
It takes a callback, and any number of arrays.
Note that this is the opposite order of the `array_filter` arguments.
When multiple arrays are passed, the resulting array contains
an element corresponding to each element in each of the arrays.
The callback can be the string name of a function or function definition.
For example:

```php
$upper_colors = array_map("strtoupper", $colors);
```

The `array_reduce` function creates a single value from an array.
It takes an array and a callback.
The callback can be the string name of a function or function definition.
For example:

```php
$numbers = [1, 2, 3, 4];
$sum = array_reduce($numbers, function ($carry, $number) {
  return $carry + $number;
});
```

## Classes

A PHP class can define properties and methods.
For example:

```php
class Car {
  public $color;
  public $make;
  public $model;
  public $year;

  public function __construct($year, $make, $model, $color) {
    $this->color = $color;
    $this->make = $make;
    $this->model = $model;
    $this->year = $year;
  }

  public function toString() {
    return "$this->year $this->make $this->model in $this->color";
  }
}
```

TODO: What access specifiers besides `public` are supported?

Method parameter names must begin with `$` just like variable names.

Methods refer to the current object using `$this`
and refer to properties of that object using `->`.

## Objects

The `new` keyword is used to create an instance of a class.
For example:

```php
$myCar = new Car(2015, "MINI", "Cooper", "orange");
```

To access a public property of an object, use the `->` operator.
For example:

```php
echo 'color is ' . $myCar->color . '<br>';
```

To call a public method of an object, use the `->` operator.
For example:

```php
echo $myCar->toString();
```

## Conditional Logic

PHP supports the same syntax for `if` and `switch` statements as JavaScript.

## Iteration

PHP supports the same syntax for `for` and `while` loops as JavaScript.

## Functions

## Query Parameters

Query parameters appear at the end of URLs after the `?` character.
They are name/value pairs where each pair is separated by a `&` character
and each name and value are separated by an `=` character.
For example, `?make=MINI&model=Cooper&year=2015`.

The builtin PHP function `isset` takes the name of a query parameter
and returns a boolean indicating if it is set.

To get the value of a query parameter, use `$_GET['digit']`.

## Calculator Example

<img alt="PHP Calculator" class="keep-size" src="/blog/assets/php-calculator.png">

```php
<!DOCTYPE html>
<html>
  <head>
    <title>PHP Hello World</title>
    <style>
      button {
        border: 1px solid #575B5A;
        color: white;
        cursor: pointer;
        font-size: 2rem;
        height: var(--size);
        outline: none;
        text-align: center;
        width: var(--size);
      }

      button:active {
        background-color: darkgray !important;
      }

      button[name='digit'] {
        background-color: #808282;
      }

      button[name='operator'] {
        background-color: #F0A33B;
      }

      button[name='special'] {
        background-color: #676A6A;
      }

      .double {
        width: calc(var(--size) * 2);
      }

      form {
        --size: 4rem;
        font-family: sans-serif;
      }

      .result {
        background-color: #545857;
        box-sizing: border-box;
        color: white;
        font-size: 3rem;
        padding: 0 0.5rem;
        text-align: right;
        width: calc(var(--size) * 4);
      }

      .row {
        display: flex;
        align-items: center;
      }
    </style>
  </head>
  <body>
    <?php
    $digit = $_GET['digit'];
    $lastOperator = $_GET['lastOperator'];
    $operator = $_GET['operator'];
    $special = $_GET['special'];
    $n1 = $_GET['n1'];
    $n2 = $_GET['n2'];

    $display = 0;

    $operateOnFirst = !$lastOperator || $lastOperator === '=';

    if (isset($digit)) {
      if ($operateOnFirst) {
        if ($lastOperator === '=') {
          $n1 = $digit;
          $lastOperator = '';
        } else {
          $n1 = $n1 . $digit;
        }
        $display = $n1;
      } else {
        $n2 = $n2 . $digit;
        $display = $n2;
      }
    } elseif (isset($special)) {
      if ($special === 'clear') {
        $n1 = '';
        $n2 = '';
        $lastOperator = '';
      } elseif ($special === '%') {
        if ($operateOnFirst) {
          $n1 /= 100;
          $display = $n1;
        } else {
          $n2 /= 100;
          $display = $n2;
        }
      } elseif ($special === 'sign') {
        if ($operateOnFirst) {
          $n1 = -$n1;
          $display = $n1;
        } else {
          $n2 = -$n2;
          $display = $n2;
        }
      }
    } elseif (isset($operator)) {
      if ($operator === '=') {
        if ($lastOperator === '+') {
          $display = $n1 + $n2;
        } elseif ($lastOperator === '-') {
          $display = $n1 - $n2;
        } elseif ($lastOperator === '*') {
          $display = $n1 * $n2;
        } elseif ($lastOperator === '/') {
          $display = $n1 / $n2;
        }
        $n1 = $display;
        $n2 = '';
        $display = is_int($display) ? $display : number_format($display, 4);
      } else {
        $display = $n1;
      }
      $lastOperator = $operator;
    }
    ?>
    <form>
      <div class="result">
        <?= $display ?>
      </div>
      <div class="row">
        <button name="special" value="clear">AC</button>
        <button name="special" value="sign">+/-</button>
        <button name="special" value="%">％</button>
        <button name="operator" value="/">÷</button>
      </div>
      <div class="row">
        <button name="digit" value="7">7</button>
        <button name="digit" value="8">8</button>
        <button name="digit" value="9">9</button>
        <button name="operator" value="*">x</button>
      </div>
      <div class="row">
        <button name="digit" value="4">4</button>
        <button name="digit" value="5">5</button>
        <button name="digit" value="6">6</button>
        <button name="operator" value="-">-</button>
      </div>
      <div class="row">
        <button name="digit" value="1">1</button>
        <button name="digit" value="2">2</button>
        <button name="digit" value="3">3</button>
        <button name="operator" value="+">+</button>
      </div>
      <div class="row">
        <button class="double" name="digit" value="0">0</button>
        <button name="digit" value=".">.</button>
        <button name="operator" value="=">＝</button>
      </div>
      <input type="hidden" name="n1" value="<?= $n1 ?>">
      <input type="hidden" name="n2" value="<?= $n2 ?>">
      <input type="hidden" name="lastOperator" value="<?= $lastOperator ?>">
    <form>
  </body>
</html>
```
