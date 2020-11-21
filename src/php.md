---
eleventyNavigation:
  key: PHP
layout: topic-layout.njk
---

PHP is an acronym for "PHP Hypertext Processor".
PHP must be run in an HTTP server.
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
    $digit = $_GET["digit"];
    $lastOperator = $_GET["lastOperator"];
    $operator = $_GET["operator"];
    $special = $_GET["special"];
    $n1 = $_GET["n1"];
    $n2 = $_GET["n2"];

    $display = 0;

    $operateOnFirst = !$lastOperator || $lastOperator === "=";

    if (isset($digit)) {
        if ($operateOnFirst) {
            if ($lastOperator === "=") {
                $n1 = $digit;
                $lastOperator = "";
            } else {
                $n1 = $n1 . $digit;
            }
            $display = $n1;
        } else {
            $n2 = $n2 . $digit;
            $display = $n2;
        }
    } elseif (isset($special)) {
        if ($special === "clear") {
            $n1 = "";
            $n2 = "";
            $lastOperator = "";
        } elseif ($special === "%") {
            if ($operateOnFirst) {
                $n1 /= 100;
                $display = $n1;
            } else {
                $n2 /= 100;
                $display = $n2;
            }
        } elseif ($special === "sign") {
            if ($operateOnFirst) {
                $n1 = -$n1;
                $display = $n1;
            } else {
                $n2 = -$n2;
                $display = $n2;
            }
        }
    } elseif (isset($operator)) {
        if ($operator === "=") {
            if ($lastOperator === "+") {
                $display = $n1 + $n2;
            } elseif ($lastOperator === "-") {
                $display = $n1 - $n2;
            } elseif ($lastOperator === "*") {
                $display = $n1 * $n2;
            } elseif ($lastOperator === "/") {
                $display = $n1 / $n2;
            }
            $n1 = $display;
            $n2 = "";
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
