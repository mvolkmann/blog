---
eleventyNavigation:
  key: PHP
layout: topic-layout.njk
---

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
