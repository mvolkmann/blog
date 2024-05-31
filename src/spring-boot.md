---
eleventyNavigation:
  key: Spring Boot
layout: topic-layout.njk
---

<figure style="width: 30%">
  <img alt="Spring logo"
    src="/blog/assets/spring-logo.png?v={{pkg.version}}">
</figure>

## Overview

<a href="https://spring.io" target="_blank">Spring</a>
is a set of Java services that aim to simplify some common tasks.
Spring Boot "makes it easy to create stand-alone,
production-grade Spring based Applications".

## VS Code

When using VS Code for Java development:

- Install the extension "Extension Pack For Java" from Microsoft.
- If using Spring Boot, install the "Spring Boot Extension Pack" from VMware.

## Steps

1. Install the latest version of the Java Development Kit (JDK).
1. Install the Gradle build tool.
   In macOS, this can be done with `brew install gradle`.
1. Browse the <a href="https://start.spring.io"
   target="_blank">spring initializr</a>.
1. For Project, select "Gradle - Kotlin".
1. For Language, select "Java".
1. For "Spring Boot", select "3.3.0".
1. For "Packaging", select "Jar".
1. For "Java", select "21".
1. Click the "ADD DEPENDENCIES..." button.
1. Unde the "WEB" category, click "Spring Web".
1. Click the "GENERATE" button which
   creates the file `demo.zip` in the `Downloads' directory.
1. Unzip that file and move the resulting directory to the desired location.
1. Open a terminal and cd to the directory of the new project.
1. Enter `./gradlew build`
1. Open the project in your editor of choice.
1. Create the file `` containing the following:

   ```java
   package com.example.demo;

   import org.springframework.web.bind.annotation.GetMapping;
   import org.springframework.web.bind.annotation.RestController;

   @RestController
   public class Controller {

       @GetMapping("/api/test")
       public String test() {
           return "test";
       }
   }
   ```

1. Ensure that port 8080 is not already in use.
1. Enter `./gradlew bootRun`
1. Browse `localhost:8080/api/test` and verify that "test" is displayed.
