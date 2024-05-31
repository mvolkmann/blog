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
1. Open a terminal.
1. `cd` to the directory where a new project will be created.
1. Create a directory for a new project and `cd` to it.
1. `gradle init`.
1. For the type of build to generate, select "1. Application".
1. For the implementation language, select "1. Java".
1. For the target Java version,
   press enter to accept the default version of Java.
1. For the project name, press enter to accept the default project name
   which matches the directory name.
1. For the application structure, select "1. Single appplication project".
1. For the build script DSL, select "1. Kotlin".
1. For the test framework, select "1. JUnit 4".
1. For "Generate build using new APIs and behavior", enter "yes".
1. Open the project in your editor of choice.
1. Open the file `app/src/main/java/org/example/App.java`.
1. Replace the contents of this file with the following:

   ```java
    package org.example;

    import org.springframework.boot.autoconfigure.SpringBootApplication;

    @SpringBootApplication
    public class App {
        public static void main(String[] args) {
            SpringApplication.run(Main.class, args);
        }
    }
   ```
