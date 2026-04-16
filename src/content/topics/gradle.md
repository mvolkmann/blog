---
eleventyNavigation:
  key: Gradle
layout: topic-layout.njk
---

## Overview

<a href="https://gradle.org" target="_blank">Gradle</a>
is a build tool for Java.

## Example Project

The following steps create a simple Java project that can be built using Gradle.

1. Install <a href="https://sdkman.io" target="_blank">SDKMAN!</a>.

1. Install Gradle by entering `sdk install gradle 8.8`

1. Create a directory for the project.

1. Create the file `build.gradle`, which uses Groovy,
   in the project directory containing the following:

   ```groovy
   plugins {
      id 'application'
      id 'java'
   }

   application {
      // applicationDefaultJvmArgs = ['-Dgreeting.language=en']
      mainClass = 'HelloWorld'
   }

   java {
      sourceCompatibility = JavaVersion.VERSION_18
   }

   repositories {
      mavenCentral()
   }

   dependencies {
      // Add dependencies here if needed
   }
   ```

1. Inside the project directory, create the diretory `src/main/java`.

1. Create the file `src/main/java/HelloWorld.java` containing the following:

   ```java
   public class HelloWorld {
       public static void main(String[] args) {
           System.out.println("Hello, World!");
       }
   }
   ```

1. To build the project, enter `gradle build`

   This creates the `build` subdirectory if it doesn't exist
   and it writes all generated files inside that directory.

1. To run the project, enter `gradle run`

   This will also build the project before running
   if it has not been built or
   if source files have been modified since the last build.
   So it is never necessary to run `gradle build`.

1. To delete all the generated files (the `build` directory),
   enter `gradle clean`.

## Kotlin

The following file `build.gradle.kts` is nearly identical to the `build.gradle` file above, but it uses Kotlin instead of Groovy.

```kotlin
plugins {
    // The "java" and "application" plugins are special.
    // All other plugins must use the syntax `id("plugin.id")`.
    id 'application'
    id 'java'
}

application {
    applicationDefaultJvmArgs = ['-Dgreeting.language=en']
    mainClass = 'HelloWorld'
}

java {
    sourceCompatibility = JavaVersion.VERSION_18
}

repositories {
    mavenCentral()
}

dependencies {
    // Add dependencies here if needed
}
```
