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

1. Enter `gradle build`

1. Enter `gradle run`

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
