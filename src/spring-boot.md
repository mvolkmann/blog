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
   When using <a href="https://sdkman.io" target="_blank">SDKMAN</a>,
   change the version of Java used with `sdk use java 21.0.3-tem`.
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
1. Create the file `src/main/java/com/example/demo/DogController.java`
   containing the following which
   implements endpoints for all the CRUD operations:

   ```java
   package com.example.demo;

   import java.util.Collection;
   import java.util.HashMap;
   import java.util.UUID;
   import org.springframework.http.HttpStatus;
   import org.springframework.http.ResponseEntity;
   import org.springframework.web.bind.annotation.\*;

   @RestController
   public class DogController {

       HashMap<UUID, Dog> dogMap = new HashMap<>();

       public DogController() {
           addDog("Whippet", "Comet");
           addDog("German Shorthaired Pointer", "Oscar");
       }

       private Dog addDog(String breed, String name) {
           Dog dog = new Dog(breed, name);
           dogMap.put(dog.id, dog);
           return dog;
       }

       @GetMapping("/api/dog")
       public Dog[] getDogs() {
           System.out.println("in getDogs");
           Collection<Dog> dogCollection = dogMap.values();
           Dog[] dogArray = dogCollection.toArray(new Dog[0]);
           return dogArray;
       }

       @PostMapping("/api/dog")
       public Dog createDog(@RequestBody Dog dog) {
           return addDog(dog.breed, dog.name);
       }

       @PutMapping("/api/dog/{dogId}")
       public Dog updateDog(@PathVariable String dogId, @RequestBody Dog dog) {
           Dog existingDog = dogMap.get(UUID.fromString(dogId));
           existingDog.breed = dog.breed;
           existingDog.name = dog.name;
           return existingDog;
       }

       @DeleteMapping("/api/dog/{dogId}")
       public ResponseEntity<Void> deleteDog(@PathVariable String dogId) {
           Dog dog = dogMap.remove(UUID.fromString(dogId));
           return new ResponseEntity<>(null, dog == null ? HttpStatus.NOT_FOUND : HttpStatus.OK);
       }
   }
   ```

1. Specify the port (defaults to 8080) to be used in
   `src/main/resources/application.properties` by adding the following line:

   ```text
   server.port=1919
   ```

1. Ensure that port to be used is not already in use.
1. Enter `./gradlew bootRun`
1. Browse `localhost:8080/api/test` and verify that "test" is displayed.
