---
eleventyNavigation:
  key: AWS Lambda
layout: topic-layout.njk
---

The steps to create a Java-based AWS Lambda function are:

- In IDEA, create a new Java Gradle project
  that uses Kotlin for the Gradle file.

- In the file `build.gradle.kts`:

  - Add the following dependencies:

    ```kotlin
    implementation("com.amazonaws:aws-lambda-java-core:1.2.1")
    implementation("com.amazonaws:aws-lambda-java-events:3.11.0")
    ```

  - Add the following task definition:

    ```kotlin
    tasks.jar {
        manifest {
            attributes["Main-Class"] = "org.example.HelloLambda"
        }
    }
    ```

- Create the file `src/main/java/org/example/HelloLambda.java`
  containing the following:

  ```java
  package org.example;

  import com.amazonaws.services.lambda.runtime.Context;
  import com.amazonaws.services.lambda.runtime.RequestHandler;

  import java.util.Map;

  public class HelloLambda implements RequestHandler<Map<String, Object>, String> {
      @Override
      public String handleRequest(Map<String, Object> input, Context context) {
          return "Hello, " + input.get("name") + "!";
      }
  }
  ```

- In a terminal, enter `./gradlew jar`.
  This will create the file `build/libs/AWSLambdaDemo-1.0-SNAPSHOT.jar`.

- Browse https://aws.amazon.com.
- Click the "Sign In to the Console" button in the upper-right.
- If you do not see "Console Home" and a "Lambda" link ...
  - Click the waffle button in the upper-left.
  - Click "Compute" in the left nav.
- Click "Lambda"
- Click the "Create a function" button.
- Select the "Author from scratch" radio button (selected by default).
- Enter a value for "Function name".
- Select a "Runtime" Java version such as "Java 21".
- Select an "Architecture" such as "x86_64" (selected by default).
- Click the "Create function" button at the bottom.
- Click the "Upload from" dropdown and select ".zip or .jar file".
- Click the "Upload" button.
- Select the `.zip` file created above and click the "Upload" button.
- Click the "Save" button.
- In the "Runtime settings" section, click the "Edit" button.
- Change the "Handler" string to "AwsLambdaFunctions::alterPayload".
- Click the "Save" button.
- Click the "Test" tab.
- Enter an "Event name" like "test".
- Click the "Save" tab.
- Click the "Test" tab.
