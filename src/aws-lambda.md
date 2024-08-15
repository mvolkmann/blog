---
eleventyNavigation:
  key: AWS Lambda
layout: topic-layout.njk
---

The steps to create a Java-based AWS Lambda function are:

- Browse https://aws.amazon.com.
- Click the "Sign In to the Console" button in the upper-right.
- Click the waffle button in the upper-left.
- Click "Compute" in the left nav.
- Click "Lambda"
- Click the "Create a function" button.
- Select the "Author from scratch" radio button.
- Enter a value for "Function name".
- Select a "Runtime" Java version such as "Java 21".
- Select an "Architecture" such as "x86_64".
- Click the "Create function" button at the bottom.
- Create a local `.java` file that defines the function. For example:

  ```java
  package demo;

  public class AwsLambdaFunctions {
      public String alterPayload(String payload) {
          System.out.println("We are in the Java AWS Lambda function!");
          return payload.toLowerCase().replace('e', '3').replace('l', '1');
      }
  }
  ```

- Create a `.zip` file containing the `.java` file.
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
