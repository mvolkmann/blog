---
eleventyNavigation:
  key: Zod
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<img alt="Zod logo" style="border: none; width: 20%"
  src="/blog/assets/zod-logo.svg?v={{pkg.version}}"
  title="Hono logo">

## Overview

{% aTargetBlank "https://zod.dev", "Zod" %} is a TypeScript library
for validating JavaScript values.

The rationale for the name "Zod" is not given in its documentation,
but some definitions include:

- one of Superman's greatest archenemies (General Zod)
- someone who is very foolish, awkward, or socially inept
- any repellent thing or person
- a studious person

The following sections provide an overview
of specifying validation constraints using Zod.
For more detail, see the Zod {% aTargetBlank "https://zod.dev", "home page" %}.

## Primitive Types

Primitive types are specified with the syntax `z.{type}()`.
They include:

- `bigint`, `boolean`, `date`, `number`, `string`, `symbol`
- `null`, `undefined`, `void`, `never`
- `any`, `unknown`

These types are all functions that are typically called with no arguments.
Some of them take an optional configuration argument.
Rather that calling these functions repeatedly,
they can be called once to store the return value in a variable
and the variable can be used multiple times.

## Strings

Many validation methods can be applied to the `string` type.
Highlights include the following:

- `.datetime()`
- `.email()`
- `.emoji()`
- `.endsWith(string)`
- `.includes(string)`
- `.length(number)`
- `.max(number)`
- `.min(number)`
- `.regex(regex)`
- `.startsWith(string)`
- `.url()`

For example, the type `z.string().min(3).max(10).endsWith("X")`
matches string values with a length of at least 3, not more than 10,
and ending with "X".

## Numbers

Many validation methods can be applied to the `number` type.
Highlights include the following:

- `.int()` - not floating point
- `.gt(number)`
- `.gte(number)` - alias .min(number)
- `.lt(number)`
- `.lte(number);` - alias .max(number)
- `.positive()` - greater than 0
- `.nonnegative()` - greater than or equal to 0
- `.negative()` - less than 0
- `.nonpositive()` - less than or equal to 0

All validation methods take an optional final argument that
specifies the error message to display when the validation fails.
For example, `z.number().max(10, { message: "cannot exceed 10" })`

## Enumerations

The `enum` type specifies a set of allowed string values.
For example, `z.enum(["red", "green", "blue"])`

To validate against the values in a TypeScript `enum`,
use the `nativeEnum` type. For example:

```ts
enum Color { red, green, blue }
...
z.nativeEnum(Color)
```

## Optional Values

All types specify a required value unless the `optional` method is used.
For example: `z.optional(z.string())` or `z.string().optional()`.

## Objects

Object types are specified with the `object` method.
For example:

```ts
const DogSchema = z.object({
  name: z.string(),
  breed: z.string(),
  age: z.number().optional()
});
```

## Arrays

Array types are specified with the `array` function.
For example, the following both specify an array of integers:

```ts
z.array(z.number().int());
z.number().int().array();
```

## Infering TypeScript Types

To generate a TypeScript type from a Zod schema object,
use `z.infer`. For example:

```ts
type Dog = z.infer<typeof DogSchema>;
```
