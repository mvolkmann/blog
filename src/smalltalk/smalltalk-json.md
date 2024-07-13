---
eleventyNavigation:
  key: JSON
  order: 1.3
  parent: Smalltalk
layout: topic-layout.njk
---

The `JSON` package defines the classes `Json`, `JsonObject`, and `JsonError`.
It also adds the method `jsonWriteOn:` to many classes including
`Array2D`, `Association`, `CharacterSequence`, `Collection`, `Dictionary`,
`False`, `Integer`, `Number`, `Text`, `True`, and `UndefinedObject`.

To install the `JSON` package,
enter `Feature require: 'JSON'` in a Workspace and "Do it".

Custom classes whose instances need to be serialized to JSON
should implement the instance method `jsonWriteOn:`.
This method describes the instance variables that should be included.
Here's how it could be implemented for a `Dog` class
with instance varaibles `id`, `name`, and `breed`.

```smalltalk
jsonWriteOn: aWriteStream
    {
        #id->id.
        #name->name.
        #breed->breed
    } asDictionary jsonWriteOn: aWriteStream
```

Here's how we can get a JSON string for a `Dog` object.

```smalltalk
json := Json render: dog
```

Custom classes that need to deserialize JSON to a new instance
should implement the class method `fromJson:` (suggested name)
that takes a stream of JSON data and returns a new object created from it.
Here's an example that uses the `Dog` class method `id:name:breed:`
to create a new instance.

```smalltalk
fromJson: aStream
    | jsonObject |
    jsonObject := Json readFrom: aStream.
    ^Dog
        id: (jsonObject at: #id)
        name: (jsonObject at: #name)
        breed: (jsonObject at: #breed)
```

Here's how we can parse a JSON string to get a `Dog` object.

```smalltalk
newDog := Dog fromJson: json readStream
```
