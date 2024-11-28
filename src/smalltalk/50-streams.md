---
eleventyNavigation:
  key: Streams
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

Streams provide a way to iterate over many kinds of collections and resources.
They also provide a way to write data.

The class `SequenceableCollection` which is a superclass of `Array`
provides the methods `readStream`, `readStreamFrom:to:`, and `writeStream`
that each return a stream.

The `readStream` method of `SequenceableCollection` returns a `ReadStream`
which is a subclass of `Positionable` stream.
It has a `position` instance variable
that can be retrieved by sending `#position`
and modified by sending `#position:`.
It also has a `readLimit` instance variable
that is intialized to the collection size.

## PositionableStream

The `PositionableStream` class method `on:`
takes a `Collection` and returns a stream over it.
The `ReadStream` class method `on:from:to:`
takes a `Collection` and two indexes, and
returns a stream over that range of elements.

The following table describes commonly used methods
in the `PositionableStream` class.

| Method          | Description                                                                 |
| --------------- | --------------------------------------------------------------------------- |
| `atEnd`         | answers true if `position` >= `readLimit`                                   |
| `atStart`       | answers true if `position` is zero                                          |
| `back`          | subtract 1 from `position` and answers that element                         |
| `contents`      | answers copy of collection                                                  |
| `isEmpty`       | answers true if `atEnd` and `position` is zero                              |
| `next:`         | anwsers collection of next argument elements, advancing `position` for each |
| `notEmpty`      | answers true if not `isEmpty`                                               |
| `peek`          | answers element at `position`                                               |
| `peekBack`      | answers element at `position` minus 1                                       |
| `position`      | answers the current value of `position`                                     |
| `position:`     | sets `position` to argument if not greater than `readLimit`                 |
| `reset`         | sets `position` to zero                                                     |
| `resetContents` | sets `position` and `readLimit` to zero                                     |
| `setToEnd`      | sets `position` to `readLimit`                                              |
| `skip`          | adds 1 to `position`                                                        |
| `skip:`         | adds argument to `position`                                                 |
| `skipBack`      | subtracts 1 from `position`                                                 |
| `skipTo:`       | advances position to element matching argument and answers whether found    |

## ReadStream

The following table describes commonly used methods
in the `ReadStream` class.

| Method | Description                                   |
| ------ | --------------------------------------------- |
| `next` | adds 1 to `position` and answers that element |
| `size` | answers value of `readLimit`                  |

The following code demonstrates using many of the methods described above.
The `logAs:` method is defined in the "Getting Started" section "Transcripts".
When the end is reached, the `next` method returns `nil`.

```smalltalk
arr := #('apple' 'banana' 'cherry').
stream := arr readStream.
stream position logAs: 'position'. "0"
stream size logAs: 'readLimit'. "3"
stream atStart logAs: 'atStart'. "true"
stream atEnd logAs: 'atEnd'. "false"
stream next print. "position -> 1; apple"
stream peek print. "position stays 1; banana"
stream peekBack print. "position stays 1; apple"
stream next print. "position -> 2; banana"
stream reset. "position -> 0"
stream next print. "position -> 1; apple"
stream position: 2.
stream next print. "position -> 3; cherry"
stream back. "position -> 2"
stream peekBack print. "banana"
stream peek print. "cherry"
stream atStart logAs: 'atStart'. "false"
stream atEnd logAs: 'atEnd'. "true"
```

The `readStreamFrom:to:` method returns a stream that can
read the elements in a range of the collection.

```smalltalk
arr := #('apple' 'banana' 'cherry' 'grape').
stream := arr readStreamFrom: 2 to: 3.
stream next print. "banana"
stream next print. "cherry"
stream next print. "nil"
```

## WriteStream

The `writeStream` method returns a `WriteStream` that can modify the collection.
It adds the instance variable `writeLimit`
which is initialized to the collection size.

TODO: So far I can only get a `WriteStream` to modify existing elements,
not add new ones. For example, this does not work:

```smalltalk
coll := OrderedCollection newFrom: #('apple' 'banana' 'cherry').
stream := coll writeStream.
stream pastEndPut: 'grape'.
coll print.
```

## ReadWriteStream

The `ReadWriteStream` class creates a string that can
read and write (modify) collection elements.

```smalltalk
coll := OrderedCollection newFrom: #('apple' 'banana' 'cherry').
stream := ReadWriteStream with: coll. "position is 3"
stream reset. "position is 0"
stream next print. "changes position to 1; outputs apple"
stream nextPut: 'grape'. "changes position to 2; changes element to grape"
coll print. "apple grape cherry"
```

## Creating Strings

One use of streams is to efficiently build a `String`.
For example:

```smalltalk
stream := WriteStream on: (String new: 100).
stream nextPutAll: 'Hello'; nextPutAll: ', World'; nextPut: $!; newLine.
string := stream contents. "Hello, World!\n"
```

`100` above is just a size estimate.
It does not affect the size of the final `String`.
More than 100 characters can be added,
but doing that will be slightly less efficient
because additional space will need to be allocated
during execution of the `nextPut*` methods.

If the purpose of building a `String` is to write it to the Transcript,
it is better to just use methods in the `Transcript` class.
That class implements many of the same methods as the `WriteStream` class.
The example above can be written as follows to write to the Transcript.

```smalltalk
Transcript nextPutAll: 'Hello'; nextPutAll: ', World'; nextPut: $!; cr.
```

## File I/O

### Files

To create a `FileEntry` object, send the `#asFileEntry` message
to a string that contains the file name.
The file is assumed to be in the `\*-UserFiles` directory
(ex. `Cuis-Smalltalk-Dev-UserFiles`).
To see this, enter `DirectoryEntry currentDirectory` in a Workspace
and "Print it".

For example:

```smalltalk
fileEntry := 'demo.txt' asFileEntry.
```

To write to a text file, overwriting any previous contents:

```smalltalk
fileEntry forceWriteStreamDo: [:fileStream |
    fileStream nextPutAll: 'line #1'.
    fileStream newLine.
    fileStream nextPutAll: 'line #2'
].
```

Another way to obtain a stream for writing and reading a file at a given path
is the following:

```smalltalk
stream := FileStream fileNamed: 'some-file-path'
```

To read the entire contents of a text file into a string:

```smalltalk
contents := fileEntry fileContents.
```

To read lines in a text file one at a time:

```smalltalk
fileEntry := 'demo.txt' asFileEntry.
stream := fileEntry readStream.
line := stream nextLine "do repeatedly to get each line"
```

To write serialized objects to a file:

```smalltalk
fileEntry writeStreamDo: [:fileStream |
    | refStream |
    refStream := ReferenceStream on: fileStream.
    refStream nextPut: true.
    refStream nextPut: 3.
    refStream nextPut: 'text'.
    refStream nextPut: #(1 2 3).
].
```

TODO: Do ReferenceStreams support circular object references?

To read serialized objects from a file:

```smalltalk
fileEntry readStreamDo: [:fileStream |
    | object refStream |
    refStream := ReferenceStream on: fileStream.
    [refStream atEnd] whileFalse: [
        object := refStream next.
        object print "writes to Transcript"
    ]
]
```

To delete a file:

```smalltalk
fileEntry delete.
```

TODO: Harvest more information about file system operations from this video.
<a href="https://youtu.be/stMoWMlLVzk?si=_3rmJFPkZ2g4ZIIV"
target="_blank">squeak smalltalk tutorial: file handling part 1</a>.

### Directories

The following instance methods of the `DirectoryEntry` class
iterate over its files and subdirectories:

| Method              | Description                                                                 |
| ------------------- | --------------------------------------------------------------------------- | --- |
| `allChildrenDo:`    | iterates over all subdirectories and files recursively                      |     |
| `allDirectoriesDo:` | iterates over all subdirectories recursively                                |
| `allFilesDo:`       | iterates over all files in this directory and in subdirectories recursively |
| `directoriesDo:`    | iteraters over all subdirectories non-recursively                           |
| `filesDo:`          | iterates over all files in this directory                                   |
| `childrenDo:`       | iterates over all subdirectories and files non-recursively                  |     |

The following code iterates over all the files
found in and below a given directory:

```smalltalk
de := DirectoryEntry smalltalkImageDirectory.
de allFilesDo: [:file | file name print]
```
