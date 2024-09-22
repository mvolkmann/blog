---
eleventyNavigation:
  key: Streams
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

Streams provide a way to iterate over many kinds of collections and resources.

The class `SequenceableCollection` which is a superclass of `Array`
provides the methods `readStream`, `readStreamFrom:to:`, and `writeStream`
that each return a stream.

The `readStream` method returns a stream that can be used to
read every element of the collection.

```smalltalk
arr := #('apple' 'banana' 'cherry' 'grape').
stream := arr readStream.
stream next print. "apple"
(stream next: 2) print. "banana cherry"
stream next print. "grape"
stream next print. "nil"
stream next print. "nil"
```

The `readStreamFrom:to:` method returns a stream that can be used to
read the elements in a range of the collection.

```smalltalk
arr := #('apple' 'banana' 'cherry' 'grape').
stream := arr readStreamFrom: 2 to: 3.
stream next print. "banana"
stream next print. "cherry"
stream next print. "nil"
```

The `writeStream` method returns a stream that can be used to

## File I/O

### Files

To create a file, send the `#asFileEntry` message
to a string that contains the file name.
The file is assumed to be in the `\*-UserFiles` directory.
For example:

```smalltalk
fileEntry := 'demo.txt' asFileEntry.
```

To write to a text file, overwriting any previous contents:

```smalltalk
fileEntry forceWriteStreamDo: [ :fileStream |
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

To write serialized objects to a file:

```smalltalk
fileEntry writeStreamDo: [ :fileStream |
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
fileEntry readStreamDo: [ :fileStream |
    | object refStream |
    refStream := ReferenceStream on: fileStream.
    [ refStream atEnd ] whileFalse: [
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
