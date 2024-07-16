---
eleventyNavigation:
  key: Streams
  order: 30
  parent: Smalltalk
layout: topic-layout.njk
---

TODO: Add information about working with streams
that are not necessarily associated with a file.

## File I/O

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
