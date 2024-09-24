---
eleventyNavigation:
  key: Blocks
  order: 11
  parent: Smalltalk
layout: topic-layout.njk
---

A block is closure (anonymous function) that can have parameters
and contain many expressions.
They are instances of the class `BlockClosure`.
Their execution is deferred until they are sent a message like `value`.

The value of the block is the value of its last expression.
If a block uses the caret operator (`^`) to return a value,
the containing method will exit and return that value.
This is referred to as a "non-local return".
Few programming languages support this.
Languages that do include Common Lisp, Prolog,
Ruby, Scala, Scheme, and Smalltalk.

Blocks take zero or more positional arguments,
which is something methods cannot do.
Parameter names appear at the beginning of a block
and each name is preceded by a colon.
(This syntax was most likely chosen to simplify parsing because it
enables determining whether a block has any parameters without backtracking.)
The parameter list is separated from the expressions by a vertical bar.

Blocks can be saved in variables,
passed as arguments to methods and other blocks,
returned from methods and other blocks,
and can be evaluated multiple times. For example:

```smalltalk
noArgBlock := [2 * 3].
singleArgBlock := [:a | a * 3].
multipleArgBlock := [:a :b | a * b].
```

Blocks support several messages that evaluate the block
whose names begin with `value`.
These messages enable providing zero to four arguments.
For blocks with more than four parameters, send the message
`#valueWithArguments:` and an array holding all the arguments.
For example:

```smalltalk
noArgBlock value.
singleArgBlock value: 1.
multiArgBlock value: 1 value: 2.
multiArgBlock value: 1 value: 2 value: 3.
multiArgBlock value: 1 value: 2 value: 3 value: 4.
multiArgBlock valueWithArguments: #(1 2 3 4 5).
```

A block must be passed the same number of arguments as it has parameters.
If a block is passed fewer or more arguments than it accepts,
a Debug window will open.

Blocks can declare and use temporary (local) variables just like a method.

For example:

```smalltalk
average := [:a :b |
    | sum |
    sum := a + b.
    sum / 2.0
]
```

Blocks are closures, meaning that they can
access in-scope variables defined outside them.
For example:

```smalltalk
n := 19.
b := [:a | a + n].
b value: 2 "result is 21"
```

To use a block as an iteration condition,
use the methods `whileTrue:`, `whileFalse:`, `whileNotNil:`, and `whileNil:`
that are defined in the `BlockClosure` class.
Note that these are not methods in the `Boolean` class.

For example, the following code prints the integer values 1 through 10
in the Transcript:

```smalltalk
| counter |
counter := 1.
[counter <= 10] whileTrue: [
    counter print.
    counter := counter + 1.
].
```

A block can call itself if it passes itself in as an argument.
For example:

```smalltalk
fact := [:block :n |
    n = 1
        ifTrue: 1
        ifFalse: [n * (block value: block value: n - 1)]
].

fact value: fact value: 5 "gives 120"
```

## Running in another Process

The Squeak VM is single-threaded, like JavaScript.
Processes do not run in separate threads
and instead use cooperative multitasking.

Some Smalltalk implementations also support execute code in a new thread.
The Squeak VM does not support this.

Processes yield control to other processes by
sending certain messages to the `Delay`, performing I/O,
or performing another blocking operation.

The order in which processes execute is affected by their priority.
The default priority is 40, the lowest is 10, and the highest is 80.
The `priority:` method is used to set the priority of a `Process`
to a value other than the default.
This method ensures that the value actually used is
in the range of the lowest to the highest allowed.

The following table summaries the instance methods in the `BlockClosure` class
that execute a block in a new process.

| Method            | Description                                                                          |
| ----------------- | ------------------------------------------------------------------------------------ |
| `fork`            | creates new process that executes block at default priority                          |
| `forkNamed:`      | creates new process with given name that executes block at default priority          |
| `forkAt:`         | creates new process that executes block at given priority                            |
| `forkAt:named:`   | creates new process with given name that executes block at given priority            |
| `forkAndWait`     | creates new process that executes block and blocks current process until it finishes |
| `newProcess`      | creates new process that will execute block when `#resume` message is sent to it     |
| `newProcessWith:` | creates new process that will execute block with arguments in array argument         |

All the methods above must be used with a block that takes no arguments
except `newProcessWith:`.

When a new process is not given a name, it defaults to "unnamed".

To see long-running processes and optionally terminate them,
open the World menu and select Open ... Process Browser.
To terminate a process, select it and press cmd-t
(or right-click and select "Terminate").

To give a name to the process returned by `newProcess` or `newProcessWith:`,
send the message `#name:` to it with a `String` argument.

The following code demonstrates using each of the methods described above.

```smalltalk
"This prints one, three, and two."
'one' print.
['two' print] fork.
'three' print.

"This prints one, two, and three."
'one' print.
['two' print] forkAndWait.
'three' print.

process := [:a :b |
    (Delay forSeconds: 4) wait.
    (a + b) print. "prints 5"
] newProcessWith: #(2 3).
process name: 'addition'.
process resume.
```

## Partial Application

Partial application is the ability to pass a subset
of the arguments required by a function to the function
and get a new function that takes the remaining arguments.

Currying is similar, but only passes a single argument to the function.

The Smalltalk `BlockClosure` class supports a limited form of currying,
handling up to four arguments.
But it does not support partial application.

The following code demonstrates using currying with the `BlockClosure` class:

```smalltalk
add2Numbers := [:a :b | a + b].
add2Numbers value: 2 value: 3. "5"
block := add2Numbers withFirstArg: 2.
block argumentCount. "1"
block value: 3. "5"

add3Numbers := [:a :b :c | a + b + c].
block1 := add3Numbers withFirstArg: 2.
block2 := block1 withFirstArg: 3.
"The withFirstArg: method always returns a block.
 Send it the #value message to get the final value."
(block2 withFirstArg: 4) value.
```

We can implement unlimited currying and partial application.
The `PartialBlock` class defined below does this.

Here are examples of using it:

```smalltalk
pb := PartialBlock block: [:a :b | a + b].
"All arguments supplied."
pb valueWithArguments: #(2 3). "5"

"Single argument supplied".
pb2 := pb value: 2. "a new PartialBlock"
pb2 value: 3. "5"

pb := PartialBlock block: [:a :b :c | a + b + c].

"Partial arguments supplied."
pb3 := pb valueWithArguments: #(2 3). "a new PartialBlock"
"Remaining arguments supplied."
pb3 valueWithArguments: #(4). "9"!
```

Here is the defintion of the `PartialBlock` class:

```smalltalk
Object subclass: #PartialBlock
    instanceVariableNames: 'arguments block'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'

"class method in private category"
block: aBlock arguments: anArray
    ^ self new
        setBlock: aBlock
        arguments: anArray

"class method in instance creation category"
block: aBlock
    ^ self new setBlock: aBlock

"instance method in private category"
setBlock: aBlock
    block := aBlock.
    arguments := #()

"instance method in private category"
setBlock: aBlock arguments: anArray
    block := aBlock.
    arguments := anArray

"instance method in evaluating category"
value: anObject
    ^ self valueWithArguments: { anObject }

"instance method in evaluating category"
valueWithArguments: anArray
    | args |
    (anArray isMemberOf: Array) ifFalse: [
        self error: 'valueWithArguments: requires an Array'
    ].
    args := arguments , anArray.
    args logAs: 'args'.
    anArray size = self argumentCount ifTrue: [
        ^ block valueWithArguments: args
    ].
    anArray size > self argumentCount ifTrue: [
        self error: 'too many arguments'
    ].
    ^ PartialBlock
        block: block
        arguments: args

"instance method in accessing category"
argumentCount
    ^ block argumentCount - arguments size
```
