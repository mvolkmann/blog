---
eleventyNavigation:
  key: Basic Data Types
  order: 20
  parent: Smalltalk
layout: topic-layout.njk
---

The following subsections provide a review of
commonly used Smalltalk classes that represent data types.

## UndefinedObject

The pseudo-variable `nil` represents not having a value.
It refers to the singleton instance of the `UndefinedObject` class.
Creation of additional instances is prevented by
overriding the class method `new` in the `UndefinedObject` class.

The following table describes some of the instance methods
defined in the `UndefinedObject` class.
These can be invoked on the pseudo-variable `nil`.

| Method            | Description                                              |
| ----------------- | -------------------------------------------------------- |
| `ifNil:`          | always evaluates its argument                            |
| `ifNil:ifNotNil:` | always evaluates its first argument and never its second |
| `ifNotNil:`       | never evaluates its argument                             |
| `ifNotNil:ifNil:` | always evaluates its second argument and never its first |
| `isEmptyOrNil:`   | always answers `true`                                    |
| `isLiteral`       | always answers `true`                                    |
| `isNil`           | always answers `true`                                    |
| `notNil`          | always answers `false`                                   |

## Boolean

The pseudo-variables `true` and `false` refer to
singleton instances of the classes `True` and `False`
which are subclasses of the class `Boolean`.

`True` and `False` are singleton classes.
Creating of additional instances is prevented by
overriding the class method `new` in the `Boolean` class.

Representing the values `true` and `false` by distinct classes
simplifies the implementation of many of their methods.
For example, here are the implementations of the
`&` and `ifTrue:` instance methods in the `True` class.

```smalltalk
"This is simplified because it can assume
 the receiver (left-hand side) is true."
& alternativeObject
    ^alternativeObject

"This is simplified by not needing to test whether the receiver is true."
ifTrue: alternativeBlock
    ^alternativeBlock value
```

The following table describes most of the instance methods
defined in the `Boolean` class.
Since `True` and `False` are subclasses, they also have these methods.

| Method             | Description                                                   |
| ------------------ | ------------------------------------------------------------- |
| `&`                | "and" without short-circuiting                                |
| `\|`               | "or" without short-circuiting                                 |
| `and:`             | "and" with short-circuiting                                   |
| `and:and:`         | like `and:` but for three values                              |
| `and:and:and:`     | like `and:` but for four values                               |
| `and:and:and:and:` | like `and:` but for five values                               |
| `eqv:`             | answers whether two `Boolean` values are equivalent           |
| `ifFalse:`         | evaluates argument (typically a block) if receiver is `false` |
| `ifFalse:ifTrue:`  | conditionally evaluates arguments (typically blocks)          |
| `ifTrue:`          | evaluates argument (typically a block) if receiver is `true`  |
| `ifTrue:ifFalse:`  | conditionally evaluates arguments (typically blocks)          |
| `isLiteral`        | always answers `true`                                         |
| `not`              | answers opposite Boolean value                                |
| `or:`              | "or" with short-circuiting                                    |
| `or:or:`           | like `or:` but for three values                               |
| `or:or:or:`        | like `or:` but for four values                                |
| `or:or:or:or:`     | like `or:` but for five values                                |
| `xor:`             | exclusive "or" of two `Boolean` values                        |

The `True` and `False` classes implement some of the methods described above,
but they do not add any methods.

Short-circuiting only works if the argument value is a block.
This can be demonstrated by creating the class `ShortCircuitDemo`
with the following instance methods:

```smalltalk
isHot
    'isHot entered' print.
    ^ true

isNight
    'isNight entered' print.
    ^ true

initialize
    (self isHot or: [self isNight])
        ifTrue: ['was true' print]
        ifFalse: ['was false' print]
```

To test this, enter `ShortCircuitDemo new` and "Do it".
It will print "isHot entered" and "was true" in the Transcript window.
It will not print "isNight entered" due to short-circuiting.
However, if the square brackets around "self isNight" are removed,
it will print "isHot entered", "isNight entered", and "was true".
This happens because "self isNight" will be
evaluated before the "or:" method is evaluated.

## Number

The following list depicts the class hierarchy for various kinds of numbers:

- `Number`
  - `Float`
    - `BoxedFloat64`
    - `SmallFloat64`
  - `Fraction`
    - `Integer`
      - `LargePositiveInteger`
        - `LargeNegativeInteger`
      - `SmallInteger`

Literal numbers without a decimal point automatically become
objects of one of the `Integer` subclasses.

Literal numbers with a decimal point automatically become
objects of one of the `Float` subclasses.

The assignment operator `:=` can be used to
assign a literal number to a variable.
For example:

```smalltalk
n := 1.
n := n + 1
```

Be sure to include the colon because `n = 1` answers whether
`n` is equal to `1`, it doesn't assign `1` to `n`.

A series of assignment operators can be used to
assign the same value to multiple variables.
For example:

```smalltalk
foregroundColor := backgroundColor := Color blue.
```

There are no shorthand assignment operators like `+=` for numbers.

Numbers are automatically converted to objects of the appropriate type.
This includes changing size to accomodate larger and smaller values.
For example:

```smalltalk
| a b c |
a := 1000000000000000000.
b := a * 10.
c := b / 10.
a class print. "SmallInteger"
b class print. "LargePositiveInteger"
c class print. "SmallInteger"
```

The following table describes most of the instance methods
defined in the `Number` class.
These can be invoked on instances of all `Number` subclasses.

| Method                | Description                                                                     |
| --------------------- | ------------------------------------------------------------------------------- |
| `*`                   | answers product of two numbers                                                  |
| `+`                   | answers sum of two numbers                                                      |
| `-`                   | answers difference of two numbers                                               |
| `/`                   | answers quotient of two numbers                                                 |
| `//`                  | answers integer quotient of two numbers truncating toward negative infinity     |
| `=`                   | answers if two numbers are equivalent                                           |
| `@`                   | answers a `Point` object where receiver is x and argument is y                  |
| `\\`                  | answers same as `mod:`                                                          |
| `^`                   | answers same as `raisedTo:`                                                     |
| `abs`                 | answers absolute value of receiver                                              |
| `arcCos`              | answers arccosine of receiver                                                   |
| `arcSin`              | answers arcsine of receiver                                                     |
| `arcTan`              | answers arctangent of receiver                                                  |
| `asFloat`             | answers equivalent `Float` value                                                |
| `asInteger`           | answers equivalent `Integer` value                                              |
| `ceiling`             | answers nearest integer rounding toward infinity                                |
| `cos`                 | answers cosine of receiver in radians                                           |
| `degreeCos`           | answers cosine of receiver in degrees                                           |
| `degreeSin`           | answers sine of receiver in degrees                                             |
| `degreeTan`           | answers tangent of receiver in degrees                                          |
| `cubed`               | answers receiver raised to 3rd power                                            |
| `degreesToRadians`    | answers result of converting receiver in degees to radians                      |
| `div:`                | answers integer division rounding toward negative infinity                      |
| `even`                | answers `Boolean` value indicating if receiver is equivalent to an even integer |
| `floor`               | answers nearest integer rounding toward negative infinity                       |
| `fractionPart`        | answers fractional part (ex. `3.25 fractionPart` gives 0.25)                    |
| `ifNotZero:`          | evaluates argument (typically a block) if receiver is not zero                  |
| `integerPart`         | answers integer part (ex. `3.25 integerPart` gives 3.0)                         |
| `isDivisibleBy:`      | answers `Boolean` value indicating if receiver is divisible by argument         |
| `isNaN`               | always answers `false`                                                          |
| `isNumber`            | always answers `true`                                                           |
| `isZero`              | answers `Boolean` value indicating if receiver is zero                          |
| `lg`                  | answers same as `log2`                                                          |
| `ln`                  | answers natural log of receiver                                                 |
| `log`                 | answers base 10 log of receiver                                                 |
| `log2`                | answers base 2 log of receiver                                                  |
| `log:`                | answers log of receiver where argument is the base                              |
| `magnitude`           | same as `abs`                                                                   |
| `mod:`                | answers receiver modulo argument                                                |
| `moduloTwoPiAsFloat:` | answers receiver modulo 2 \* pi as a `Float`                                    |
| `negated`             | answers receiver with opposite sign                                             |
| `negative`            | answers `Boolean` value indicating if receiver is negative                      |
| `nthRoot:`            | answers argument root of receiver                                               |
| `odd`                 | answers `Boolean` value indicating if receiver is equivalent to an odd integer  |
| `positive`            | answers `Boolean` value indicating if receiver is positive or zero              |
| `radiansToDegrees`    | answers result of converting receiver in radians to degrees                     |
| `raisedTo:`           | answers receiver raised to argument exponent                                    |
| `reciprocal`          | answers reciprocoal of receiver (`1 / self`)                                    |
| `rem:`                | answers remainder of integer division of receiver by argument                   |
| `roundTo:`            | answers nearest value or receiver rounded to a multiple of argument             |
| `rounded`             | answers nearest integer to receiver                                             |
| `sign`                | answers `1`, `0`, or `-1` based on sign of receiver                             |
| `sin`                 | answers sine of receiver in radians                                             |
| `sqrt`                | answers square root of receiver                                                 |
| `squared`             | answers square of receiver                                                      |
| `strictlyPositive`    | answers `Boolean` value indicating if receiver is positive and not zero         |
| `tan`                 | answers tangent of receiver in radians                                          |
| `to:`                 | answers an `Interval` from receiver to argument                                 |
| `to:by:`              | answers an `Interval` from receiver to `to:` in steps of `by:`                  |
| `to:by:do:`           | evaluates "do" block with every value from receiver to `to:` in steps of `by:`  |
| `to:do:`              | evaluates "do" block with every value from receiver to `to:`                    |
| `toSelfPlus:`         | answers an `Interval` from receiver to receiver plus argument                   |
| `truncated`           | answers closes integer rounding toward zero                                     |

For example, `3.14159 roundTo: 0.0001` gives `3.1416`.

All subclasses of `Number` except `Fraction`
implement the `isLiteral` method to always return `true`.
The `Fraction` class implements the `isLiteral` method
to return `true` if the denominator is a multiple of 2 or 5,
and `false` otherwise. TODO: Why?

### Float

The subclasses `Float`, `BoxedFloat64`, and `SmallFloat64`
do not implement any particularly interesting methods
that were not already described for the `Number` class.

The following table describes some of the instance methods
defined in the `BoxedFloat64` and `SmallFloat64` classes
(both subclasses of `Integer`) that are not defined in the `Number` class.

| Method | Description                                                                         |
| ------ | ----------------------------------------------------------------------------------- |
| `<`    | answers `Boolean` value indicating if receiver is less than argument                |
| `<=`   | answers `Boolean` value indicating if receiver is less than or equal to argument    |
| `=`    | answers `Boolean` value indicating if receiver is equal to argument                 |
| `>`    | answers `Boolean` value indicating if receiver is greater than argument             |
| `>=`   | answers `Boolean` value indicating if receiver is greater than or equal to argument |
| `~=`   | answers `Boolean` value indicating if receiver is not equal to argument             |

### Integer

The following table describes some of the instance methods defined
in the `Integer` class that are not defined in the `Number` class.

| Method             | Description                                                                                                            |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------- |
| `/`                | answers result of dividing receiver by argument (`Integer` if divides evenly; `Fraction` otherwise to retain accuracy) |
| `<`                | answers `Boolean` value indicating if receiver is less than argument                                                   |
| `<=`               | answers `Boolean` value indicating if receiver is less than or equal to argument                                       |
| `=`                | answers `Boolean` value indicating if receiver is equal to argument                                                    |
| `>`                | answers `Boolean` value indicating if receiver is greater than argument                                                |
| `>=`               | answers `Boolean` value indicating if receiver is greater than or equal to argument                                    |
| `~=`               | answers `Boolean` value indicating if receiver is not equal to the argument                                            |
| `<<`               | answers new `Integer` obtained by shifting argument bits left                                                          |
| `>>`               | answers new `Integer` obtained by shifting argument bits right                                                         |
| `asFloat`          | answers the equivalent value as a `Float`                                                                              |
| `atRandom`         | answers a random integer from 1 to receiver                                                                            |
| `atRandom:`        | answers a random integer from 1 to receiver using argument as a generator                                              |
| `bitAnd:`          | answers new `Integer` obtained by anding the bits in receiver and argument                                             |
| `bitAt:`           | answers the bit (0 or 1) in receiver at argument position                                                              |
| `bitAt:put:`       | answers new `Integer` obtained by changing the bit at `bitAt:` to `put:`                                               |
| `bitOr:`           | answers new `Integer` obtained by oring the bits in receiver and argument                                              |
| `bitXor:`          | answers new `Integer` obtained by exclusive oring the bits in receiver and argument                                    |
| `factorial`        | answers factorial of receiver                                                                                          |
| `gcd`              | answers greatest common divisor of receiver and argument                                                               |
| `hex`              | answers equivalent hexadecimal string                                                                                  |
| `isPrime`          | answers `Boolean` value indicating if receiver is a prime number                                                       |
| `lcm`              | answers least common multiple of receiver and argument                                                                 |
| `printStringRoman` | answers `String` that is the equivalent Roman numeral                                                                  |
| `printStringWords` | answers `String` that is the equivalent in English words                                                               |
| `timesRepeat:`     | evaluate argument block receiver times                                                                                 |

In the following code, the `Integer` instance method `/`
is used to set the variable `result` to the `Fraction` `4/3`
rather than the `Float` `1.333333...`.

```smalltalk
result := (1/3) * 4
```

To cause the result of the expression above
to be a `Float` value rather than a `Fraction`,
change one or more of the literal `Integer` values to a `Float`
by adding `.0` after the value.
Alternatively, send the `#asFloat` message to any of the `Integer` values.

The result of `1961 printStringRoman` is `'MCMLXI'`.  
The result of `1961 printStringWords` is `'one thousand, nine hundred sixty-one'`.

### Fraction

`Fraction` objects represent rational values that
have the instance variables `numerator` and `denominator`.

Operations of fractions always return a new `Fraction` object
rather than a `Float` object in order to maintain accuracy.

It is recommended to use `Fraction` values rather that `Float` values
whenever possible for better calculation accuracy.

The following table describes some of the instance methods defined
in the `Fraction` class that are not defined in the `Number` class.

| Method        | Description                                                           |
| ------------- | --------------------------------------------------------------------- |
| `asFloat`     | answers the nearest equivalent `Float` value                          |
| `denominator` | answers the denominator of the fraction                               |
| `numerator`   | answers the numerator of the fraction                                 |
| `reduced`     | answers a new `Fraction` that is a reduced equivalent of the receiver |

For example, `(4/6) reduced` returns `2/3`.
Parentheses are needed here because otherwise the message `#reduced`
is sent to the `Integer` `6` which does not have a corresponding method.

## Character

Characters are represented by the `Character` class.
They are restricted to single-byte Latin-1 (ISO 8859-1) characters.

Printable literal characters are preceded by a dollar sign.
For example, `$a`.
Non-printable characters can be obtained from unary class methods
in the `Character` class such as `cr`, `space`, and `tab`.

All instances of this class are pre-created and new instances cannot be created.

The following table describes some of the class methods defined
in the `Character` class.

| Method             | Description                                                           |
| ------------------ | --------------------------------------------------------------------- |
| `codePoint:`       | answers `Character` instance that corresponds to argument code        |
| `cr`               | answers carriage return instance                                      |
| `digitValue:`      | answers `Character` instance that corresponds to argument digit (0-9) |
| `escape`           | answers `Character` instance that corresponds to escape character     |
| `lf`               | answers `Character` instance that corresponds to line feed character  |
| `newLineCharacter` | answers same as `lf`                                                  |
| `separators`       | answers array of whitespace characters                                |
| `space`            | answers `Character` instance that corresponds to space character      |
| `tab`              | answers `Character` instance that corresponds to tab character        |

The following table describes some of the instance methods defined
in the `Character` class.

| Method                     | Description                                                                                                               |
| -------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `<`                        | answers `Boolean` value indicating if receiver is less than argument                                                      |
| `<=`                       | answers `Boolean` value indicating if receiver is less than or equal to argument                                          |
| `=`                        | answers `Boolean` value indicating if receiver is equal to argument                                                       |
| `>`                        | answers `Boolean` value indicating if receiver is greater than argument                                                   |
| `>=`                       | answers `Boolean` value indicating if receiver is greater than or equal to argument                                       |
| `asLowercase`              | answers lowercase version of receiver                                                                                     |
| `asString`                 | answers `String` or `UnicodeString` representation of receiver                                                            |
| `asSymbol`                 | answers `Symbol` of receiver                                                                                              |
| `asUppercase`              | answers uppercase version of receiver                                                                                     |
| `asciiValue`               | answers decimal ASCII value of receiver                                                                                   |
| `codePoint`                | answers decimal Unicode value of receiver                                                                                 |
| `digitValue`               | answers `Integer` value of digit `Character`; opposite of class method `digitValue:`                                      |
| `hex`                      | answers hexadecimal ASCII value of receiver                                                                               |
| `isAlphaNumeric`           | answers `Boolean` value indicating if receiver is a letter or digit                                                       |
| `isDigit`                  | answers `Boolean` value indicating if receiver is a digit                                                                 |
| `isLetter`                 | answers `Boolean` value indicating if receiver is a letter                                                                |
| `isLiteral`                | always answers `true`                                                                                                     |
| `isLowercase`              | answers `Boolean` value indicating if receiver is lowercase                                                               |
| `isSeparator`              | answers `Boolean` value indicating if receiver is whitespace                                                              |
| `isUppercase`              | answers `Boolean` value indicating if receiver is uppercase                                                               |
| `isValidInBinarySelectors` | answers `Boolean` value indicating if receiver can appear in a binary selector name                                       |
| `isValidInFilenames`       | answers `Boolean` value indicating if receiver can appear in a file name                                                  |
| `isValidInIdentifier`      | answers `Boolean` value indicating if receiver can appear in a variable name or unary/keyword selector                    |
| `isValidStartOfIdentifier` | answers `Boolean` value indicating if receiver can appear as first character in a variable name or unary/keyword selector |
| `isVowel`                  | answers `Boolean` value indicating if receiver is a vowel                                                                 |
| `to:`                      | answers `Array` of `Character` instances from receiver to argument                                                        |
| `tokenish`                 | answers `Boolean` value indicating if receiver can appear in a token (letter, digit, or colon)                            |

## Strings

The following list depicts the class hierarchy for character data:

- `Collection`
  - `SequenceableCollection`
    - `CharacterSequence`
      - `String`
        - `Symbol`
      - `UnicodeString`
        - `UnicodeSymbol`

Many methods for operating on `String` and `Symbol` instances
are found in the `SequenceableCollection` class.

Instances of `String` and `UnicodeString` are mutable collections of characters.
But instances of `CharacterSequence`, `Symbol`, and `UnicodeSymbol` are immutable.

Literal strings are delimited by single quotes,
not double quotes which are used to delimit comments.
To include (escape) a single quote, use two in succession.

The type of object created, `String` or `UnicodeString`, is automatically
selected based on whether any of the characters require more than one byte.
`String` objects can only hold Latin-1 (ISO 8859-1) characters
which are represented by a single byte.
`UnicodeString` objects can, as the name implies, hold Unicode characters
whose representation can require multiple bytes.

Once a `String` object is created, the `#at:put:` message cannot be
used to set its characters to ones that require multiple bytes.
For example, the following code results in the error
"String only store Latin-1 Characters".

```smalltalk
| s smile |
s := 'abc'.
smile := Character codePoint: 9786.
s at: 1 put: smile.
```

To fix this, change the line that assigns to `s`
to `s := 'abc' asUnicodeString`.

To find the first character in a string `CharacterSequence`
that matches some condition,
use the `findFirst:` method in `SequenceableCollection`.
For example, the following finds the index of the first uppercase character.

```smalltalk
'fooBar' findFirst: [:char |char isUppercase]. "4"
```

### CharacterSequence

The `CharacterSequence` class method `readFrom:` answers an instance
created by reading text from a stream.

The following table describes some of the instance methods
defined in the `CharacterSequence` class.

All indexes are 1-based and `0` means not found.

| Method                                 | Description                                                                                                                   |
| -------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `append:`                              | answers new `CharacterSequence` containing argument characters appended to receiver characters; same as `,` in `String` class |
| `asCamelCase`                          | answers copy created by camelCasing white-space separated words (first letter lower)                                          |
| `asDate`                               | answers `Date` parsed from receiver                                                                                           |
| `asLowercase`                          | answers copy that is all lowercase                                                                                            |
| `asNumber`                             | answers number parsed from receiver                                                                                           |
| `asPlural`                             | answers plural of an English word                                                                                             |
| `asUnicodeString`                      | answers receiver converted to a `UnicodeString`                                                                               |
| `asUppercase`                          | answers copy that is all uppercase                                                                                            |
| `beginsWith:`                          | answers `Boolean` indicating if receiver begins with given substring                                                          |
| `capitalized`                          | answers copy where first letter is changed to uppercase                                                                       |
| `collect:`                             | answers result of applying block argument to each character                                                                   |
| `endsWith:`                            | answers `Boolean` indicating if receiver ends with given substring                                                            |
| `findString:`                          | answers index where argument substring begins                                                                                 |
| `findString:startingAt:caseSensitive:` | answers index after `startingAt:` where a substring begins, optionally case sensitive                                         |
| `findTokens:`                          | answers `Array` of instances created by splitting receiver on delimiters in argument                                          |
| `format:`                              | answers instance created using interpolation                                                                                  |
| `includesSubString:`                   | answers `Boolean` indicating if receiver contains substring (note capital S)                                                  |
| `includesSubstring:caseSensitive:`     | answers `Boolean` indicating if receiver contains substring                                                                   |
| `indexOf:`                             | answers index of a character                                                                                                  |
| `isEmpty`                              | answers `Boolean` indicating if receiver size is zero                                                                         |
| `isLiteral`                            | always answers `true`                                                                                                         |
| `join:`                                | answers instance formed by joining `Array` elements of any type with receiver delimiter                                       |
| `match:`                               | answers `Boolean` indicating whether receiver matches a pattern (can include `#` and `\*` characters)                         |
| `padded:to:width:`                     | answers copy formed by padding receiver on left or right with a given `Character`                                             |
| `prefixAndSuffix:`                     | answers `Array` of instances formed by splitting receiver on last occurrence of a `Character`                                 |
| `size`                                 | answers largest legal index                                                                                                   |
| `squeezedTo:`                          | answers instance that optimizes readability of receiver in given number of characters                                         |
| `subStrings:`                          | answers `Array` of instances formed by splitting receiver on delimiters                                                       |
| `substrings`                           | answers `Array` of instances created by splitting receiver on whitespace                                                      |
| `substringsSeparatedBy:`               | answers `Array` of instances formed by splitting receiver on a single delimiter `Character`                                   |
| `truncateWithElipsisTo:`               | answers instance formed by truncating receiver to given length with elipsis in last 3 of length                               |
| `uncapitalized`                        | answers copy where first letter is changed to lowercase                                                                       |
| `withBlanksCondensed`                  | answers instance created by removing leading and trailing spaces and replacing consecutive spaces with one                    |
| `withBlanksTrimmed`                    | answers instance created by removing leading and trailing spaces                                                              |
| `withoutEnclosing:`                    | answers instance created by removing first and last characters if they match a given `Character`                              |
| `withoutLeadingBlanks`                 | answers instance created by removing leading blanks                                                                           |
| `withoutPrefix`                        | answers instance created by removing given substring prefix                                                                   |
| `withoutSuffix`                        | answers instance created by removing given substring suffix                                                                   |
| `withoutTrailingBlanks`                | answers instance created by removing trailing blanks                                                                          |

To get a substring from a given index to another index,
use the method `#copyFrom:to:`
which is defined in the `SequenceableCollection` class.
For example:

```smalltalk
s := 'abcdef'.
sub := s copyFrom: 4 to: s size. "answers 'def'"
```

There is no method that only takes a start index
and returns the subtring to the end of the string.

### String

Literal `String` objects are surrounded by single quotes.

The `String` class inherits many instance methods from the
`CharacterSequence` class and overrides the behavior some of them.
A notable addition is the binary method comma (`,`)
which answers a new `String` containing
the argument characters appended to the receiver characters.
For example, `'Hello', ' ', 'World'` yields the `String` `'Hello World'`.

The following table describes some of the class methods
defined in the `String` class.

All indexes are 1-based and `0` means not found.

| Method                                  | Description                                                                                   |
| --------------------------------------- | --------------------------------------------------------------------------------------------- |
| `compare:with:`                         | answer 1 if `compare:` is less than `with:`, 2 if equal, and 3 if greater than                |
| `compareIgnoringCase:with:`             | same as `compare:with:`, but case is ignored                                                  |
| `crString`                              | answers instance containing the carriage return character                                     |
| `crlfString`                            | answers instance containing the carriage return and line feed characters                      |
| `findString:in:startingAt:`             | answers index of `findString:` in `in:` starting at index `startingAt:`                       |
| `findStringIgnoringCase:in:startingAt:` | same as `findString:in:startingAt:`, but case is ignored                                      |
| `is:equalTo:`                           | answers `Boolean` indicating if `is:` is equal to `equalTo:`                                  |
| `isAscii:`                              | answers `Boolean` indicating if all the characters are ASCII                                  |
| `isEmpy:`                               | answers `Boolean` indicating if size is zero                                                  |
| `lfString`                              | answers instance containing only a line feed character                                        |
| `new:withAll:`                          | answers instance with length `new:` where all characters are `withAll:`                       |
| `newLineString`                         | answers instance containing only a newline character                                          |
| `percentEscapingNonAscii`               | answers URL encoded instance where non-ASCII characters are percent encoded                   |
| `string:lineIndicesDo:`                 | evalautes block `lineIndicesDo:` for each substring of `string:` delimited by CR, LF, or CRLF |
| `substringsIn:`                         | answers an `Array` of substrings delimited by whitespace characters                           |
| `tab`                                   | answers instance containing the tab character                                                 |

To get a substring of a `String`, use the `copyFrom:to:` method
defined in `SequenceableCollection`. For example:

```smalltalk
'foobarbaz' copyFrom: 4 to: 6 "bar"
```

The following code demonstrates processing lines in a `String`:

```smalltalk
cr := String crString.
s := 'foo', cr, 'bar', cr, 'baz'.
"Alternate way to embed newline characters in a String"
s := 'foo
bar
baz'.

"Print each line."
String string: s lineIndicesDo: [:start :end :endWith |
    (s copyFrom: start to: end) print
].
"Alternate way to iterate over the lines in a String"
s substrings do: [:sub | sub print].
```

The following table describes some of the instance methods
defined in the `String` class
that are not also defined in its superclass `CharacterSequence`.

| Method                                 | Description                                                                                   |
| -------------------------------------- | --------------------------------------------------------------------------------------------- |
| `,`                                    | answers new string that results from appending argument                                       |
| `<`                                    | answers `Boolean` value indicating if receiver is less than argument                          |
| `<=`                                   | answers `Boolean` value indicating if receiver is less than or equal to argument              |
| `=`                                    | answers `Boolean` value indicating if receiver is equal to argument                           |
| `>`                                    | answers `Boolean` value indicating if receiver is greater than argument                       |
| `>=`                                   | answers `Boolean` value indicating if receiver is greater than or equal to argument           |
| `at:`                                  | answers `Character` at given index                                                            |
| `at:put:`                              | replaces `Character` at given index                                                           |
| `byteSize`                             | answers size in bytes                                                                         |
| `findString:startingAt:`               | answers index after `startingAt:` where a substring begins, case sensitive                    |
| `findStringCaseInsenstive:startingAt:` | answers index after `startingAt:` where a substring begins, case insensitive                  |
| `lineIndicesDo:`                       | evalautes block `lineIndicesDo:` for each substring of `string:` delimited by CR, LF, or CRLF |
| `percentEscapeUrl`                     | answers URL encoded instance where non-ASCII characters are percent encoded                   |
| `percentEscapeUrlField`                | answers URL encoded instance where non-ASCII characters in fields are percent encoded         |
| `size`                                 | answers largest index                                                                         |
| `substrings`                           | answers `Array` of substrings delimited by whitespace characters                              |
| `unescapePercents`                     | answers reverse of `percentEscapeUrl`                                                         |

To get a substring of a `String`, use the `copyFrom:to:` method
defined in the `OrderedCollection` class which is a superclass of `String`.
For example:

```smalltalk
s := 'foobar'.
sub := s copyFrom: 4 to: s size. "bar"
```

The `at:put:` method is the only one
that modifies a `String` in place (TODO: true?).
All other methods return a new `String`
that is a modified version of the receiver.

The `format:` method returns a `String` created from a template
using interpolation where input comes from an `Array`.
For example, both of the following produce the string
`'Player Ratelle is number 19.'`:

```smalltalk
s := 'Player {1} is number {2}.' format: #('Ratelle' 19).

name := 'Ratelle'.
number := 19.
s := 'Player {1} is number {2}.' format: {name. number}.
```

The `String` `format:` method is useful for print-style debugging.
For example, the following is the equivalent
of a `console.log` call in JavaScript.

```smalltalk
('myVariable = {1}' format: {myVariable}) print
```

An even better approach is to defined the `logAs:` method in the `Object` class.
This was described in "Getting Started" section "Transcripts".

The `padded:to:with:` method answers a copy formed by
padding receiver on the left or right with a given `Character`.
For example, the following code answers a `String`
containing three spaces followed by `'19'`:

```smalltalk
19 asString padded: #left to: 5 with: Character space
```

The `prefixAndSuffix:` method answers an `Array` of instances
formed by splitting receiver on last occurrence of a `Character`.
For example, the following code answers
an `Array` containing `'/foo/bar'` and `'baz.txt')`.

```smalltalk
'/foo/bar/baz.txt' prefixAndSuffix: $/
```

Since the `String` class is a subclass of `Collection`, the `select:` method
can be used to iterate over and select a subset of its characters.
For example, the following code returns a `String`
containing all the digits found in another `String`:

```smalltalk
'Buy 14 bananas.' select: [:c | c isDigit] "14"
```

To create a string containing a given number of a single character
such as a space for indentation:

```smalltalk
indentation := String new: aNumber * 4 withAll: Character space.
```

### Symbol

The `Symbol` class is a subclass of the `String` class.

`Symbol` instances are globally unique whereas `String` instances are not.
Literal symbols are preceded by a pound sign (`#`).
Their text is surrounded by single quotes if it
contains any special characters such as spaces.
For example:

```smalltalk
| sym1 sym2 |
sym1 := #word.
sym2 := #'multiple words'.
```

The following code demonstrates a difference
between `String` and `Symbol` objects.

```smalltalk
| str1 str2 sym1 sym2 |
sym1 := #test.
sym2 := #test.
"sym1 and sym2 will always refer to the same object in memory."

str1 := 'test'.
str2 := 'test'.
"str1 and str2 may or may not refer to the same object in memory.
```

The compiler decides whether equivalent string values
should be represented by the same object in memory.
Perhaps it only does this when they appear in the same scope.
For this reason, user code should not assume that they will be different objects,
especially when code modifies a `String` by sending it the `#at:put:` message.

There are no particularly interesting class methods in the `Symbol` class.

The following table describes some of the instance methods
defined in the `Symbol` class that are not also defined in superclasses.

| Method             | Description                                                                  |
| ------------------ | ---------------------------------------------------------------------------- |
| `asString`         | answers a `String` containing the same characters as receiver                |
| `isLiteral`        | always answers `true`                                                        |
| `numArgs`          | answers number of arguments in a keyword message or 0 if not                 |
| `precedence`       | answers 0 if not a valid selector, 1 if unary, 2 if binary, and 3 if keyword |
| `separateKeywords` | answers space-separated `String` containing keywords                         |
| `value:`           | answers result of sending receiver as a unary message to argument            |

Many of the `Symbol` instance methods are useful for
run-time evaluation of instances as keyword messages.

The `UnicodeSymbol` class is similar to the `Symbol` class,
but can hold Unicode characters.

## Regular Expressions

To add support for regular expressions:

- `cd` to the directory containing the 'Cuis-Smalltalk-Dev' directory.
- `git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Regex.git`
- Open a Workspace.
- Enter `Feature require: 'Regex-Core'` and "Do it".
- Enter `Feature require: 'Regex-TerseGuide'` and "Do it".
- Open the World menu and select Help ... Terse Guide to Cuis.
- In the left nav, scroll to the bottom and select "Regex".
- Review all the example code.

Regular expressions are specfied with strings.

The following code processes
a URL path `String` like `'/state/:state/city/:city'`
and returns the pattern `String` `'/state/*/city/*'`.

```smalltalk
pattern := pathString copyWithRegex: '\:\w+' matchesReplacedWith: '*'.
```

Many standard regular expression features are not implemented.

- The regex delimiters '^', '$', '\n' are not recognized.
  - The `String matchesRegex:`, `String matchesRegexIgnoringCase:`,
    `RxParser match:`, and `RxMatcher matches` methods
    match against the entire string as if the regular expression
    began with `^` and ended with `$`.
  - The `RxMatcher` `search:` and `searchStream:` methods
    match against any substring.
- To match a literal colon, use '\:'.
- Non-greedy syntax is not implemented.
- Limits such as `\d{2,5}` are not implemented.

The following code demonstrates using capture groups
which this package refers to as "subexpressions".

```smalltalk
s := 'Mark was born on 4/16/1961.'.
re := '(\d+)/(\d+)\/(\d+)' asRegex. "returns an RxMatcher object"
re search: s.
re subexpressionCount print. "4"
(re subexpression: 1) print. "4/16/1961"
(re subexpression: 2) print. "4"
(re subexpression: 3) print. "16"
(re subexpression: 4) print. "1961"
```

## Point

The `Point` class is in the "Graphics-Primitives" class category.
Instances represent a 2D point and have the instance variables `x` and `y`.
In addition to describing Cartesian points, these can be used to describe
a 2D size where `x` represents the width and `y` represents the height.

An instance can be created by sending `#x:y:` to the `Point` class
or by sending `#@` to a `Number` object.
For example, the following expressions create equivalent `Point` objects:

```smalltalk
p1 := Point x: 1.2 y: 3.4.
p2 := 1.2 @ 3.4.
```

Including spaces around `@` is optional.

The `Point` class has a large number of instance methods (over 70)
including methods for adding, subtracting, dividing, and comparing instances.

## Rectangle

There are many ways create an instance of the `Rectangle` class.
Each of the following lines creates an equivalent `Rectangle` instance.

```smalltalk
rect1 := Rectangle center: 5@4 extent: 4@6.
rect2 := Rectangle left: 3 right: 7 top: 1 bottom: 7.
rect3 := Rectangle origin: 3@1 corner: 7@7.
rect4 := Rectangle origin: 3@1 extent: 4@6.
rect5 := 3@1 extent: 4@6. "receiver is a Point instance"
```

The following messages get specific data from a `Rectangle` instance:

```smalltalk
rect origin. "3@1"
rect extent. "4@6"
rect width. "4"
rect height. "6"
```

To determine if a `Point` is inside or on the edge of a `Rectangle`,
send the `#containsPoint:` message to the `Rectangle` with a `Point` argument.
For example:

```smalltalk
rect containsPoint: 5@3. "true"
```

To determine if a `Rectangle` intersects another `Rectangle`,
send one the `#intersects:` message with the other as the argument.

## UUID

The package "Identities-UUID" generates UUID values.
To install it, enter `Feature require: 'Identities-UUID'` in a Workspace
and "Do it".
To generate a UUID value, use `UUID new`.

## Conversions

Many classes that represent generic data types have instance methods
whose names begin with "as" that convert the value to a different type.
Highlights include:

- `asCamelCase`: answers `String` created by
  removing spaces from a `CharacterSequence` and
  capitalizing the first letter of each word except the first
- `asDate`: answers `Date` created from a `CharacterSequence`
  with many allowed forms
- `asFloat`: answers `Float` representation of any kind of `Number`
- `asFraction`: answers `Fraction` representation of a `Float` or `Integer`
- `asInteger`: answers `Integer` representation of any kind of `Number` (truncates)
- `asJsonString`: answers JSON `String` representation of any `Object`
- `asLowercase`: answers lowercase `String` representation
  of any `CharacterSequence` or `String`
- `asMonth`: answers `Month` created from a `CharacterSequence`
  with many allowed forms such as `4/16/1961`
  (`start` instance variable sets day to 1 and time to midnight,
  keeping the month and year)
- `asNumber`: answers a `Number` parsed from a `CharacterSequence`
- `asOxfordCommandStringAnd`: answers `String` created from `Collection` elements;
  for example, `#(#red #green #blue) asOxfordCommaStringAnd`
  answers `'red, green, and blue'`
- `asPlural`: answers `String` plural of a `CharacterSequence`;
  for example, `'cactus' asPlural` answers `'cacti'`
- `asRegex`: answers `RxMatcher` created from a `String`
  containing regular expression syntax
- `asRegexIgnoringCase`: same as `asRegex`, but ignores case
- `asString`: answers `String` representation of any kind of `Object`
- `asSymbol`: answers `Symbol` representation of any `Character` or `CharacterSequence`
- `asTrueFraction`: answers exact `Fraction` representation of a `Float`
- `asUnicodeString`: answers `UnicodeString` representation of a `CharacterSequence`
- `asUppercase`: answers lowercase `String` representation
  of any `CharacterSequence` or `String`
- `asYear`: answers `Year` created from a `CharacterSequence`
  containing only a year number

See the "Collections" section for methods that
convert from one kind of collection to another.
