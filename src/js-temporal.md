---
eleventyNavigation:
  key: JavaScript Temporal
layout: topic-layout.njk
---

### Overview

`Temporal` is a global object being added to JavaScript.
It supports representing dates and times,
and performing calculations on them.
It provides a better alternative to the JavaScript `Date` class
and libraries like Moment.js and date-fns.

Issues with the JavaScript `Date` class
that are all addressed in `Temporal` include:

- lack of support for time zones other than local time and UTC
- unpredictable daylight savings time (DST) behavior
- lack of support for non-Gregorian calendars
- Date objects are mutable

As of November 7, 2023, `Temporal` is a
[stage 3 ECMAScript draft](https://tc39.es/proposal-temporal/)
and it not yet supported by any web browsers.
However, a [polyfill](https://www.npmjs.com/package/proposal-temporal)
is available in npm that can be used today to gain experience in using it.
Since the proposal has not reached stage 4,
the API may undergo changes before it is finalized.

### Installing

To install the polyfill in a project,
enter `npm install proposal-temporal` (not `temporal`!).

### Importing

To import the polyfill, use

```js
const {Temporal} = require('proposal-temporal');
```

or

```js
import {Temporal} from 'proposal-temporal/lib/index.mjs';
```

### Primary Classes

The primary classes that support the functionality of `Temporal` are:

- `Absolute` represents the current absolute time without regard
  to a particular calendar system or time zone.
  It stores the number of nanoseconds since the epoch in a `BigInt`.
- `TimeZone` represents a time zone.
- `DateTime` represents a date/time combination.
- `Date` represents a date with no time.
- `YearMonth` represents a year/month combination with no day or time.
- `MonthDay` represents a month/day combination with no year or time.
- `Time` represents a time of day with no date.
- `Duration` represents a duration of time rather than a date or time of day.
- `Calendar` implements a calendar system used by the other classes.

### Common Features

The classes provided by `Temporal` have many common features.

There are multiple ways to create instances.

1. constructor  
   Instances can be created by using the `new` keyword
   to invoke the constructor of any of these classes.
   For example:

   ```js
   const lunch = new Temporal.Time(11, 30); // 11:30 AM
   ```

2. `from` static method  
   All of these classes have a `from` static method
   that can be passed a string or an object
   that describes the properties the instance should have.
   For example:

   ```js
   const lunch = Temporal.Time.from({hour: 11, minute: 30});
   // or
   const lunch = Temporal.Time.from('11:30');
   ```

3. `with` instance method  
   All of these classes except `Calendar`, `Duration`, and `TimeZone`
   have a `with` instance method that makes a copy of an existing instance
   and sets specified properties to the values in an object that is passed.
   For example:

   ```js
   const lateLunch = lunch.with({hour: 12}); // 12:30 PM
   ```

Most of the classes create instances that have read-only properties.
They are read-only because they have getters, but no setters.
In addition to accessing their values directly by property name,
all of these classes except `Calendar`, `Duration`, and `TimeZone`
have a `getFields` instance method that returns an object
that contains the value of each read-only property.
For example:

```js
console.log(lunch.minute); // 30
console.log(lunch.getFields());
/* Output is
{
  hour: 11,
  microsecond: 0,
  millisecond: 0,
  minute: 30,
  nanosecond: 0,
  second: 0
}
*/
```

All of these classes provide a `toString` instance method.
All but `Calendar` provide a `toJSON` instance method
that returns the same value as `toString`,
typically by just delegating to that method.
All but `Calendar` and `TimeZone` provide a `toLocaleString` instance method
that accepts formatting arguments that are passed to `Intl.DateTimeFormat`.
For example:

```js
const now = Temporal.now.dateTime();
console.log(now.toString()); // 2020-09-02T12:01:13.886073869
console.log(now.toLocaleString()); // 9/2/2020, 11:51:06 AM
console.log(now.toLocaleString('en-GB')); // 02/09/2020, 11:51:06
```

All of these classes except `Calendar`, `Duration`, and `TimeZone`
provide an `equals` instance method that returns a boolean indicating
whether a given object is from the same class as the receiver object
and their properties are equal. For example:

```js
console.log(lunch.equals(dinner)); // false
```

All of these classes except `Calendar`, `Duration`, `TimeZone`, and `MonthDay`
provide a `compare` static method that compares two objects from the same class
and returns -1, 0, or 1.
It is suitable for passing to the `Array` `sort` method
for sorting arrays of instances.
(Why doesn't `MonthDay` support the static method `compare`?)

For example:

```js
const times = [
  new Temporal.Time(11, 30),
  new Temporal.Time(15),
  new Temporal.Time(7, 45)
];
times.sort(Temporal.Time.compare);
for (const time of times) {
  console.log(time.toString()); // 07:45, 11:30, 15:00
}
```

All of these classes except `Calendar`, `TimeZone`, and `MonthDay`
provide `plus` and `minus` instance methods
that take a `Duration` or an object describing one
and return a new instance from the same class as the receiver object.
For example:

```js
const meeting = new Temporal.DateTime(2020, 9, 2, 13); // Sep 2, 2020, 1 PM
const reschedule = meeting.plus({hours: 2}); // 2 hours later
// reschedule is a Temporal.DateTime object.
console.log(reschedule.toString()); // 2020-09-02T15:00
```

The `plus` and `minus` methods correctly adjust for
unit underflows and overflows. For example:

```js
// Create a DateTime that is one minute before midnight.
const beforeMidnight = new Temporal.DateTime(2020, 9, 2, 23, 59);
// Create a new DateTime that is two minutes later.
const afterMidnight = beforeMidnight.plus({minutes: 2});
// Note how the day has incremented.
console.log(afterMidnight.toString()); // 2020-09-03T00:01

const inMarch = new Temporal.Date(1981, 3, 1);
const inFebruary = inMarch.minus({days: 1});
// Note how the month and day were properly adjusted.
console.log(inFebruary.toString()); // 1981-02-28
```

All of these classes except `Calendar`, `Duration`, `TimeZone`, and `MonthDay`
provide a `difference` instance method that takes
another object from the same class and
returns a `Duration` that represents their difference.
For example:

```js
const lunch = new Temporal.Time(11, 30); // 11:30 AM
const dinner = new Temporal.Time(18, 0); // 6:00 PM
const timeBetween = dinner.difference(lunch);
console.log('lunch to dinner =', timeBetween.toString()); // PT6H30M
```

### Temporal.now

`Temporal.now` is a object with methods that return an object
that represents the current system date/time using an instance of
`Absolute`, `TimeZone`, `DateTime`, `Date`, or `Time`.

`Temporal.now.absolute()` returns an `Absolute` object.

`Temporal.now.timeZone()` returns a `TimeZone` object.

`Temporal.now.dateTime()` returns a `DateTime` object.
To use a different time zone, pass that as the first argument.
To use a different calendar system, pass that as the second argument.

`Temporal.now.date()` returns a `Date` object.
To use a different time zone, pass that as the first argument.
To use a different calendar system, pass that as the second argument.

`Temporal.now.time()` returns a `Time` object.
To use a different time zone, pass that as the first argument.

### Temporal.Absolute

To create an instance that represents the current system date/time:

```js
const abs = Temporal.now.absolute();
```

To create an instance that represents a date/time
that is a given number of nanoseconds from the epoch:

```js
const abs = new Temporal.Absolute(0n); // pass a BigInt
// or from a string
const abs = Temporal.Absolute.from('1961-04-16T10:19:20Z');
// or from a Java Date object
const date = new Date('1961-04-16');
const abs = Temporal.Absolute.from(date.toISOString());
```

Instances of this class have no read-only properties.

The value can be retrieved as a number of some unit since the epoch
by calling one of the instance methods `getEpochSeconds`,
`getEpochMilliseconds`, `getEpochMicroseconds`, or `getEpochNanoseconds`
and passing no arguments.

The `toString` instance method returns strings like
`2020-09-01T22:31:22.956482956Z`.
It can be passed an optional time zone.

To create a `DateTime` from an `Absolute` and a `TimeZone`,
`absolute.toDateTime(timeZone)`.

Class methods that create an instance include:

- `Absolute.fromEpochSeconds(seconds)`
- `Absolute.fromEpochMilliseconds(milliseconds)`
- `Absolute.fromEpochMicroseconds(microseconds)`
- `Absolute.fromEpochNanoseconds(nanoseconds)`
- `Absolute.from(item)`

### Temporal.TimeZone

To create an instance that represents the current time zone:

```js
const tz = Temporal.now.timeZone();
```

To create an instance that represents a specific time zone:

```js
const laTz = new Temporal.TimeZone('America/Los_Angeles');
// or using the static method "from" ...
const laTz = Temporal.TimeZone.from('America/Los_Angeles');
```

Valid time zone identifiers are listed at
<https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>.
and <https://nodatime.org/TimeZones>.

Instances of this class have the read-only property `name`.

Time zone offsets vary based on a date and time.
Computing an offset requires an `Absolute`, typically the current value.
To get the time zone offset for a given instance
in nanoseconds or as a string:

```js
const nowAbs = Temporal.now.absolute();
const currentOffset = tz.getOffsetNanosecondsFor(nowAbs); // some large number
const currentOffset = tz.getOffsetStringFor(nowAbs); // ex. -05:00
const laOffset = laTz.getOffsetStringFor(nowAbs); // ex. -07:00
```

To get a DateTime from a TimeZone and Absolute,
`timeZone.getDateTimeFor(absolute)`.

To get an Absolute from a TimeZone and DateTime,
`timeZone.getAbsoluteFor(dateTime)`.

To get the previous and next dates at which a
daylight savings time change occurs in a given time zone:

```js
const prevDst = tz.getPreviousTransition(nowAbs).toString(); // 2020-03-08T08:00Z
const nextDst = tz.getNextTransition(nowAbs).toString(); // 2020-11-01T07:00Z
```

To get the time zone identifier string from an instance,
use `timeZone.toString()` which returns the value of the `name` property.

### Temporal.DateTime

To create an instance that represents the current date/time:

```js
const nowDt = Temporal.now.dateTime();
```

To create an instance that represents a specific date/time:

```js
// Must pass year, month, and day.
// Can optionally pass hour, minute, second,
// millisecond, microsecond, nanosecond, and calendar.
const dateTime = new Temporal.DateTime(1961, 4, 16, 10, 19, 20);
// or
const dateTime = Temporal.DateTime.from({
  year: 1961,
  month: 4,
  day: 16,
  hour: 10,
  minute: 19,
  second: 20
});
// or
const dateTime = Temporal.DateTime.from('1961-04-16T10:19:20');
```

To create a copy of an instance where certain properties are modified:

```js
const hourLater = dateTime.with({hour: dateTime.hour + 1});
```

Instances of this class have the read-only properties
`year`, `month`, `day`, `hour`, `minute`, `second`,
`millisecond`, `microsecond`, `nanosecond`, `calendar`,
`era` (undefined for default calendar), `dayOfWeek`, `dayOfYear`,
`weekOfYear`, `daysInYear`, `daysInMonth`, and `isLeapYear`.

Instance methods include:

The `toString` instance method returns strings
like `2020-09-01T17:55:23.700923686`.

To create an `Absolute` from a `DateTime` and a `TimeZone`,
`dateTime.toAbsolute(timeZone)`.

To create a `Date`, `dateTime.toDate()`.

To create a `YearMonth`, `dateTime.toYearMonth()`.

To create a `MonthDay`, `dateTime.toMonthDay()`.

To create a `Time`, `dateTime.toTime()`.

### Temporal.Date

To create an instance that represents the current date:

```js
const now = Temporal.now.date(); // current date
```

To create an instance that represents a specific date:

```js
// Must pass year, month, and day.
// Can optionally pass calendar.
const birthday = new Temporal.Date(1961, 4, 16);
// or from an object containing properties
const birthday = Temporal.Date.from({year: 1961, month: 4, day: 16});
// or create from a string
const birthday = Temporal.Date.from('1961-04-16');
```

To create a copy of an instance where certain properties are modified:

```js
const dayLater = birthday.with({day: birthday.day + 1});
```

This doesn't wrap values into the next unit. It just stops at the upper limit.
For example, if 30 is added to the day,
the day is set to the last day in the month.

Instances of this class have the read-only properties
`year` (4-digit), `month` (1-based), `day` (0=Monday, ..., 6=Sunday),
`calendar`, `era`, `dayOfWeek`, `dayOfYear`,
`weekOfYear`, `daysInYear`, and `isLeapYear`.

To get the day of the week from an instance:

```js
const DAYS = [
  '',
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday',
  'Sunday'
];
console.log(
  'birthday =',
  birthday.toString(),
  'on a',
  DAYS[birthday.dayOfWeek]
);
```

To get the number of days between two dates:

```js
const otherBirthday = new Temporal.Date(1961, 9, 9);
const diff = otherBirthday.difference(birthday);
console.log('difference in days =', diff.days);
```

To create a `DateTime` from a `Date` and a `Time`, `date.toDateTime(time)`.

To create a `YearMonth` from a `Date`, `date.toYearMonth()`.

To create a `MonthDay` from a `Date`, `date.toMonthDay()`.

The `toString` instance method returns strings like `2020-09-01`.

### Temporal.YearMonth

To create an instance that represents a specific year/month:

```js
// Must pass year and month.
// Can optionally pass calendar.
const ym = new Temporal.YearMonth(1961, 4);
// or
const ym = Temporal.YearMonth.from({year: 1961, month: 4});
// or
const ym = Temporal.YearMonth.from('1961-04');
```

To create a copy of an instance where certain properties are modified:

```js
const monthLater = ym.with({month: ym.month + 1});
```

This doesn't wrap values into the next unit. It just stops at the upper limit.
For example, if 11 is added to the month, the month is set to 12.

Instances of this class have the read-only properties
`year`, `month`, `calendar`, `era`,
`daysInMonth`, `daysInYear`, and `isLeapYear`.

console.log('year/month =', ym.toString()); // 1961-04

The `toString` instance method returns strings like `1961-04`.

To create a `Date` for a given day within the `YearMonth`,
`yearMonth.toDateOnDay(day)`. For example:

```js
console.log('date on 16th =', ym.toDateOnDay(16).toString()); // 1961-04-16
```

### Temporal.MonthDay

To create an instance that represents a specific year/month:

```js
// Must pass month and day.
// Can optionally pass calendar.
const md = new Temporal.MonthDay(4, 16);
// or
const md = Temporal.MonthDay.from({month: 4, day: 16});
// or
const md = Temporal.MonthDay.from('04-16');
```

To create a copy of an instance where certain properties are modified:

```js
const dayLater = md.with({day: md.day + 1});
```

This doesn't wrap values into the next unit. It just stops at the upper limit.
For example, if 30 is added to the day,
the day is set to the last day in the month.

Instances of this class have the read-only properties
`month`, `day`, and `calendar`.

console.log('md =', md.toString()); // 04-16

To create a `Date` from a `MonthDay` and a year number,
`monthDay.toDateInYear(year)`. For example:

```js
console.log('date in year =', md.toDateInYear(1961).toString()); // 1961-04-16
```

The `toString` instance method returns strings like `04-16`.

### Temporal.Time

To create an instance that represents the current time:

```js
let nowT = Temporal.now.time();
```

To create an instance that represents a specific time:

```js
// Can pass hour, minute, second, millisecond, microsecond, and nanosecond.
const t = new Temporal.Time(1, 2, 3);
// or
const t = Temporal.Time.from({hour: 1, minute: 2, second: 3});
// or
const t = Temporal.Time.from('01:02:03');
```

To create a copy of an instance where certain properties are modified:

```js
const minuteLater = t.with({day: t.minute + 1});
```

This doesn't wrap values into the next unit. It just stops at the upper limit.
For example, if 59 is added to the minute, the minute is set to 60.

Instances of this class have the read-only properties
`hour`, `minute`, `second`, `millisecond`, `microsecond`, and `nanosecond`.

console.log('time =', t.toString());
console.log('time =', t.toString()); // 01:02:03

The `toString` instance method returns strings like `01:02:03`.

### Temporal.Duration

To create an instance:

```js
// Can pass years, months, weeks, days, hours, minutes, seconds,
// milliseconds, microseconds, and nanoseconds.
// 2 hours 57 minutes 11 seconds
const dur = new Temporal.Duration(0, 0, 0, 0, 2, 57, 11);
// or
const dur = Temporal.Duration.from({hours: 2, minutes: 57, seconds: 11});
// or
const dur = Temporal.Duration.from('PT02H57M11S');
```

To create a copy of an instance where certain properties are modified:

```js
const extraHour = dur.with({hours: dur.hours + 1}); // 3 hours 57 minutes 11 seconds
```

Instances of this class have the read-only properties
`years`, `months`, `weeks`, `days`, `hours`, `minutes`, `seconds`,
`milliseconds`, `microseconds`, `nanoseconds`, and `sign`.
Note that these property names (with the exception of `sign`) are plural,
unlike the property names in the other `Temporal` classes.

To add one `Duration` to another, creating a new `Duration`:

```js
const newDur = dur.plus(Temporal.Duration.from({seconds: 45}));
// or
const newDur = dur.plus({seconds: 45}); // better
```

Either way the result is 75 seconds, not 1 minute and 15 seconds.
This seems unexpected.

To create a negated instance, `duration.negated()`.

To create a non-negated instance, `duration.abs()`.

The `toString` instance method returns strings
like `PT2H57M11S` (2 hrs 57 min 11 sec).

### Temporal.Calendar

Supported calendar systems currently only include:

- ISO8601 (same as Gregorian, but adds week numbering)
- Gregorian
- Japanese

By default the ISO8601 calendar is used.

This class and instances of it are not typically accessed directly.
