---
eleventyNavigation:
  key: Overview
  order: 1
  parent: Smalltalk
layout: topic-layout.njk
---

<style>
  .logo {
    --size: 8rem;
    max-height: var(--size);
    max-width: var(--size); 
    margin-left: 2rem;
  }

  .row {
    display: flex;
    align-items: center;
    margin-bottom: 1rem;
  }
</style>

<figure style="width: 30%">
  <img alt="Smalltalk Byte magazine cover"
    src="/blog/assets/smalltalk-byte-cover.jpg?v={{pkg.version}}">
</figure>

The target audience for this series of articles on Smalltalk
is readers that already know at least one programming language.
There are many tables that summarize features of Smalltalk.
These aim to quickly provide a sense of what is available
by including the most useful features and not aiming to be exhaustive.

<a href="https://en.wikipedia.org/wiki/Smalltalk" target="_blank">Smalltalk</a>
"is a purely object oriented programming language (OOP)."
Everything is represented by an object that is an instance of some class.
This includes classes themselves and
all the GUI elements in the development environment.
Everything happens by sending messages to objects.
The objects decide whether and how to act on the messages.

<a href="https://en.wikipedia.org/wiki/Simula" target="_blank">Simula</a>
is considered to be the first object-oriented programming (OOP) language.
In Simula, the data that models a simulation are represented by objects,
but primitive data types like numbers are not.
While Simula preceded Smalltalk, Smalltalk was
the first programming language to make OOP popular.

Running Smalltalk programs requires two parts,
a virtual machine (VM) and an image file.
The VM reads and executes Smalltalk code found in an image file.
It is specific to the operating system and CPU architecture being used.

Image files can be moved between
operating systems on different CPU architectures.
An image will display the same windows, pixel for pixel,
across Windows, Linux, and MacOS, only differing based on screen size.
There is no need to recompile code for different environments.
This makes Smalltalk code highly portable!

Smalltalk is not an interpreted language.
It was the first programming language
to use just-in-time (JIT) compilation.
Smalltalk code is compiled to optimized bytecode that is executed by a VM.
Compilation occurs during program execution rather than before execution.
This results in better performance than interpreting code.

Everything in Smalltalk is represented by an object, including
classes and all GUI elements in the development environment.
An image file can be thought of as a snapshot
of the current state of the development environment.
It describes a collection of all the active objects.
During development, changes can be saved to the current image
or to a new image.

Smalltalk is perhaps most known for its incredible development tools.
These support:

- finding code in many ways
- live environment where code changes are immediately reflected
  (no need to recompile or restart the environment)
- debugging with the ability to modify code and data, then continue or restart
- ability to modify the classes and methods that implement the
  development environment as easily as modifying your own classes and methods
- inspecting objects to see their class and instance variables
- rapid prototyping
- TODO: add more?

Smalltalk is a dynamically typed language.
Types of variables, method parameters,
and method return types are never specified.
Instead, duck typing is used. Any object can be used as long as it
is able to respond to all the messages that are sent to it.
This is determined at run-time.

Alan Kay, Dan Ingalls, and Adele Goldberg worked at
Xerox PARC (Palo Alto Research Center) in the 1970s.
All of them collaborated to create Smalltalk.
Alan Kay was the primary designer of Smalltalk and gave it its name.
Dan Ingalls was the primary implementor.
Adele Goldberg primarily focused on documentation
and promoting Smalltalk outside of PARC.
The original goal was to use Smalltalk to teach programming.

Many other technologies were invented at PARC including
graphical user interfaces, the mouse, drag and drop,
model-view-controller architecture, and virtual machines.
The office metaphor of applying the terms desktop, folder, and file
were also introduced at PARC.

Alan Kay said "OOP to me means only messaging,
local retention and protection and hiding of state-process,
and extreme late-binding of all things."
He also said "I'm sorry that I, long ago,
coined the term 'Objects' for this topic, because it gets
many people to focus on the lesser idea. The big idea is messaging!".

Late binding means that messages sent to objects
are looked up for compatible methods at runtime.
A method name is referred to as a selector.
However, Smalltalk editors do check for "unknown selectors" when
code is entered that sends a message to a literal object (not to a variable).

Smalltalk did not gain much traction outside Xerox Parc until
BYTE magizine published an issue focused on Smalltalk in August 1981.
The cover, shown at the beginning of this article,
featured a colorful hot air balloon.

In the 1990's the popularity of Smalltalk had risen enough
that it was seen as a possible alternative to C++.
For a time, IBM promoted replacing COBOL with Smalltalk.
In 1995, Smalltalk was the second most popular OO language after C++.
Smalltalk had a 15% market share compared to 71% for C++.

At the OOPSLA 1997 conference, Alan Kay said
"Actually, I made up the term 'object-oriented'
and I can tell you I did not have C++ in mind."
He also said "Languages like C++ and Java are OOP done wrong.
Smalltalk is OOP done right."

Today Smalltalk is still used by the financial industry,
manufacturers, utilities, transportation, and academia.

The entire syntax of Smalltalk can be demonstrated on a post card.

<img alt="Smalltalk on a post card"
  src="/blog/assets/smalltalk-on-postcard.jpg?v={{pkg.version}}">

## Why Learn Smalltalk

Some reasons to learn Smalltalk include:

- Gaining an understanding of its pros and cons compared to other languages.
- Getting ideas for features to be added to other languages.
- Getting ideas for features to be added in the
  development environments of other languages.
- Actually using it as an alternative to other languages.

## Resources

- <a href="https://en.wikipedia.org/wiki/Smalltalk"
  target="_blank">Smalltalk in Wikipedia</a>

- <a href="https://archive.org/details/byte-magazine-1981-08"
  target="_blank">Byte Magazine issue on Smalltalk</a>

- <a href="https://cuis.st" target="_blank">Cuis Smalltalk</a>

- <a href="https://cuis-smalltalk.github.io/TheCuisBook/" target="_blank">The Cuis Book</a>

- <a href="https://github.com/Cuis-Smalltalk" target="_blank">Cuis GitHub repositories</a>

- <a href="https://pharo.org" target="_blank">Pharo Smalltalk</a>

- <a href="https://squeak.org" target="_blank">Squeak Smalltalk</a>

- <a href="https://squeak.js.org" target="_blank">SqueakJS</a> -
  "A Squeak VM in JavaScript" by Vanessa Freudenberg

- <a href="https://github.com/Cuis-Smalltalk/Learning-Cuis/blob/master/Quick-UI-Tour.md"
  target="_blank">Quick-UI-Tour</a> for Cuis Smalltalk

- <a href="https://www.fast.org.ar"
  target="_blank">Fundación Argentina de Smalltalk</a> (FAST)

- <a href="https://www.gnu.org/software/dr-geo/" target="_blank">Dr. Geo</a>

  "A program to design and manipulate interactive geometric sketches.
  It helps kids to explore geometry."

- <a href="https://www.goodreads.com/shelf/show/smalltalk"
  target="_blank">Smalltalk Books</a> list on goodreads.

- <a href="https://www.youtube.com/playlist?list=PL6601A198DF14788D"
  target="_blank">Squeak from the very start</a>
  YouTube videos by Lawson English

There is a <a href="https://lists.cuis.st/mailman/listinfo/cuis-dev"
target="_blank">Cuis Smalltalk mailing list</a>,
but no Discord or Slack channel.

There is a <a href="https://discord.gg/43VEWSw2"
target="_blank">Discord channel channel for Squeak Smalltalk</a>.

The following recent podcast episodes discuss Smalltalk:

- <a href="https://www.youtube.com/watch?v=sokb6zZC-ZE&t=3105s"
  target="_blank">Cuis Smalltalk and the History of Computing’s Future</a>
  with Juan Vuletich
- <a href="https://podcasts.apple.com/us/podcast/software-unscripted/id1602572955?i=1000656742775"
  target="_blank">A Haskller Tries Smalltalk</a> with Ian Jeffries
- <a href="https://podcasts.apple.com/us/podcast/software-unscripted/id1602572955"
  target="_blank">Smalltalk's Past, Present, and Future</a> with Juan Vuletich

## Pros and Cons

Smalltalk has the following pros:

- It has a small, consistently applied syntax.
- It has a great development environment consisting of tools such as
  System Browser, Workspace, Transcript, Method Finder, Debugger,
  Hierarchy Browser, Protocol Browser, and more.
- Everything is an object.
- All methods are invoked through message passing
  which supports polymorphism.
  Any kind of object can be passed as a message argument
  as long as it responds to the messages that will be sent to it.
- It provides automatic garbage collection to free the space
  used by objects when they are no longer referenced.
- It provides versioning and development history.
- It has a great web app. framework (Seaside) and a great CMS framework (Pier).

Smalltalk has the following cons:

- It isn't as popular as many other programming languages.

  - Schools generally don't teach it.
  - Few jobs using it are available.
  - IT press rarely talks about it.
  - It's difficult to convince others to use it.

- Help is limited.

  There are fewer developers using Smalltalk
  than many other programming languages.
  This means there are fewer people available
  to answer questions for new developers.

- Library documentation is lacking.

  Many Smalltalk libraries have little to no documentation and example code.
  There seems to be a feeling that since the source code is easily accessible,
  developers can just read it to determine how to use a library.
  This makes it difficult to get started using new libraries.

- Interfacing with resources is challenging.

  Interfacing with resources outside of the Smalltalk environment
  can be difficult. Examples include accessing databases
  and operating in cloud environments with tools like AWS and Docker.

- Team development is challenging.

  Resolve conflicts in code edited by multiple developers
  may require working with the "chunk format".

- Classes are global and not in namespaces, so all class names must be unique.

  Using class name prefixes is sometimes recommended.
  These use 1, 2, or 3 capital letters.
  Prefixes are important for packages of classes and methods
  intended to be reused by others.
  Squeak has a <a href="http://wiki.squeak.org/squeak/3318"
  target="_blank">prefix registry</a> in its wiki.
  Unfortunately it was last updated in 2010.

  There is an effort to add module support
  which would scope class names to a module.
  See <a href="http://haver.klix.ch/"
  target="_blank">HaverOnCuis: A Cuis based Smalltalk With Modules</a>.

- Many errors are only caught at run-time.

  The use of late binding for resolving message sends means that
  there are more errors that can only be detected at run-time
  than in statically typed languages such as C++, C#, and Java.
  However, Smalltalk does do incremental compiling when methods are saved,
  so it finds syntax errors before runtime, unlike most scripting languages.

- Immutability is not favored.

  While it is possible to define Smalltalk classes whose objects are immutable,
  this is not common.
  The lack of focus on immutability will feel wrong
  to developers that prefer functional programming.

- Application deployment is tedious in free versions of Smalltalk.

  Tools to strip a Smalltalk image of developer-only features
  in order to create an image that is suitable for deployment are lacking.
  This is a highly manual process.

  Commercial Smalltalks semi-automatically upgrade application code
  as their base implementation features change over time.
  See the VAST, GemStone, and Circom Smalltalk implementations.
  TODO: Do they provide tooling to strip out classes that are only used
  in the development environment so what remains in the image is
  only what is needed to run the application being developed?

- The image file can be large.
  The base image for Cuis Smalltalk is 19 MB, but installing
  optional packages can easily increase the size to around 200 MB.
  However, it is common to store custom code and modifications to
  provided classes in a "file out" or package that can be
  shared with other developers and installed into fresh images.

## History

- Smalltalk-71

  This was a product of research led by
  Alan Kay at Xerox Palo Alto Research Center (PARC).
  It was created by Alan Kay in just a few days.

- Smalltalk-72

  This version influenced the actor model
  that is used by some modern programming languages.
  From this point on, most of the implementation was provided by Dan Ingalis.

- Smalltalk-76

  This version added most of GUI tools present in current versions of Smalltalk.

- Smalltalk-80

  This version added support for metaclasses of everything,
  including classes, so everything could be treated as an object.
  This was the first version of Smalltalk that was shared outside of PARC.

- ANSI Smalltalk

  This became the standard language reference for Smalltalk in 1998.

- Squeak Smalltalk

  The first release of Squeak Smalltalk was in October, 1996.
  See <a href="http://files.squeak.org/docs/OOPSLA.Squeak.html"
  target="_blank">Back to the Future</a> -
  The Story of Squeak, A Practical Smalltalk Written in Itself

- Pharo Smalltalk

  The first release of Pharo Smalltalk was in March, 2008.
  See <a href="https://en.wikipedia.org/wiki/Pharo"
  target="_blank">Wikipedia</a>.

- Cuis Smalltalk

  The first release of Cuis Smalltalk was in March, 2009.
  See <a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/CuisHistory.md"
  target="_blank">A short history of Cuis</a>.

## Implementations

The following implementations of Smalltalk
were created after those listed above in the "History" section.

- Squeak

  This was the first popular implementation that was free and open source.

- VisualWorks

- ObjectWorks

- ParcPlace Systems for Unix and Sun systems

- Digitalk for Windows and OS/2 systems

- Enfin

- Cincom

- GemTalk

- <a href="https://en.wikipedia.org/wiki/Etoys_(programming_language)" target="_blank">Etoys</a>

  "Etoys is a child-friendly computer environment and object-oriented
  prototype-based programming language for use in education."
  It was used in the One Laptop per Child (OLPC) project.

- GNU Smalltalk

  This uses text files for code rather than
  providing a GUI environment that uses an image file.

- <a href="https://amber-lang.net" target="_blank">Amber</a>

  This is a language that is "deeply inspired by Smalltalk".
  It compiles to JavaScript.

- <a href="https://github.com/dolphinsmalltalk" target="_blank">Dolphin Smalltalk</a>,
  only for Microsoft Windows

- <a href="https://www.gnu.org/software/smalltalk/" target="_blank">GNU Smalltalk</a>

- <a href="https://github.com/nikboyd/hoot-smalltalk#hoot-smalltalk" target="_blank">Hoot Smalltalk</a>

  This runs on the Java Virtual Machine and uses some Java features.

- <a href="https://www.cincomsmalltalk.com/main/" target="_blank">Cincom Smalltalk</a>

- <a href="https://www.instantiations.com/vast-platform/" target="_blank">VAST Platform</a>

  VAST stands for "VisualAge SmallTalk" and is provided by Instantiations.

- <a href="https://gemtalksystems.com/products/gs64/" target="_blank">GemStone/S</a>

- Pharo - forked from Squeak with goal to be more comprehensive

- Cuis - forked from Squeak with goal to remain small and easy to learn

The most popular open source Smalltalk implementations include the following:

- <div class="row">
    <a href="https://squeak.org" target="_blank">Squeak (2,832 classes)</a>
    <img alt="Squeak Smalltalk log" class="logo"
      src="/blog/assets/squeak-smalltalk-logo.svg?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://pharo.org" target="_blank">Pharo (10,405 classes)</a>
    <img alt="Pharo Smalltalk log" class="logo"
      src="/blog/assets/pharo-smalltalk-logo.png?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://cuis.st" target="_blank">Cuis (675 classes)</a>
    <img alt="Cuis Smalltalk log" class="logo"
      src="/blog/assets/cuis-smalltalk-logo.png?v={{pkg.version}}">
  </div>

Squeak, Pharo, and Cuis all use the MIT license.
Both Pharo and Cuis began as forks of Squeak
after maintenance of Squeak was turned over to the community
and there was a lack of concensus on its future goals.

The number of predefined classes in each implementation above
were obtained by printing the result of `Smalltalk allClasses size`
with latest versions as of June 10, 2024.

## Commerial Applications

Commercial applications built with Smalltalk are listed at:

- <a href="https://gemtalksystems.com/about/customers/" target="_blank">GemTalk Systems</a>
- <a href="https://web.archive.org/web/20070101092404/http://wiki.cs.uiuc.edu/VisualWorks/Commercial+projects+that+use+Smalltalk" target="_blank">VisualWorks</a>

## Cuis Smalltalk

This blog primarily focuses on Cuis Smalltalk and running in macOS.
Most keyboard shortcuts in macOS use the command key, abbreviated as "cmd".
In Linux and Windows, the control key is used instead, abbreviated as "ctrl".

Cuis Smalltalk was created by Juan Vuletich who
has been active in the Smalltalk community since 1997.
Juan began work on Cuis Smalltalk began in 2005
and version 1.0 was released in March 2009.

The objectives of Cuis Smalltalk are to:

- Strive for the simplicity of Smalltalk-80.
- Include a minimal number of classes required for the kernel,
  a GUI framework, and development tools,
  removing everything that is not essential.
- Create a system that is good for learning and experimenting.

Any missing features can be defined by combining the features that are present
and by installing more packages.

Some advantages that Cuis has over Squeak and Pharo are that
it has built-in support for Unicode and TrueType fonts.
Those can be added to Squeak and Pharo through foreign libraries.
Cuis also adds support for high quality vector graphics and
rendering Scalable Vector Graphics (SVG).

Cuis Smalltalk uses the same VM as Squeak Smalltalk, now called the
<a href="https://opensmalltalk.org"
target="_blank">Open Smalltalk Virtual Machine</a>.
Pharo Smalltalk has its own VM which is a fork of the Open Smalltalk VM.

The Cuis Smalltalk base image is almost entirely implemented in Smalltalk.

The Cuis mascot is southern mountain cavy which is a "tailless rodent with
short, speckled, greyish-yellow fur, fading to pale grey on the underparts."
They look similar to a mouse, but grow to around eight inches in length.
They are found in Argentina.
Juan Vuletich began development of Cuis Smalltalk in Buenos Aires, Argentina.
The word "cuis" means "squeak" in Rioplatense Spanish.

## GitHub Account

The GitHub account for Cuis Smalltalk is at
<a href="https://github.com/Cuis-Smalltalk" target="_blank">Cuis-Smalltalk</a>.
This hosts many related repositories including the main one
that holds the latest code for the Cuis Smalltalk image,
<a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev"
target="_blank">Cuis-Smalltalk-Dev</a>.
As of May 2024, 96.8% of the code in the repository is written in Smalltalk.
Other related repositories include:

- AMQP: messaging client library compatible with RabbitMQ and other AMQP servers
- AnimatedGIF: supports animated GIFs
- Cairo: supports Cairo graphics
- Cuis6-2: stable release
- Cuis7-0: stable release
- Cuis-Smalltalk-UI: collection of Morphic components
- Cuis-Smalltalk-Tools: collection of additional development tools
- Cuis-Smalltalk-Regex: code for parsing and evaluating regular expressions
- Cuis-Website: a Jekyll site that uses Markdown files
  to generate the Cuis website
- DatabaseSupport: implementation of the Open Database Connectivity (ODBC) standard
  for access databases that support ODBC. This includes
  MariaDB, Microsoft SQL Server, MongoDB, MySQL, Oracle, PostgreSQL, SQLite,
  and many more.
- Games: collection of games implemented in Smalltalk including
  Anagram Aid, Construction, Life, Solitaire (FreeCell and Klondike),
  and classes that can be used to build your own games
- GeographicInformationSystems: supports cartography (maps)
  and includes some data sets
- LearningCuis: learning "tours" of various Cuis Smalltalk aspects
  such as the development environment (UITour) and Morphic layout (LayoutTour)
- Machine-Learning: code for machine learning in Smalltalk
- Measures: code for operating on measurements that combine a number and a unit
  such as 2 meters, 17 celcius, 3 days, and 4 dollars
- Numerics: code for image processing, linear algebra, numerical methods,
  probability distribution, signal processing, statistics, and 3D operations
- OSProcess: provides access to operating system functions, including pipes,
  child process creation, and control of Squeak VM processes
- Parsers: for parsing Smalltalk source code (TODO: Verify this.)
- Styled Text Editor: a framework for rich text editing using styles seen in
  word processors like Apple Pages and Microsoft Word
- SVG: code for reading and displaying Scalable Vector Graphics files
- TheCuisBook: a free book about Cuis Smalltalk
- VMMaker: port of the Squeak VMMaker for building VM plugins

Many of these repositories define optional packages.
To install them, clone their repository into the same directory
as the Cuis-Smalltalk-Dev directory (or Cuis6-2 or Cuis7-0).
Then install them using one of the approaches described in
the "Packages" section of the "Saving Code" chapter.

For additional packages, search GitHub for
repositories whose names begin with "Cuis-Smalltalk-".
