---
eleventyNavigation:
  key: UNIX Notes
layout: topic-layout.njk
---

## Overview

This is a collection of tips on using UNIX and Linux systems.

## ASCII characters

To see a list of all the ASCII characters and their codes,
enter `man ascii`.

To dump the ASCII characters in a text file, enter
`od -cx {file-path}`.

The `-c` option causes it to print ASCII characters
using backslash escapes for control characters.
The `-x` option causes it to output inverted pairs of hex ASCII codes.

## Awk

This command can replace given text in a set of files with new text.
For example, the following replaces"foo" with "bar"
in all the `.java` files in the current directory.

```bash
for file in \*.java; do
  mv $file $file.orig
  awk -e '{ if ($0 == "foo") print "bar"; else print $0 }' $file.orig > $file
done
```

Also see `MyUnixEnv/notes/AwkNotes.txt`.

## bc line-oriented calculator

- To convert decimal numbers to hex
  obase=16
  enter any decimal number and it will be output as hex
- To convert hex numbers to decimal
  ibase=16
  enter any hex number and it will be output as decimal
- To exit, enter "quit".

## Creating Directories

To create directories whose parent directories don't exist yet,
enter a command like `mkdir -p dir1/dir2/dir3`.

## Curl

Ampersands separating query parameters must be preceded by backslashes.

Surround url in single or double quotes if it contains special characters
like double quotes.

To specify a request body for a POST or PUT, use `-d '{body-payload}'`

To use a verb other than GET which is the default, add `-X{verb}`.
For example, `curl -XPUT http://localhost:3000 -d '{body-payload}'`

To specify the body content type, add `-H "Content-Type: {type}"`.
An example type is `application/json`.

To put data from a text file:

```bash
curl -XPUT http://localhost:3000 -H"Content-Type: application/json" --data @security.json
curl http://localhost:3000/security/admin/rights/%2F # verifies
```

To put data from a binary file

```bash
curl -XPUT http://localhost:3000 -H"Content-Type: image/png" --data-binary @{file-path}
```

To specify output to be added after successful output, add `-o`.
For example, to add a newline:

```bash
curl -w'\n' http://java.ociweb.com/mark/
```

To see headers, `curl -i {url}`.

To send a HEAD request, add `--head` or `-I`, not `-XHEAD`.

## Default Shell

To change the default shell, enter `ypchsh`.

## Diff

- To get lines missing in first file and present in second file
  - verify that the two files are sorted
  - comm -13 file1.txt file2.txt
  - if the lines are not sorted
    - diff --new-line-format="gronk:%L" | grep "^gronk" | sed 's/gronk://'
      where "gronk" is just some unique string
      that is used to filter out only the relevant lines
      and sed is used to remove that unique prefix

## Disk Space

- To determine top offenders in disk space usage,
  du -sk \* | sort -rn | head
  -s = summarize
  -k = --blocksize=1K
  -r = reverse
  -n = sort as numeric
  head defaults to showing only first 10
  (really slow!)

- To list the files in the current directory from largest to smallest,
  ls -lS

- To find the largest files in and below the current directory,
  find . -type f -size +1M -exec ls -lk {} \; | sort -nr -k 5 | head
  The "-size +1M" limits processing to files that are at least one megabyte.
  The "-nr" options for sort are numeric-sort and reverse.
  This dramatically improves performance.

## Display

- To allow other Linux boxes to display to yours
  - on yours execute "xhost +"
  - on theirs execute "export DISPLAY={host}:0.0"
    then run application that generates the display
    (at Boeing your host is b878360.stl.mo.boeing.com)
- Auto-scroll
  - When Linux thinks the display aspect ratio differs
    from that of the current resolution setting,
    it auto-scrolls when the mouse moves off an edge of the screen.
    To fix this, change the resolution and then change it back.

## Directory Structure

- To see the content of the current directory
  including subdirectories as a tree, enter "tree".
- install on Mac with "brew install tree"

## Environment Variables

- To set in bash,
  export name=value
- To unset in bash,
  unset name

## Finding/Searching Files

There are multiple commands that search files,
including `find`, `grep`, and `rg` (ripgrep).

### find

To find all files of a given type in and below the current directory
that contain a given string,
`find -name '_.{type}' | xargs grep "{string}"`.
An alternative is `find -name '_.{type}' -exec grep "{string}" {} \;`
Consider using ripgrep instead because it is much faster.

If the first argument starts with `-`
then the search begins in the current directory.
This means that `find . -name 'foo.bar'`
is the same as `find -name 'foo.bar'`.
If `-name` is omitted, it defaults to `-name '\*'`.

To delete all matching files below the current directory,
`find . -name 'name-pattern' -delete`.
`-delete` is much shorter than the alternative `-exec rm {} \;`.

To output only file names containing a match and not the matching lines:

```bash
find -name '\*.{type}' | xargs grep -l "{string}"
```

To find all .h and .cpp files that contain "foo":

```bash
find -regex ".\*[.h|.cpp]" | xargs grep foo
```

To avoid "Permission denied" messages
when find tries to search certain directories:

```bash
find -name 'whatever' 2> /dev/null
```

To find all directories that contain files
that have been modified today:

```bash
find . -ctime -1 -type d
```

To find all files in and below the current directory
whose contents have been modified in the last n days:

```bash
find . -type f -mtime -n
```

To edit all the files that contain a given string:

```bash
vim $(find -name '\*.filetype' | xargs grep -l "string")
```

To find all files that contain text that matches a regular expression,
for example, .h files that define a class that inherits from another (:):

```bash
find . -name '\*.h' -exec grep -E 'class \w+ :' {} \;
```

To find all files with a given extension that are executable
and make them not executable:

```bash
find . -name '\*.{ext}' -perm 001 -exec chmod a-x {} \;
```

The `001` is a mask that specifies the x bit.

To limit the depth of the search (# of directories deep),
add `-maxdepth n`.

### grep

`grep` stands for Global Regular Expression Print.
It came from the `ed` editor command `g/RE/p`
where `RE` is replaced by a regular expression.
It globally operates on every line in the file
and prints all lines that match the regular expression.

To find all files with a given extension in the current directory
that contain a given string, enter `grep '{string}' *.{extension}`.

### ripgrep

{% aTargetBlank "https://github.com/BurntSushi/ripgrep", "ripgrep" %}
implements the `rg` command in Rust
and is a much faster alternative to the `find` command.

To find all occurrences of a string within
any file that is in or below the current directory,
enter `rg '{string}'.
Quotes around the string are only needed if
it contains spaces, other special characters, or a regular expression.

By default `rg` does not search the following files:

- hidden files (whose names begin with `.`)
- files in a Git repository that match patterns
  in its `.gitignore` file
- binary files
- files referenced by symbolic links

To search these files, add `--no-ignore`.
To only add searching of hidden files, add `--hidden` or `-.`.
To follow and search files referenced by symbolic links,
add `--follow` or `-L`.

To limit the search to a specific file,
enter `rg '{string}' {file-path}`.

To limit the search to files that are in and below a given directory,
enter `rg '{string}' {dir-path}`.

To limit the search to files with
a given type (such as rust) or file extension (such as `.r`),
use one of the following:

```bash
rg '{string}' --type rust
rg '{string}' trust
rg '{string}' -g '*.rs'
```

Some file types map to multiple file extensions.
For example, `-tc` is the same as `-g '*.{c.h}'`.
To see all the supported file types, enter `rg --type-list`.
To see the glob pattern used by a given file type,
enter `rg --type-list | rg '^{type}:'`.

## FTP

- See notes on ncftp in CygwinNotes.txt
- Don't forget to enter "binary" before attempting to put binary files.
- ~/.netrc can be used to reduce typing required.
  - To register username/password for specific machines,
    add one line like the following for each machine:
    machine {name-or-IP-address} login {username} password {password}
  - To create macros of FTP commands
    such as updating a file and quitting,
    macdef uploadExe
    lcd <YOUR_EXECUTABLE_DIR>
    binary
    put <EXECUTABLE_NAME>
    quit
    There must be a blank line after the last macdef!
    It may be necessary to place macdefs immediately after
    the machine definitions where they will be used.
    This will require duplicating macros that need to be
    used on multiple machines.
  - To use the .netrc file, enter "ftp {name-or-IP-address}"
  - To execute an FTP command and capture its output in a file
    echo "{command}" | ftp {name-or-IP-address} > ftp.txt
  - To run a macro from an FTP prompt, "${macro-name}".
  - To run a macro from the command line,
    "echo "\${macro-name}" | ftp {name-or-IP-address}
  - To see all the commands that a macro executes
    - if running macro from ftp prompt, start ftp with -d
    - if running macro from command line, start ftp with -v

## Functions

- to see the definition of a function defined in some script
  that has been run (perhaps .bashrc), enter "type {function-name}"

## Graphics files

- to display a graphics file such as a .png file,
  use the "Eye of Gnome" application by entering "eog {file-path}"

## Hex

- To see the hex characters in the output of a command
  {command} | od -cx

## Home Directory Mount

- To force someone else's home directory to be mounted on your Linux box,
  sudo su - {their-id}
  Use your root password.

## Host

- To determine name of current host, enter "hostname".

## IP address

- To determine the IP address of the current host (four ways)
  - google "what is my ip"
  - run /sbin/ifconfig and look for en0 (wired) or en1 (wireless) inet addr
    - on some machines, ifconfig is in /sbin
  - run ipconfig and look for the en0 (wired) or en1 (wireless) inet addr
  - nslookup {hostname}

## LD_LIBRARY_PATH

This environment variable lists directories to be searched
for library files (such as .so files).

## Line Endings

- to convert line endings in files from DOS (CRLF) to UNIX (LF)
  - install dos2unix command (on macOS, brew install dos2unix)
  - open a bash shell
  - cd to a directory containing the files to convert
  - for each file extension to be processed (ex. .js)
    find . -type f -name '\*.js' -exec dos2unix {} \;

## List Directory Contents

- To show hidden files, ls -a
- To show details, ls -l
  - extended attribuets
    - the last position in the permissions string
      contains @ if the file has "extended attributes"
    - to list them, xattr {file-path}
    - to delete one, xattr -d {extended-attr-name} {file-path}
- To sort on creation time with newest files first, ls -lt
- To use color, alias ls='ls --color=tty'
  (directories are blue, executables are green)

## List Library Contents

- Use nm command which outputs lines containing symbol value, type and name.
- To unmangle symbol names for C++ methods, pipe nm output to c++filt.
- The symbol type is output before each symbol in the library:
  A - absolute, won't change by further linking
  B - uninitialized
  C - common
  D - initialized
  G - initialized for small objects
  I - indirect reference to another symbol
  N - debugging symbol (use -a to see these)
  R - read only
  S - uninitialized for small objects
  T - text (code)
  U - undefined
  V - weak object
  W - weach symbol
  - - a "stabs" symbol
      ? - unknown

## Locate

- This command searches a database for a filename
  and outputs all the locations where it resides.
- The database is typically updated every night for all files on the drive.
- Usage: locate {filename}

## Make

- To see commands that make would execute without executing them,
  make -n
- Use MPC to generate makefiles for various platforms

## Man Pages

- to save a man page in a text file that can be printed
  - man {command} | col -b > {command}.txt
- to save it in a much more appealing Postscript format
  - man -t {command} > {command}.ps

## Messages

- To send a message to a particular user that they will see
  at their command prompt, "write {user-name}".
  After that, each line you type is sent to them when you press the enter key.
  They can do the same to you in order to have a conversation.
- To send a message to all users on your current host, "wall {message}".

## Mount

- If your home directory is actually mounted from somewhere else
  (such as an NIS server), to see where, cd to your home directory
  and enter "df .".
  - this is the case at Boeing
- To see all directories that are mounted from somewhere else,
  enter "df | grep :" or "mount | grep :".
- To mount USB drives that are detected by automount,
  - run "dmesg" to see what device name was assigned (ex. sda1 or sdb2)
  - run "ls /media" to see what device names it assigned (ex. WD_Combo or ipod)
  - mount -t vfat /dev/sda1 /media/WD_Combo
  - mount -t vfat /dev/sdb2 /media/ipod
  - there should be a way to make these mounts happen automatically on reboot
    but I haven't figured that out
- /etc/fstab (stands for File System TABle)
  - lists all file system devices that are mounted, and their options
  - is this automatically updated by the mount command?
- automount
  - responsible for dynamically mounting USB devices
  - to stop it, run "/etc/init.d/autofs stop"
  - to start it, run "/etc/init.d/autofs start"

## Netstat

- To see which ports are currently being listened on by some server
  netstat -an | grep LISTEN

## Password

- To change password
  passwd [username]
  To change to a dictionary word, su root and ignore warnings.

## Patch

- To create a patch file,
  diff {file1} {file2} > {name}.patch
- To apply a patch file to a file,
  patch ???

## PATH

- This environment variable contains a list of directories
  Identify areas of concern when using Java threads alone and together with C++ threads.
  Document the behavior of Java threads, how their behavior can be configured, and their interaction with C++ threads.
  Design, implement and document a testing framework for measuring the impact of various implementation and tuning approaches.
  Present all the information gathered in a teleconference.
  Assist NSIT in running the testing framework to gather results on the target platform.
  Modify the testing framework based on feedback from NSIT.
  to be searched for executables.
- After an executable is run once, it's location can be remembered.
- If PATH is changed later, you may have to run "hash -r"
  to get the OS to forget all executable locations it was remembering.

## Ports

- to find the process listening on a given port, use "list open files"
  lsof -i:{port-number}

## Printing

- To get name of default printer,
  lpstat -d
- To print source files using enscript
  alias ens="enscript --borders --columns=2 --fancy-header \
   --landscape --line-numbers=1 --mark-wrapped-lines=arrow \
   --pretty-print=cpp --printer=bstl0860"
  ens {filename}.cpp
- Your "ens" alias is configured to print in two columns
  and "ens1" prints in one column.
- To print selected lines from a file,
  sed -n '{start-line},{end-line}p' {file-path} | ens
  - "p" is for print

## Processes

- $! holds the process id (PID) of the last process that was spawned using &
  - for example,
    some-executable&
    pid=$!
    # do some work
    kill -9 $pid
- to kill all processes with a given name
  - killall {name}
    - useful with flow!
- best way to kill
  - Unless a program was written to catch a specific signal, the best way
    to try to kill it is the Software Termination signal (SIGTERM - 15).
    This is the default signal for the kill command if none is specified.
    syntax: kill {pid}
    Well-written \*nix applications catch this signal and terminate gracefully.
  - If this fails, try these in order.
    - hangup - SIGHUP(1): kill -1 {pid}
    - interrupt - SIGINT(2) (sent by a ctrl-c): kill -2 {pid}
    - quit program - SIGQUIT(3) (causes a core dump): kill -3 {pid}
    - abort - SIGABRT(6): kill -6 {pid} (should also cause a core dump)
    - kill - SIGKILL(9): kill -9 {pid} (uncatchable)
  - If a process won't die and you're curious about why, attach a gdb session.
    After poking around, do a "quit" without detaching
    which should kill the process.
  - Sun's JVM uses SIQQUIT(3) as a signal to dump all the running threads.
- Defunct processes used to be called zombie processes.
  They are neither dead nor alive.
  They are processes that have become corrupted in such a way
  that they no long can communicate with their parent or child process.
  Killing the parent or child process will usually make them go away.
  If the parent or child process is missing, you're out of luck.
  Possibly there is a stuck automount.
  Of course rebooting the machine will get rid of them.

## rsync

- Copies new and modified files to a remote server
- example
  - cd ~/Documents/VolkmannWebSite
  - rsync -a --delete --progress --stats volkmanm@ftp.ociweb.com:/
    - this doesn't work! need to figure out how to supply password
    - -a archive mode which implies these options: rlptgoD
      - r is recursive
      - l copies symlinks as symlinks
      - p preserves permissions
      - t preserves create and modify times
      - g preserves groups
      - o preserves owners
      - D implies --devices and --specials
      - --devices preserves devices?
      - --specials preserves special files?
    - --delete deletes files from remote server that are not present locally
    - --progress shows progress during transfer
    - --states shows file transfer statistics

## Script Command Echoing

- To see each command that is executed in a script named foo,
  sh -x foo

## Secure Copy

- To avoid being prompted for a password, see notes on ssh-keygen below.
- To copy a file to another host,
  scp file-name {user-name}@{host}:{file-path}
- To copy a file from another host,
  scp {user-name}@{host}:{file-path} .
- To copy a directory from another host,
  scp -r {user-name}@{host}:{dir-path} .
- To copy a file to your home directory on another host,
  scp {file-path} {user-name}@{host}:

## Shell Prompt

- To customize shell prompt in bash,
  export PS1="prompt"
  Escape sequences that can be used in prompt include
  \H - host name
  \u - user name
  \w - current working directory
  Recommended prompt is "\w$ ".

## Shell Scripts

- To specify the type of script, in this example using bash,
  make the following the first line.
  #!/bin/bash
- To cause every command in a script that is executed to be echoed,
  add the following near the top of the script.
  set -x
  (also see "set -v")

## SSH

- download PuTTY from http://www.chiark.greenend.org.uk/~sgtatham/putty/
  - need putty.exe, pageant.exe and plink.exe
  - see C:\Program Files\PuTTY on your laptop
- need public and private SSH keys
  - yours are in C:\OCI\OCI*Putty*\*\_key.ppk
- double-click the Pageant icon on your laptop desktop (needed?)
- add the environment variable SVN_SSH="ssh -l volkmanm -P 12345"
- to generate a new SSH key
  - run C:\Program Files\PuTTY\puttygen.exe on laptop
  - select "SSH2 DSA" radio button near bottom of dialog
  - click the "Generate" button
  - move the mouse over the "Key" area
    until the progress bar goes all the way to the right
  - copy the public key by selecting it with the mouse and pressing Ctrl-C
    (using the "Save public key" button saves it in a form
    that the OCI SSH server can't use)
  - save the private key by pressing the "Save private key" button
  - add the public key to ~/.ssh/authorized_keys on the OCI hickory server
- to test that the SSH key is working properly, from a command prompt,
  enter "putty -i {path-to-private-key-file} volkmanm@ssh.ociweb.com"
- to avoid being prompted for a password
  - on source machine
    - enter ssh-keygen (accept all the defaults; don't enter a passphrase)
    - use ftp or scp to copy ~/.ssh/id_rsa.pub to the destination machine
  - on destination machine
    - mkdir .ssh
    - mv id_rsa.pub ~/.ssh/authorized_keys
    - chmod 700 ~/.ssh/authorized_keys

## stdout and stderr

- to redirect both to the same place
  command > file 2>&1
  or (bash only)
  command &> file

## Switch User (su and sudo)

- To switch to the root user,
  enter "su root" or just "su".
- To switch to the root user AND get its enviroment (ex. PATH),
  enter "su - root" or just "su -".
- sudo does a "switch user" and then executes some command
- At Boeing you can use "sudo su" to switch to the root id.
  To verify that it worked, enter "id" which should show that uid is root.

## Symbolic Links

- To create a symbolic link,
  enter "ln -s {target-file} {link-name}"
- Suggested use
  - directories for installed libraries and applications under /opt
    should include version numbers
  - create a symbolic link for each whose name doesn't include the version
  - refer to these symbolic links in applications that use them
  - when new versions are installed, only the symbolic links
    need to be changed, not the applications that use them

## Tar

- To tar the contents of a directory named d
  cd to the parent directory
  tar zcf d.tgz d
  - .tgz is typically preferred over .tar.gz
  - The above is shorthand for the following:
    tar cf d.tar d
    gzip d.tar # creates d.tgz
  - the last two steps can be compbined into one with
- To untar a gzipped tar file f.tgz
  tar zxf f.tgz
  - tar now infers the z option.
  - The above is shorthand for the following:
    gunzip f.tgz # or gzip -d f.tgz
    tar xf f.tar
- To untar a bz2 tar file f.bz2
  tar xvjpf f.bz2
- To tar only files found using the find command:
  tar xf myfiles.tar `find ...`
- See your pkg and unpkg scripts that make it easier to
  tar/zip and unzip/untar directories.

## TERM

- If delete/backspace key doesn't work, you may need to enter
  export TERM=xterm-256color
  - can add this in .bashrc

## Threads

- Some versions of UNIX/Linux don't support thread priorities.
- NPTL stands for New POSIX Thread Library
- RedHat Enterprise 3 uses the 2.4.21 kernel which uses NPTL.
- Fedora probably uses NPTL.
- To enable thread priorities, switch to older thread library with
  export LD_ASSUME_KERNEL=2.4.1
- Have to run as root to assign thread priorities.

## Time

- To find out how long it takes to execute a given command,
  time {command}

## Version

- To get UNIX version,
  uname -a
  Under Linux, the 3rd piece of data is the Linux kernel version.
  At Boeing it's Linux 2.4.21-15.EL and VxWorks 6.1.
- To get RedHat version,
  cat /etc/redhat-release
  At Boeing it's Red Hat Enterprise Linux WS release 3 (Taroon Update 2).
- To get the thread library version,
  getconf GNU_LIBPTHREAD_VERSION
  At Boeing it's NPTL 0.60.
  NPTL stands for New POSIX Thread Library.

## Windows

- To minimize all windows, ctrl-alt-d.
- To cycle between windows, ctrl-alt-tab.

## Zip

- To create a zip file named foo.zip containing
  all the files in and below the directory bar
  zip -r foo bar
- To unzip .bz2 files, use "bunzip2 {name}.bz2".
- To list the files inside a zip file,
  zipinfo foo.zip
