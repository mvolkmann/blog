---
eleventyNavigation:
  key: Python os module
  order: 1.4
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

The `os` module in the Python standard library (from the docs)
"provides a portable way of using operating system dependent functionality".
The most commonly used features are demonstrated below.

In each of the code snippets that follow,
the following imports may be needed:

```python
from datetime import datetime
import os
from pathlib import Path
import stat
```

Use the `dir` function to get a list of
all the attributes and methods in a module.
For example:

```python
print(dir(os))
print(dir(os.path))
print(dir(os.path.sys))
```

## Get and change current working directory

```python
# Print current working directory.
print(os.getcwd())

# Change current working directory.
os.chdir('/Users/Mark') # absolute path
os.chdir('..') # relative path to parent directory
```

## Get files and directories

```python
# List files and directories in current directory.
print(os.listdir())

# Get list of filenames in current directory with a .csv file extension.
csv_files = filter(lambda name: name.endswith('.csv'), os.listdir())
print(list(csv_files))
```

## Create/remove/rename files and directories

```python
# Make (create) a new directory.
# This raises FileExistsError if it already exists.
os.mkdir('foo')

# Make a directory tree where any of them may already exist.
# This raises FileExistsError if the last
# directory name in the path already exists.
os.makedirs('foo/bar/baz')

# Remove a single directory.
# This raises OSError if the directory is not empty.
# Intermediate directories are not removed.
os.rmdir('foo/bar/baz')

# Remove a directory tree working from bottom to top.
# Intermediate directories are removed if they are empty.
os.removedirs('foo/bar/baz')

# Touch to create an empty file.
Path('test.csv').touch()

# Remove a file.
# This raises FileNotFoundError if the file does not exist.
os.remove('text.csv')

# Rename a file or directory.
# This raises FileNotFoundError if the file does not exist.
os.rename('test.csv', 'renamed.csv')
```

## Recursively walk a directory

The `os.walk` method returns a generator
that recursively walks a given directory.
Each yielded value is a tuple that contains:

- `dirpath`: directory path
- `dirnames`: list of all directories found at `dirpath`
- `filenames`: list of all files found at `dirpath`

By default this does not follow symbolic links,
but it will if `followlinks=True` is passed.

```python
# Walk a directory tree to find all the .py files.
cwd = os.getcwd()
for dirpath, dirnames, filenames in os.walk(cwd):
    #print('dirnames =', dirnames)
    #print('filenames =', filenames)
    py_files = list(filter(lambda name: name.endswith('.py'), filenames))
    if py_files:
        print(dirpath, 'contains', py_files)
```

When only files whose names match some pattern will be processed,
another approach is to use the standard library `glob` module.

The `glob` method returns a list of matching relative file paths.
The `iglob` method returns an iterator so all the matching
relative file paths don't need to be stored before iteration begins.

In patterns passed to glob methods:

- `?` matches any single character.
- `*` matches any run of characters.
- `**` matches any subdirectory tree, including the current directory.

For example:

```python
import glob

# Match all Python source files in and below the current directory.
PATTERN = '**/*.py'

files = glob.glob(PATTERN)
print(files)

for file in glob.iglob(PATTERN):
    print(file)
```

## Get environment variables

```python
# Get all environment variable names and values.
print(os.environ) # prints a dict

# Get the value of a specific environment variable.
dir_path = os.environ.get('PYTHON_DIR')
```

## Write and read files

```python
# Build a file path using an environment variable.
dir_path = os.environ.get('PYTHON_DIR')
FILE_NAME = 'test.csv'
# Using os.path.join instead of string concatenation
# ensures correct handling of path separator characters.
file_path = os.path.join(dir_path, FILE_NAME)

# Create a CSV file.
with open(file_path, 'w') as f:
    f.write('name,breed\n')
    f.write('Comet,Whippet\n')

# Read the file all at once.
with open(file_path, 'r') as f:
    contents = f.read()
    lines = contents.split('\n')
    print('lines =', lines)

# Read the file one line at a time.
with open(file_path, 'r') as f:
    while True:
        line = f.readline()
        if not line:
            break
        print('line =', line, end='') # omits newline at end
```

## Get information about files

The `os.stat` method returns an object with many fields.

- `st_atime` is the timestamp at which the file was last accessed.  
   On macOS, this is not updated simply by cat'ing a file.
- `st_ctime` is the timestamp at which the file was created.
- `st_mtime` is the timestamp at which the file was last modified.
- `st_dev` is the unique identifier for the device on which the file resides.
- `st_gid` is the group id of the file owner.
- `st_ino` is the OS-specific unique identifier for the file.
- `st_mode` encodes the file type and mode (permissions)
- `st_nlink` is the number of hard links to the file (often 1).
- `st_size` is the size in bytes.
- `st_uid` is the user id of the owner.

```python
# Get statistics for a file or directory.
stats = os.stat('os/test.txt')
print(stats)

print('modified at', datetime.fromtimestamp(stats.st_mtime))
mode = stats.st_mode
print('is directory?', stat.S_ISDIR(mode))
print('is regular file?', stat.S_ISREG(mode))

def have_permission(stats, permission):
    return stat.S_IMODE(stats.st_mode) & permission == permission
can_read = lambda stats: have_permission(stats, stat.S_IREAD)
can_write = lambda stats: have_permission(stats, stat.S_IWRITE)
can_execute = lambda stats: have_permission(stats, stat.S_IEXEC)

print('can read =', can_read(stats))
print('can write =', can_write(stats))
print('can execute =', can_execute(stats))

# Using os.path ...

print('basename =', os.path.basename(file_path)) # test.csv
print('dirname =', os.path.dirname(file_path)) # PYTHON_DIR value
print('split =', os.path.split(file_path)) # tuple of PYTHON_DIR value and test.csv
print('splitext =', os.path.splitext(file_path))
# tuple of PYTHON_DIR value + file name
# and file extension including leading dot

print('exists =', os.path.exists(file_path)) # True
print('isdir =', os.path.isdir(file_path)) # False
print('isfile =', os.path.isfile(file_path)) # True
print('islink =', os.path.islink(file_path)) # False

print('size =', os.path.getsize(file_path), 'bytes')

atime = os.path.getatime(file_path)
ctime = os.path.getctime(file_path)
mtime = os.path.getmtime(file_path)
print('atime =', atime, datetime.fromtimestamp(atime))
print('ctime =', ctime, datetime.fromtimestamp(ctime))
print('mtime =', mtime, datetime.fromtimestamp(mtime))
```

## Get OS information

```python
print('pathsep =', os.path.pathsep) # : on macOS
print('sep =', os.path.sep) # / on macOS
print('platform =', os.path.sys.platform) # darwin on macOS
print('version =', os.path.sys.version) # 3.8.5 - version of Python
print('version_info =', os.path.sys.version_info)
# tuple containing major, minor, micro,
# and releaselevel of Python executable
```
