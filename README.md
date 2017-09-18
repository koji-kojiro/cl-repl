# CL-REPL - Common Lisp REPL for Roswell

[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://github.com/koji-kojiro/cl-repl/blob/master/LICENSE)
[![GitHub tag](https://img.shields.io/github/tag/koji-kojiro/cl-repl.svg?style=flat)](https://github.com/koji-kojiro/cl-repl/releases)
[![Quicklisp dist](http://quickdocs.org/badge/cl-repl.svg)](http://quickdocs.org/cl-repl/)  
:jp: [For Japanese](README_jp.md)


<br>
<p align="center">
  <img src="https://github.com/koji-kojiro/cl-repl/blob/master/image/cl-repl.gif">
</p>


# Overview

CL-REPL is a full-featured repl implementation designed to work with **[Roswell](https://github.com/roswell/roswell/)**, which is strongly inspired by **[SLIME](https://github.com/slime/slime)** and **[IPython](https://github.com/ipython/ipython)**.

## Features

- Emacs-like key bindings.<br>
- Tab-completion.
- Shell commands and magic commands.

Of course, the other general features are available! (e.g. debugger)

# Requiremnets
- [Roswell](https://github.com/roswell/roswell/)
- Steel Bank Common Lisp (bundled to Roswell)
- GNU Readline

If you are using Homebrew on MacOSX, you may need to link the library yourself as follows.

```
$ brew link --force readline
```

***[Note]:Since v.0.3.0, CCL and ACL are no longer supported. This means that CL-REPL always uses `sbcl-bin` even if you switch implementation using` ros use`.***  
***[Note]:Terminal should support ANSI escapes.***

# Installation

Via Roswell.<br>
`$ ros install koji-kojiro/cl-repl`

# Usage
`$ cl-repl`

or,

`$ ros repl`

```
$ cl-repl --help
A full-featured Common Lisp REPL implementation.

Usage:
  cl-repl [OPTIONS]

Options:
  --help        Show this screen and exit.
  --version     Show the version info and exit.
  --load <file> Load <file> when startup.

```

## debugger
When a condition is caught by REPL, the debugger starts up. You can select a restart type from three kinds of candidates. Please enter the number corresponding to the desired restart type and hit enter key. You can also execute any codes before restarting.

### Restart type
- [0]. Try evaluating again.
    The debugger will try to evaluating the code again.
- [1]. Return to top level.
    The debugger will be aborted without re-evaluating the code.
- [2]. Edit code.
    The text editor specified by the environment variable `EDITOR` will start.
    You can rewrite code with the editor.

## Shell commands (`![commands]...`)

If the line starts with "!", excute it as shell commnads.

```
CL-USER> !ls
a.txt
CL-USER> !ls -a
.
..
.hidden-file
a.txt
CL-USER>
```

## Magic commands

Some useful magic commands are available. All magic commands have name prefixed `%`.

### `%edit <file>`
Start editing the code with the text editor specified by the environment variable `EDITOR`. When `<file>` is not given, a temporary file will be created.

We tested the following text editors.

| Editor | Result |
|:----------:|:-----------:|
| GNU Emacs | OK! |
| GNU Emacs (emacs -nw) | OK! (recommended)|
| [lem](https://github.com/cxxxr/lem) | OK!  (recommended)|
| vim | OK! |
| Joe's own editor | OK! |
| GNU nano | OK! |
| Atom | NG... |
| Sublime Text3 | NG... |

We recommend to add the line `export EDITOR="emacs -nw -q"` to your `.bashrc`. We recommend [lem](https://github.com/cxxxr/lem) also, because it's very lightweight and highly optimized for Common Lisp.

***NOTE: The environment variable `EDITOR` is quite widely used (e.g. crontab -e, git commit...). So be careful with configuration.***

### `%load <systems>...`

Alias of `(ql:quickload systems... :silent t)`

### `%save [file]`

Save input history into `[file]`.

### `%time [expression]`

Measure execusion time of `[expression]`.

## Introspection (`?[symbol's name]`)

IPython style introspection is available.

# License

CL-REPL is distributed under [MIT license](LICENSE).

# Contributing
Don't hesitate to open issues or to send pull requests!  
The authors are unfamiliar to Common Lisp, as well as English...

# Author

[TANI Kojiro](https://github.com/koji-kojiro) (kojiro0531@gmail.com)
