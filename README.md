# CL-REPL - Common Lisp REPL for Roswell

[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](LICENSE)<br>

<p align="center">
  <img src="image/cl-repl.gif">
</p>

# Overview

CL-REPL is a full-featured repl implementation designed to work with **[Roswell](https://github.com/roswell/roswell/)**, which is strongly inspired by **[SLIME](https://github.com/slime/slime)** and **[IPython](https://github.com/ipython/ipython)**.

## Features

- Portable.<br>
  Written as a **[Roswell script](https://github.com/roswell/roswell/wiki/2.-Roswell-as-a-Scripting-Environment)**.
- Emacs-like key bindings.<br>
  Provides powerful line editor on REPL.
- Tab-completion.
- Shell commands and magic commands.

Of course, the other general features are available! (e.g. debugger)

## Surpported Lisps

- Steel Bank Common Lisp (Recommended)
- Clozure CL
- Allegro CL

We mainly target SBCL and CCL. It might work even on the other implementations, but not guarantied.

# Installation

Via Roswell.<br>
`$ ros install koji-kojiro/cl-repl`

# Usage
`$ cl-repl`

or,

`$ ros repl`

## debugger
When a condition is caught by REPL, the debugger starts up. You can select a restart type from three kinds of candidates. Please enter the number corresponding to the desired restart type and hit enter key. You can also execute any codes before restarting.

### Restart type
- [0]. Try evaluating again.
    The debugger will try to evaluating the code again.
- [1]. Return to top level.
    The debugger will be aborted without re-evaluating the code.
- [2]. Edit code. (Only for SBCL)
    The text editor specified by the environment variable `EDITOR` will start.
    You can rewrite code with the editor.

## Shell commands (![commands]...)

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

Some useful magic commands are available. All magic commands have name prefixed "%".

### %edit (for SBCL only)
Start editing the code with the text editor specified by the environment variable `EDITOR`.
Currently, supported only for SBCL.

### %load [systems]...

Alias of `(ql:quickload systems... :silent t)`

### %save [file]

Save input history into [file].

### %time [expression]

Measure execusion time of [expression].

## Introspection (?[symbol's name])

IPython style introspection is available.

# License

CL-REPL is distributed under [MIT license](LICENSE).

# Author

[TANI Kojiro](https://github.com/koji-kojiro) (kojiro0531@gmail.com)
