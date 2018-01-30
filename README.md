# CL-REPL
[![Build Status](https://travis-ci.org/koji-kojiro/cl-repl.svg?branch=master)](https://travis-ci.org/koji-kojiro/cl-repl)
[![License](http://img.shields.io/badge/license-GPLv3-blue.svg?style=flat)](https://github.com/koji-kojiro/cl-repl/blob/master/LICENSE)
[![GitHub tag](https://img.shields.io/github/tag/koji-kojiro/cl-repl.svg?style=flat)](https://github.com/koji-kojiro/cl-repl/releases)
[![Quicklisp dist](http://quickdocs.org/badge/cl-repl.svg)](http://quickdocs.org/cl-repl/)

# **Warning; WIP**
The software is still alpha quality.  
The functionalities are incomplete and may cause unkown bugs.

# Overview
This project aims to provide a beginner-friendly REPL for Common Lisp with rich functionalities, such as IPython for Python.

What this project tries to achieve are listed here.

- [x] powerful line editting with gnu readline.
- [x] tab-completion of symbols.
- [x] simple installation instruction.
- [x] code editting with text editor.
- [ ] useful debugger & inspector. (incomplete)
- [ ] syntax highlighting of input texts. (not available)
- [ ] implementation independence. (only SBCL supported)

# Installation
We recommend to use roswell.  
CL-REPL can be installed via roswell as follows.

```
$ ros install koji-kojiro/cl-repl
```

Before installation, please ensure that gnu readline is installed.  
If you use OSX, you might need to execute following command.

```
$ brew link --force readline
```

Also, ensure that your terminal support 256 colors.

# Usage
```
$　cl-repl
```

or

```
$ ros repl
```

Some useful magic commands are ready to use. To list available commands:

```
CL-USER> %help
```

## execute shell
If the line starts with `!`, excute it as shell command, e.g. `!ls -a`.

## %edit magic
Line editting in repl is sometimes painful. CL-REPL allows yot to edit code with your favorite text editor. 

```
CL-REPL> %edit <filename>
```

CL-REPL invokes a text editor specified by `$EDITOR`.  
After editting code, save and close it. Then repl will start to evaluate it.  
If `<filename>` is not supplied, a temporary file will be created and deleted after evaluation.  

We've be sure the following editors work properly.  

- vi & vim
- GNU Emacs
- joe's own editor
- Lem

# Contributing
Don't hesitate to open issues or to send PRs.  
Any suggestions are always welcomed.

# Author
[TANI Kojiro](https://github.com/koji-kojiro) (kojiro0531@gmail.com)

# License
CL-REPL is distributed under GPLv3.





