# CL-REPL - Common Lisp REPL for Roswell

[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://github.com/koji-kojiro/cl-repl/blob/master/LICENSE)
[![GitHub tag](https://img.shields.io/github/tag/koji-kojiro/cl-repl.svg?style=flat)](https://github.com/koji-kojiro/cl-repl/releases)
[![Quicklisp dist](http://quickdocs.org/badge/cl-repl.svg)](http://quickdocs.org/cl-repl/)

<br>
<p align="center">
  <img src="https://github.com/koji-kojiro/cl-repl/blob/master/image/cl-repl.gif">
</p>

# 概要

CL-REPL は **[Roswell](https://github.com/roswell/roswell/)** と連携するように設計されたCommon Lispの多機能対話型開発環境です．  **[SLIME](https://github.com/slime/slime)** や **[IPython](https://github.com/ipython/ipython)** から非常に多くのアイデアを取り入れています.

## 特徴

- Emacs風のキーバインド．<br>
- タブ補完．
- シェルコマンドとマジックコマンド．

その他の一般的な機能ももちろん実装されています．（デバッガなど）

# 要件
- [Roswell](https://github.com/roswell/roswell/)
- Steel Bank Common Lisp (Roswellについてきます)
- GNU Readline

もしMacOSX上でHomebrewを使っている場合はご自身でライブラリをリンクする必要があるかもしれません．
その場合以下を実行してください．

```
$ brew link --force readline
```

***NOTE:v.0.3.0からCCLとACLはサポートされません. CL-REPLは常に`sbcl-bin`を用います．` ros use`による処理系の切り替えは無視されます．*** <br> ***NOTE:ANSIエスケープシーケンスをサポートした端末でのみ正常に動作します．*** 

# インストール

Roswellからインストールできます．<br>
`$ ros install koji-kojiro/cl-repl`

# 使い方
`$ cl-repl`

または，

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

## デバッガ
コンディションが補足されるとデバッガが起動します．三種類からリスタートを選択できます．対応する番号をタイプしてエンターキーを押してください．リスタートの前に任意のコードを実行することもできます．

### リスタートのタイプ
- [0]. Try evaluating again.
    デバッガは再度評価を試みます．
- [1]. Return to top level.
    デバッガは再評価を試みずに終了します．
- [2]. Edit code.
    環境変数`EDITOR`で指定されたテキストエディタが起動します．<br>
    コードを再度編集することができます．

## シェルコマンド (`![commands]...`)

行頭が"!"であった場合，シェルコマンドとして実行されます．

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

## マジックコマンド

いくつかの有用なマジックコマンドが利用できます．すべてのマジックは`%`で始まる名前を持ちます.

### `%edit <file>`
環境変数`EDITOR`で指定されたエディタが起動し編集を開始します．`<file>`が指定されていない場合は一時ファイルが作成されます．

以下のエディタで動作確認を行っています．

| エディタ | 結果 |
|:----------:|:-----------:|
| GNU Emacs | OK! |
| GNU Emacs (emacs -nw) | OK! (おすすめ)|
| [lem](https://github.com/cxxxr/lem) | OK!  (おすすめ)|
| vim | OK! |
| Joe's own editor | OK! |
| GNU nano | OK! |
| Atom | NG... |
| Sublime Text3 | NG... |

おすすめは`export EDITOR="emacs -nw -q"`を`.bashrc`に追加しておくことです．あるいは[lem](https://github.com/cxxxr/lem)もおすすめです．軽量に動作し，Common Lispに高度に最適化されたエディタです．

***NOTE: 環境変数`EDITOR`は非常に広く用いられています．慎重に設定してください．（例： crontab -e, git commit...）***

### `%load <systems>...`

`(ql:quickload systems... :silent t)`のエイリアスです．

### `%save [file]`

入力履歴を`[file]`に保存します．

### `%time [expression]`

`[expression]`の実行時間を計測します．

## イントロスペクション (`?[symbol's name]`)

IPython風のイントロスペクションが利用可能です．

# ライセンス

CL-REPLは[MITライセンス](LICENSE)のもとで頒布されます．

# Contributing

プルリクエストやissueは常に大歓迎です．
作者はCommon Lispにも英語にも大変不慣れです...

# 作者

[TANI Kojiro](https://github.com/koji-kojiro) (kojiro0531@gmail.com)
