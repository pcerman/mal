# mal

This is implementation of the [mal](https://github.com/kanaka/mal) programming language. I have not merged it into original repository because csharp, cpp and scheme implementations are already there.

This repository contains four implementations: one is implemented in [gambit scheme](http://gambitscheme.org), second one in [C# for dotnet core 3.1](https://dotnet.microsoft.com), third one is implemented in C++ and last one is implemented standard ML [MLton](http://www.mlton.org). Last one is possible to compile it with [PolyML](https://polyml.org) compiler by using [sml-buildscripts](https://github.com/cannam/sml-buildscripts).

I have developed them on **ubuntu mate** linux version 20.04.

All implementations follows instructions in the ["The Make-A-Lisp Process"](https://github.com/kanaka/mal/blob/master/process/guide.md) guide but there are some differences from original:

_**csharp**_

* integer and real numbers are implemented also with predicate and conversion functions.
* key for hash-map can be string, keyword, symbol and integer number
* **quit** function is implemented
* symbols are unique - for every unique **symbol name** only one **Symbol** object is created

_**gambit**_

* full numeric tower of the gambit language is used
* key for hash-map can be string, keyword, symbol and integer number

_**C++**_

* integer and real numbers are implemented also with predicate and conversion functions.
* key for hash-map can be string, keyword, symbol and integer number
* AVL tree is used for hashmap
* simple stop-and-copy garbage collector is imlemented

_**polyml**_

* integer and real numbers are implemented also with predicate and conversion functions.
* key for hash-map can be string, keyword, symbol and integer number
* AVL tree is used for hashmap

All implementations have implemented some default macros for simplifying definitions (look into [defines.mal](https://github.com/pcerman/mal/blob/master/csharp/defines.mal) file)

Subfolder [lib](https://github.com/pcerman/mal/tree/master/lib) contains three files which implements some useful functions and macros. For example there are defined simple **trace** and **untrace** macros.

## License

This code is released under [Mozilla Public License 2.0](https://github.com/pcerman/mal/blob/master/LICENSE).

Copyright (c) 2019 Peter Cerman (<https://github.com/pcerman>)

