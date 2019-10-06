# mal

This is implementation of the [mal](https://github.com/kanaka/mal) programming language. I have not merged them into original repository because csharp and scheme implementations are already there.

This repository contains two implementations: one is implemented in [gambit scheme](http://gambitscheme.org), another one in [C# for dotnet core 2.0](https://dotnet.microsoft.com). I have developed them on **ubuntu mate** linux version 18.04.

Both implementations follows instructions in the ["The Make-A-Lisp Process"](https://github.com/kanaka/mal/blob/master/process/guide.md) guide but there are some differences from original:

_**csharp**_

* only one application is created which implements all steps. It recognize argument
 -step. For example: `run -step step1`
* integer and real numbers are implemented
* **quit** function is implemented
* key for hash-map can be string, keyword and also symbol
* symbols are unique - for every **symbol name** only one **Symbol** object is created

_**gambit**_

* full numeric tower of the gambit language is used
* key for hash-map can be string, keyword, symbol and integer number
* expression for the key of a hash-map is evaluated too.
  Therefor it is possible to write: `{(str 'a 'b) "qwerty"}`

Both implementations have implemented some default macros for simplifying definitions (look into [defines.mal](https://github.com/pcerman/mal/csharp/defines.mal) file)

Subfolder [lib](https://github.com/pcerman/mal/lib/) contains three files which implements some useful functions and macros. For example there are defined simple **trace** and **untrace** functions.

## License

This code is released under **Mozilla Public License 2.0**

Copyright (c) 2019 Peter Cerman (<https://github.com/pcerman>)
