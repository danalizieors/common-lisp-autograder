# common-lisp-autograder
An automatic grader for the Common Lisp lab test. Beyond the fact that it freed me of the burden of correcting Lisp code, it prooved itself useful by interactively leading the students to correct solutions.

The test, including the grader framework, is defined in the `test.lsp` file, and it can be executed with the `clisp test.lsp` command. The students can write their solutions in the `impl.lsp` file.

The program runs entirely in the command line. In Linux terminals it has colored output, if someone wants to use it on Windows the coloring of the output can be turned off by setting a flag defined in the `impl.lsp` file.
![Screenshot](https://user-images.githubusercontent.com/20115656/48316963-2677d000-e5f3-11e8-9d1c-cf0662a8bca3.png)

The exercises provided in the test are not hard-coded in the grader framework, all the data regarding the problems and the solutions can be found under the comment `;;; Define the problems and the soultions` in `test.lsp` file. One can change the exercises to suit their or the students' needs without understanding the grader framework itself, by only understanding the data structure used to define the test. The aforementioned structure won't be discussed, because it is intuitive and can be understood with a bit of tinkering.
