# Overview
This repository contains some common algorithms and data structures written in the Scheme programming language.

# Scheme Implementation
We use ChezScheme for running the code, which means 1) the code usually runs faster than using other implementions and 2) the code is not portable since some parts of the code is ChezScheme-specific. Besides, ChezScheme has very good support for command line editing without which it would be inconvenient to write parentheses.

The code here in general comforms to R6RS. For a general introduction to the Scheme language, see the [R6RS specification](http://www.r6rs.org/). ChezScheme has a [User's Guide](https://cisco.github.io/ChezScheme/csug9.5/) that documents both the general Scheme procedures and ChezScheme-specific ones (you can also build the PDF on your own, see below). For books, I recommend:

- The Scheme Programming Language by R. Kent Dybvig
- Scheme and the Art of Programming by George Springer and Daniel Friedman
- Structure and Interpretation of Computer Programs by Harold Abelson, Gerald Jay Sussman, Julie Sussman

To achieve simplicity, no exception handling is implemented, except for some som argument checking. You will see a LOT of comments, just read them. I put them there both for me and for you to understand what the code does.

# References
You will note things like \[Cormen\] and \[Manual\] in the code comments, they are the books I used when writing the code:

- \[Cormen\]: Introduction to Algorithms, 3rd Edition, by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein
- \[Manual\]: The Algorithm Design Manual, 2nd Edition, by Steven S. Skiena
- \[Algorithms\]: Algorithms, by Jeff Erickson
- \[Sedgewick\]: Algorithms, 4th Edition, by Robert Sedgewick, Kevin Wayne

All are available in Zlibrary.

# Visualization
Some of the data structures can be visualized using dot/Graphviz. For example, in `data-structure/btree.ss`, we have `btree-to-dot` that converts a btree object to a dot file. Then you can run `dot -Tsvg b.dot > b.svg` to get a picture of the tree. Similar procedures for other data structures also follow this naming convention (`*-to-dot`).

# How to Run the Code
Install ChezScheme using your distro's package manager, or download the source [here](https://github.com/cisco/ChezScheme), in the project directory, run (I'm assuming you are running GNU/Linux on x86_64):

```
./configure --installprefix=/path/to/the/binary
make && make install
```
Then in the REPL, just type:
```
(load "file.ss")
```

now you can use the procedures exported by the file. Make sure you have read the top-level comment in the files, there is an API list and some discussion about the code.

To build the User's Guide, enter `csug/` and run `make`, then the PDF is named `csug.pdf`. Note that this step requires an installed ChezScheme and Latex.

# Contribution
All kinds of commits are welcomed, For code, make sure you follow the existing style:

- Use `#| |#` comment at the top of the file to describe the APIs and possibly the implementation
- Comments in code should start with double semi-colon: `;;`
- Make sure the code is properly indented. Personally I use emacs and when in Scheme-mode, selecting a region and pressing TAB will automatically indent the code.
- If a procedure has side effects, i.e., if it modifies memory, end it with a bang: `!`. E.g., `set!`, `set-car!` and `vector-set!`.

