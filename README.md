# cl-notebook
###### A notebook-style in-browser editor for Common Lisp

> Tools, of course, can be the subtlest of traps.
> One day I know I must smash the ~~emerald~~ Emacs.
>
> *with apologies to Neil Gaiman*

### Dependencies

`alexandria`, `anaphora`, `cl-fad`, `closer-mop`, `optima`, `local-time`, `fact-base`, `house`

### Usage

You need to install the [`house` server](https://github.com/Inaimathi/house), the [`fact-base` triple-store](https://github.com/Inaimathi/fact-base) and [this repo](https://github.com/Inaimathi/cl-notebook) by cloning them.

The rest of the dependencies are [quicklispable](http://www.quicklisp.org/beta/), so you should then be able to hop into a lisp and do `(ql:quickload :cl-notebook)`, followed by `(cl-notebook:main)`. That'll start a server listening.

### TODO
##### Bugs

##### Features (not necessarily in priority order)
######## Back-end
- Add flag to single out cells that have changed since last being successfully evaluated
	- Such cells should be updated at load time rather than just re-evaluated
	- Think a bit more about this. If the given cell randomly generates some content, it'll actually be changed on each load.
	- Should we just *update* all cells at load time rather than just re-evaluating them?
- Build using buildapp?
- Branching for notebooks
- Add cell dependencies (child cells get evaluated whenever the parent is evaluated)
- Figure out what to do about packages (thinking about defining a `:cl-notebook-user` that binds everything you need for basics and uses that in the running thread)
	- Maybe a separate cell type? It would contain just a package name change the package context of all cells coming after it (this would keep you from having to declare a new package in each cell, while allowing you to have a notebook span multiple packages)
	- Each book has a package (and system) named after it?

######## Front-end
- Use debounce to periodically save cell contents on keydown
- Really REALLY missing s-expression-based navigation. Look into it.
	- [`subpar`](https://github.com/achengs/subpar) exists, apparently
	- You... may need to roll your own s-exp navigation/deletion stuff here. Useful information:
		- `CodeMirror.runMode(byCellId(10, ".cell-contents").value, "commonlisp", function (token, type) { console.log(token, type)})` effectively tokenizes for you.
		- The CodeMirror matching paren mode might also be a good way to get s-expresison-related stuff happening
- Proper autocompletion (this may qualify as both front-end and back-end)
- Argument hints (again, both front and back-end)
- Better automatic indenting
- Better coloring
- front-end cleanup.
	- Possibly move it into a separate project?
	- Might want to annihilate some syntactic rough edges with a `defpsmacro` or two.

### License

[AGPL3](https://www.gnu.org/licenses/agpl-3.0.html) (also found in the included copying.txt)

*Short version:*

Do whatever you like, BUT afford the same freedoms to anyone you give this software or derivative works (yes, this includes the new stuff you do) to, and anyone you expose it to as a service.

### Credits

This project uses:
- [`nativesortable`](https://github.com/bgrins/nativesortable)
- [Code Mirror](http://codemirror.net/)
- [Genericons](http://genericons.com/)
- [Blob.js](https://github.com/eligrey/Blob.js) and [FileSave.js](https://github.com/eligrey/FileSaver.js)
- A spinner generated from [here](http://preloaders.net/en/letters_numbers_words)
- [`anaphora`](http://www.cliki.net/anaphora)
- [`alexandria`](http://common-lisp.net/project/alexandria/)
- [`parenscript`](http://common-lisp.net/project/parenscript/)
- [`cl-who`](http://weitz.de/cl-who/)
- [`quicklisp`](http://www.quicklisp.org/beta/)
- [`buildapp`](http://www.xach.com/lisp/buildapp/)
