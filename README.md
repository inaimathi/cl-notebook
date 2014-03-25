# cl-notebook
###### I got bored. So here's Notebook for Common Lisp

> Tools, of course, can be the subtlest of traps.
> One day I know I must smash the ~~emerald~~ Emacs.
>
> *with apologies to Neil Gaiman*

# Do not use this yet. Seriously.
######Just walk away for a little while 'till I get it running properly.

### What's `cl-notebook`?

An in-browser editor for my own idiosyncratic use editing/presentation/etc use. Maybe it'll be useful to you too, but I wouldn't hold my breath.

### TODO

- front-end cleanup.
	- Possibly move it into a separate project?
	- Might want to annihilate some syntactic rough edges with a `defpsmacro` or two.
	- Style everything so it looks pretty
	- The markup videos should have `esc` bound to `hide-editor` for their cell
- Clean up cell-type (should be something specific like `:common-lisp` or `:cl-who`, rather than `:code` vs `:markup` with each branching into specific dialects)
- full `cl-who` power in the who blocks (this is going to involve `eval` again, I'm afraid, but I want to be able to stitch lisp code into `cl-who` blocks, otherwise they're not really much better than `markdown`)

### Usage

You need to install the [`house` server](https://github.com/Inaimathi/house), the [`fact-base` triple-store](https://github.com/Inaimathi/fact-base), [`cl-mop`](https://github.com/Inaimathi/cl-mop) and [this repo](https://github.com/Inaimathi/cl-notebook) by cloning them.

The rest of the dependencies are [quicklispable](http://www.quicklisp.org/beta/), so you should then be able to hop into a lisp and do `(ql:quickload :cl-notebook)`.

### License

[AGPL3](https://www.gnu.org/licenses/agpl-3.0.html) (also found in the included copying.txt)

*Short version:*

Do whatever you like, BUT afford the same freedoms to anyone you give this software or derivative works (yes, this includes the new stuff you do) to, and anyone you expose it to as a service.

### Credits

This project uses
- [`nativesortable`](https://github.com/bgrins/nativesortable)
- [Code Mirror](http://codemirror.net/)
- [`anaphora`](http://www.cliki.net/anaphora)
- [`alexandria`](http://common-lisp.net/project/alexandria/)
- [`parenscript`](http://common-lisp.net/project/parenscript/)
- [`cl-who`](http://weitz.de/cl-who/)
- [`quicklisp`](http://www.quicklisp.org/beta/)
- [`buildapp`](http://www.xach.com/lisp/buildapp/)
