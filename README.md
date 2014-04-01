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

- Store notebooks in a separate notebook directory. Load all of them at startup
- Deleting a cell shouldn't trigger a full re-draw; just remove the cell (similar for the others; you might want to integrate event-source here sooner rather than later)
- Some errors still seem to sneak out
- System hangs forever if you send it into an infinite loop. It should time out eventually, and send some notification of the fact.
	- Should it? Are there situations where you'd legitimately want to run an infinite loop in cl-notebook? How would you handle that?
	- Alternative to timing out: run queries in a separate thread (they'll be async anyhow) and give the user a keybinding to kill the current computation
	- You'll need to somehow notify the front-end that there's still a pending computation.
- Also, simple errors don't seem to be getting stored stringified occasionally?
- New notebook should be shown on page-hash change
- Branching for notebooks
- Notebooks should be deletable (ditto branches, when we put that together)
- Notebooks should be renameable (not just re-titleable) (and ditto branches, when we put that together)
- Really REALLY missing s-expression navigation. Look into it.
- Figure out what to do about packages (thinking about defining a `:cl-notebook-user` that binds everything you need for basics and uses that in the running thread)
- front-end cleanup.
	- Possibly move it into a separate project?
	- Might want to annihilate some syntactic rough edges with a `defpsmacro` or two.

### Dependencies

`alexandria`, `anaphora`, `cl-fad`, `closer-mop`, `optima`, `local-time`, `fact-base`

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
- [Genericons](http://genericons.com/)
