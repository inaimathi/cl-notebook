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

- rename-book
- new-book
- eval-to-cell
- new-cell
- reorder-cells
- kill-cell
- change-cell-type

- Deleting a cell shouldn't trigger a full re-draw; just remove the cell on success.
	- Events that would benefit from this treatment include
		- editing notebook title
		- adding a new cell
		- removing a cell (duh)
		- editing/evaling a cell
		- re-ordering cells
	- Which is actually all of them.
	- You could hook up an event-stream per book to send incremental updates out to each viewer. This would naturally lend itself to group coding.

- Some errors still seem to sneak out
- System hangs forever if you send it into an infinite loop. It should time out eventually, and send some notification of the fact.
	- Should it? Are there situations where you'd legitimately want to run an infinite loop in cl-notebook? How would you handle that?
	- Alternative to timing out: run queries in a separate thread (they'll be async anyhow) and give the user a keybinding to kill the current computation
	- You'll need to somehow notify all front-ends that there's still a pending computation.
- Also, simple errors don't seem to be getting stored stringified occasionally? Haven't seen this since the initial sighting so it may have been fixed already.
- Branching for notebooks
- Notebooks should be deletable (ditto branches, when we put that together)
	- Not *actually* deletable from the front end. Put a delete button up, get confirmation, and move it off to `~/.cl-notebook/trash` or something. If a user really TRULY wants to delete all of their history, they can do the `rm` call manually
	- Is that the right behavior? I think so; our goal is to never lose data. An errant `POST` request shouldn't be able to counter that intent.
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
