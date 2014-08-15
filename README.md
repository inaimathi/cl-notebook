# cl-notebook
###### A notebook-style in-browser editor for Common Lisp.

Quick-ish video demo available [here](https://vimeo.com/97623064).

> Tools, of course, can be the subtlest of traps.
> One day I know I must smash the ~~emerald~~ Emacs.
>
> *with apologies to Neil Gaiman*

## This is now a pre-beta
##### Use it at your own risk, and expect explosions

### Dependencies

`alexandria`, `anaphora`, `cl-fad`, `closer-mop`, `optima`, `local-time`, `fact-base`, `house`

### Usage

**Either**

Download [this](http://173.255.226.138/cl-notebook-binaries/), run it (if you downloaded the tarball instead of the naked binary, unpack it first, obviously)

**Or**

You need to install the [`house` server](https://github.com/Inaimathi/house), the [`fact-base` triple-store](https://github.com/Inaimathi/fact-base) and [this repo](https://github.com/Inaimathi/cl-notebook) by cloning them.

The rest of the dependencies are [quicklispable](http://www.quicklisp.org/beta/), so you should then be able to hop into a lisp and do `(ql:quickload :cl-notebook)`, followed by `(cl-notebook:main)`. That'll start a server listening.

**Then**

Hop into a browser and go to `localhost:4242/` (or whatever port you chose)

### TODO
##### Thoughts
- Cells are now updated if their content is different on a notebook-load. This means that books that contain timestamp-related cells or similar will always be changed when they're opened. Is that what we really want?
	- Unsure. On the one hand, we don't want a cell to be re-saved every time on the basis that it contains a call to `random`. On the other hand, we don't want to say such changes never require a re-eval. The _easy_ way out seems to be dropping the eval-on-load thing. Or, at the very least, deferring it to the first time a given book is viewed in the lifetime of a server instance.
- Do we want to differentiate between "someone forked a book" and "someone started a new book"? Right now, there's no difference, but we may want to treat forks differently for multi-user purposes later on.

##### Bugs

##### Features (not necessarily in priority order)
######## Back-end
- Figure out what to do about packages (thinking about defining a `:cl-notebook-user` that binds everything you need for basics and uses that in the running thread)
	- Maybe a separate cell type? It would contain just a package name change the package context of all cells coming after it (this would keep you from having to declare a new package in each cell, while allowing you to have a notebook span multiple packages)
	- Each book has a package (and system) named after it? (Renaming just got really hard)
	- This just bit again (failed to properly eval a `:cl-who` form because it wasn't being done in `:cl-notebook`). Right decision might be to default to `:cl-notebook`, but allow a system specification cell type that'd let users specify different info.
	- Thinking one cell at the beginning of the notebook. Defaults to just `(in-package :cl-notebook-user)` (should be the assumed value if there isn't valid contents in the cell as well), but can be changed to include ASDF/package definition for the current notebook. Effectively locks a notebook to a single package, and may be easier to implement than notebook-name-is-package-name approach (especially with respect to re-naming)
- Get `quicklisp` working properly with this.
	- Let user configure where to check for a `quicklisp` folder (default to `~/quicklisp`)
	- If `ql` package exists when loading, just use the defaults.
	- Otherwise, try to load from the user-specified `quicklisp` location.
		- If it doesn't exist, start a new install in `~/.cl-notebook/quicklisp`
	- What we want is a workflow where we transparently use the systems' existing `quicklisp` directory, if any, and create our own if one doesn't already exist (If they *have* a quicklisp folder, but re-configured `quicklisp` to store it somewhere other than the default, they'll have to similarly configure `cl-notebook`).
- Add cell dependencies (child cells get evaluated whenever the parent is evaluated)
	- Really, what we want here is automatic resolution. When a cell is evaluated, see where its defined values are used, and re-evaluate any cells that apply.
- Use `make-broadcast-stream` and some buffering-foo to send partial `*standard-output*` results from evaluations as they arrive. Replace them with evaluation results once those are available.
- If there are no existing notebooks, we should write a default scratch book with some initial how-to instructions
- We should go through history rather than just evaluating current cells in order (they may have initially been evaluated in a different order. Doing the general thing might be better all-round)
- Get `buildapp` working properly with this
	- Give the user a one-button interaction that turns a given notebook into a binary.

######## Front-end
- You're already customizing the commonlisp mode all to hell; just go the whole nine and put in the proper Lisp-specific labels instead of this `variable-3`/`string-2` shit.
- Macroexpander (this'll need some back-end stuff too)
	- When you macroexpand in a cell, it should pop up a macroexpander div with an editor that has the highlighted results
	- If you macroexpand something in said expander div, it should be expanded in-place (replacing the original term in the expander div)
	- Hitting `<esc>` should hide all expander windows
	- Not sure if we should support multiples
- Complete on local-scope symbols (such as those introduced by `let`, `let*`, `flet`, `labels`, `macrolet`) at a higher priority than global symbols
- Notebooks should be sorted by notebook-name, at the very least (in addition to the below noted fork-grouping)
	- This may involve changes to some back-end systems; you need to order up the initial notebook list, _as well as_ inserting new notebooks in an ordered manner. Do we just bite the bullet and hit the server every time? Or maybe send out a complete notebooks list every time someone adds one?
- History entries should be grouped with their parents. Guess you could pull out parent relationships at load-time? Sounds like you're getting closer and closer to sub-classing fact-base into a separate notebook class.
- Really REALLY missing s-expression-based navigation. Basics implemented.
	- Things I still kinda want:
		- slurp-sexp (forward/backward)
		- barf-sexp (forward/backward)
- front-end cleanup.
	- Possibly move it into a separate project?
	- Might want to annihilate some syntactic rough edges with a `defpsmacro` or two.

######## Multi-user related
- Move to a thread-per-cell model to make multi-user development easier
- If you join a book in the middle of an already running computation, you currently aren't notified of this. Figure something out.
- Moving cells around isn't propagated to other users
- Editing a cell should be propagated between saves (back-end should probably figure out when to save, and it should run operational transforms on diffs to keep from clobbering any users' input)

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
