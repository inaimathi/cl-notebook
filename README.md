# cl-notebook
###### A notebook-style in-browser editor for Common Lisp.

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

A quick-ish video demo is available [here](https://vimeo.com/97623064) to get you sort-of-started.

### TODO
#### Notes
- We are not going to automatically evaluate notebooks on book load, and we won't be automatically re-evaluating and saving cells whose values have changed. Some cells might contain things like calls to `random`, or file-writes to freshly-generated temp-files; that makes the second goal difficult if not impossible. The first goal is annoying in the absence of the second; it means re-evaling all cells and saving those that changed since last time. This would potentially cause a notebook to change every time it was opened with cl-notebook, which is unacceptable from the user perspective.
	
##### Thoughts
- Charts need to support
	- Being saved to a pure HTML+CSS (no javascript) file
	- Resizing naturally with larger or smaller screen sizes
	- Data sets large enough that not all x-axis labels will fit
	- Print support is a nice-to-have, but conflicts heavily with the previous goal
- SVG can work for charts; CSS selectors work the same way as with regular HTML entities AND it can take % dimensions specifications. We basically have no other options for line/pie/doghnut charts.

- Do we want to provide a straight-up scratch REPL for each user?
- Cells are now updated if their content is different on a notebook-load. This means that books that contain timestamp-related cells or similar will always be changed when they're opened. Is that what we really want?
	- Unsure. On the one hand, we don't want a cell to be re-saved every time on the basis that it contains a call to `random`. On the other hand, we don't want to say such changes never require a re-eval. The _easy_ way out seems to be dropping the eval-on-load thing. Or, at the very least, deferring it to the first time a given book is viewed in the lifetime of a server instance.
	- No, we don't want cells evaluated on notebook load. Let the user take care of it as they need to. Should probably add an `Eval Book` option to the main menu
- Do we want to differentiate between "someone forked a book" and "someone started a new book"? Right now, there's no difference, but we may want to treat forks differently for multi-user purposes later on.

##### Bugs
- Remove the stale tags now that we no longer save between evals

##### Features (not necessarily in priority order)
######## Back-end
- Figure out what to do about packages (thinking about defining a `:cl-notebook-user` that binds everything you need for basics and uses that in the running thread)
	- Maybe a separate cell type? It would contain just a package name change the package context of all cells coming after it (this would keep you from having to declare a new package in each cell, while allowing you to have a notebook span multiple packages)
	- Each book has a package (and system) named after it? (Renaming just got really hard)
		- Did it? `rename-package` exists, and would let us pull this off fairly easily. We don't even need to sanitize input; package names can be arbitrary strings. The biggest problem is that we'd probably want a readable name for our title and a typeable name for our actual package name. Separate `:init` cell still sounds like the best idea, frankly.
	- This just bit again (failed to properly eval a `:cl-who` form because it wasn't being done in `:cl-notebook`). Right decision might be to default to `:cl-notebook`, but allow a system specification cell type that'd let users specify different info.
	- Thinking one `init` cell at the beginning of the notebook. Defaults to just `(in-package :cl-notebook-user)` (should be the assumed value if there isn't valid contents in the cell as well), but can be changed to include package definition for the current notebook. We can pull out package info automatically for ASD generation
- Get `quicklisp` working properly with this.
	- Let user configure where to check for a `quicklisp` folder (default to `~/quicklisp`)
	- If `ql` package exists when loading, just use the defaults.
	- Otherwise, try to load from the user-specified `quicklisp` location.
		- If it doesn't exist, start a new install in `~/.cl-notebook/quicklisp`
	- What we want is a workflow where we transparently use the systems' existing `quicklisp` directory, if any, and create our own if one doesn't already exist (If they *have* a quicklisp folder, but re-configured `quicklisp` to store it somewhere other than the default, they'll have to similarly configure `cl-notebook`).
- Put together better storage for charts
- Leave notebooks on disk; just figure out their names and load them on demand when opened. You might need to re-jig naming again as a result of this; the fact that a notebooks' human-readable name is kept INSIDE the notebook will fight you on it
- Add cell dependencies (child cells get evaluated whenever the parent is evaluated)
	- Really, what we want here is automatic resolution. When a cell is evaluated, see where its defined values are used, and re-evaluate any cells that apply.
- Use `make-broadcast-stream` and some buffering-foo to send partial `*standard-output*` results from evaluations as they arrive. Replace them with evaluation results once those are available.
- If there are no existing notebooks, we should write a default scratch book with some initial how-to instructions
- We should go through history rather than just evaluating current cells in order (they may have initially been evaluated in a different order. Doing the general thing might be better all-round)
- Get `buildapp` working properly with this
	- Give the user a one-button interaction that turns a given notebook into a binary.

######## Front-end
- Add an `Eval Book` option to the menu. This could either be implemented as a fresh server-side handler that does `eval-notebook!` thing, or as a purely front-end sequence of `POST` requests for each cell in sequence. The async nature of the eval-cell feature might make the first one easier.
- Need a keyboard-oriented way of jumping between cells
	- C->/C-<
	- Possibly replace C-[ and C-] (they fuck with indentation levels, which is handled automatically by the mode anyhow)
- Similarly, keyboard-oriented way of moving cells up or down
- Comment region
- Really REALLY missing s-expression-based navigation. Basics implemented.
	- Things I still kinda want:
		- transpose-sexp
		- slurp-sexp (forward/backward)
		- barf-sexp (forward/backward)
- You're already customizing the commonlisp mode all to hell; just go the whole nine and put in the proper Lisp-specific labels instead of this `variable-3`/`string-2` shit.
- Macroexpander (this'll need some back-end stuff too)
	- When you macroexpand in a cell, it should pop up a macroexpander div with an editor that has the highlighted results
	- If you macroexpand something in said expander div, it should be expanded in-place (replacing the original term in the expander div)
	- Hitting `<esc>` should hide all expander windows
	- Not sure if we should support multiples
- Complete on local-scope symbols (such as those introduced by `let`, `let*`, `flet`, `labels`, `macrolet`) at a higher priority than global symbols
- Handle completion and arg-hints of symbols with package names (for example, `alexandria:hash-table-alist`)
- Notebooks should be sorted by notebook-name, at the very least (in addition to the below noted fork-grouping)
	- This may involve changes to some back-end systems; you need to order up the initial notebook list, _as well as_ inserting new notebooks in an ordered manner. Do we just bite the bullet and hit the server every time? Or maybe send out a complete notebooks list every time someone adds one?
- History entries should be grouped with their parents. Guess you could pull out parent relationships at load-time? Sounds like you're getting closer and closer to sub-classing fact-base into a separate notebook class.
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
