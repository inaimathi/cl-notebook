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

### Project Notes
- We are not going to automatically evaluate notebooks on book load, and we won't be automatically re-evaluating and saving cells whose values have changed. Some cells might contain things like calls to `random`, or file-writes to freshly-generated temp-files; that makes the second goal difficult if not impossible. The first goal is annoying in the absence of the second; it means re-evaling all cells and saving those that changed since last time. This would potentially cause a notebook to change every time it was opened with cl-notebook, which is unacceptable from the user perspective.

### TODO
- `Ctrl-Ret` should run a `repackage-notebook` command. You'll need appropriate back-end handlers and a new `repackaged` event
- Add `Ctrl-Ret` binding for `rename-notebook` to the title input, just for consistency.

- Need a complete how-to set of videos at some point

##### Thoughts
- Charts need to support
	- Being saved to a pure HTML+CSS (no javascript) file
	- Resizing naturally with larger or smaller screen sizes
	- Data sets large enough that not all x-axis labels will fit
	- Print support is a nice-to-have, but conflicts heavily with the previous goal
- SVG can work for charts; CSS selectors work the same way as with regular HTML entities AND it can take % dimensions specifications. We basically have no other options for line/pie/doghnut charts.

- Do we want to provide a straight-up scratch REPL for each user?
- Do we want to differentiate between "someone forked a book" and "someone started a new book"? Right now, there's no difference, but we may want to treat forks differently for multi-user purposes later on.

##### Bugs
- Should show the orange border as soon as something is edited in a cell, not just between eval and completion

##### Features (not necessarily in priority order)
######## Back-end
- Figure out what to do about packages (thinking about defining a `:cl-notebook-user` that binds everything you need for basics and uses that in the running thread)
	- Automatically generated entry type.
		- Not cell, there can only be one per notebook. Keep it in with the notebook name cluster on the front-end
	- Default value is
		- `(defpackage <notebook name> (:use :cl :cl-notebook :cl-who :fact-base))`
		- This should also be the assumed value in case there's an error parsing this entry
	- On change:
		- re-name package
		- add new `use`d packages
		- add new `import`/`shadowing-import`ed symbols
		- add new `export`ed symbols
	- On rename notebook:
		- If the package is the default value, do a package rename on that too
	- On fork notebook:
		- Create a new package and re-evaluate in-notebook code into that package
			- Do we really want that?
			- Probably, from the multi-user context; you don't want people who are editing disparate versions to suddenly start clobbering each others' code.
- Get `quicklisp` working properly with this.
	- Let user configure where to check for a `quicklisp` folder (default to `~/quicklisp`)
	- If `ql` package exists when loading, just use the defaults.
	- Otherwise, try to load from the user-specified `quicklisp` location.
		- If it doesn't exist, start a new install in `~/.cl-notebook/quicklisp`
	- What we want is a workflow where we transparently use the systems' existing `quicklisp` directory, if any, and create our own if one doesn't already exist (If they *have* a quicklisp folder, but re-configured `quicklisp` to store it somewhere other than the default, they'll have to similarly configure `cl-notebook`).
- Leave notebooks on disk; just figure out their names and load them on demand when opened. You might need to re-jig naming again as a result of this; the fact that a notebooks' human-readable name is kept INSIDE the notebook will fight you on it
	- Eval all code and markup cells when opening a notebook
- Put together better storage for charts
	- Is this even possible? We could defer computation until display time, but some charts take longer to compute than I'd like. Storing the full HTML output is harder on disk use though. As in "noticeably"; the `BGG corpus charts` article weighs `80mb` on disk, and no other noebook has even cracked `2mb` yet.
- Add cell dependencies (child cells get evaluated whenever the parent is evaluated)
	- Really, what we want here is automatic resolution. When a cell is evaluated, see where its defined values are used, and re-evaluate any cells that apply.
- Use `make-broadcast-stream` and some buffering-foo to send partial `*standard-output*` results from evaluations as they arrive. Replace them with evaluation results once those are available.
- If there are no existing notebooks, we should write a default scratch book with some initial how-to instructions
- Get `buildapp` working properly with this
	- Give the user a one-button interaction that turns a given notebook into a binary.
- Get poor-man's profiling built into cell results (use `local-time` timestamps for start/end time of operations; compute duration)

######## Front-end
- Macroexpander (this'll need some back-end stuff too)
	- When you macroexpand in a cell, it should pop up a macroexpander div with an editor that has the highlighted results
	- If you macroexpand something in said expander div, it should be expanded in-place (replacing the original term in the expander div)
	- Hitting `<esc>` should hide all expander windows
	- Not sure if we should support multiples
- Add a `run-tests` option to the main menu. Have it evaluate all test cells in the current notebook.
- Things I still kinda want:
	- transpose-sexp
	- slurp-sexp (forward/backward)
	- barf-sexp (forward/backward)
- Already customizing the commonlisp mode all to hell; just go the whole nine and put in the proper Lisp-specific labels instead of this `variable-3`/`string-2` shit.
- Complete on local-scope symbols (such as those introduced by `let`, `let*`, `flet`, `labels`, `macrolet`) at a higher priority than global symbols
- Handle completion and arg-hints of symbols with package names (for example, `alexandria:hash-table-alist`)
- Notebooks should be sorted by notebook-name, at the very least (in addition to the below noted fork-grouping)
	- This may involve changes to some back-end systems; you need to order up the initial notebook list, _as well as_ inserting new notebooks in an ordered manner. Do we just bite the bullet and hit the server every time? Or maybe send out a complete notebooks list every time someone adds one?
- Forked notebook entries should be grouped with their parents in the top menu. Guess you could pull out parent relationships at load-time?

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
