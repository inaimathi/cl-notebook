# cl-notebook
###### A notebook-style in-browser editor for Common Lisp.

> Tools, of course, can be the subtlest of traps.
> One day I know I must smash the ~~emerald~~ Emacs.
>
> *with apologies to Neil Gaiman*

## This is now a pre-beta
##### Use it at your own risk, and expect explosions

### Dependencies

`alexandria`, `anaphora`, `cl-fad`, `closer-mop`, `optima`, `local-time`, `fact-base`, `session-token`, `house`

### Usage

**Either**

Download [this](http://173.255.226.138/cl-notebook-binaries/), run it (if you downloaded the tarball instead of the naked binary, unpack it first, obviously)

**Or**

You need to install the [`house` server](https://github.com/Inaimathi/house), the [`session-token` random token generator](https://github.com/Inaimathi/session-token), the [`fact-base` triple-store](https://github.com/Inaimathi/fact-base) and [this repo](https://github.com/Inaimathi/cl-notebook) by cloning them.

The rest of the dependencies are [quicklispable](http://www.quicklisp.org/beta/), so you should then be able to hop into a lisp and do `(ql:quickload :cl-notebook)`, followed by `(cl-notebook:main)`. That'll start a server listening.

**Then**

Hop into a browser and go to `localhost:4242/` (or whatever port you chose)

A quick-ish video demo is available [here](https://vimeo.com/97623064) to get you sort-of-started.

### TODO
- Need a complete how-to set of videos at some point
- Port to the proper way of using SSEs (with event tags rather than an action field in the payload)

##### Thoughts
- Charts need to support
	- Being saved to a pure HTML+CSS (no javascript) file
	- Resizing naturally with larger or smaller screen sizes
	- Data sets large enough that not all x-axis labels will fit
	- Print support is a nice-to-have, but conflicts heavily with the previous goal
- SVG can work for charts; CSS selectors work the same way as with regular HTML entities AND it can take % dimensions specifications. We basically have no other options for line/pie/doghnut charts.

- Do we want to provide a straight-up scratch REPL for each user?
	- YES
	- Also, we need additional logging buffers. It would have made debugging "Lemonade Stand" much easier to have a buffer keeping `:house` logging data
- Do we want to differentiate between "someone forked a book" and "someone started a new book"? Right now, there's no difference, but we may want to treat forks differently for multi-user purposes later on.

##### Bugs
- Should show the orange border as soon as something is edited in a cell, not just between eval and completion
- The counter in the client-side timeline doesn't update with newly added history states

##### Features (not necessarily in priority order)
######## Back-end
- Exports for projects, not just .lisp files (and the .lisp files should do something intelligent about the `package` forms).
	- Look into [compression options](http://www.cliki.net/compression) for the project part (it'll have to be handled as multiple files)
- Really REALLY need tags. Named checkpoints that you can jump to in book history. This may in fact be a `:fact-base` feature, rather than a `:cl-notebook` feature
- Let user configure where to check for a `quicklisp` folder (by default, check `~/quicklisp`, `~/.cl-notebook/quicklisp` and `quicklisp` in CWD)
- Leave notebooks on disk; just figure out their names and load them on demand when opened. You might need to re-jig naming again as a result of this; the fact that a notebooks' human-readable name is kept INSIDE the notebook will fight you on it
	- Eval all code and markup cells when opening a notebook
- Use `make-broadcast-stream` and some buffering-foo to send partial `*standard-output*` results from evaluations as they arrive. Replace them with evaluation results once those are available.
	- Suddenly more relevant because we definitely want incremental updates for proper `quicklisp` use
- Put together better storage for charts
	- Is this even possible? We could defer computation until display time, but some charts take longer to compute than I'd like. Storing the full HTML output is harder on disk use though. As in "noticeably"; the `BGG corpus charts` article weighs `80mb` on disk, and no other notebook has even cracked `2mb` yet.
- Add cell dependencies (child cells get evaluated whenever the parent is evaluated)
	- Really, what we want here is automatic resolution. When a cell is evaluated, see where its defined values are used, and re-evaluate any cells that apply.
- If there are no existing notebooks, we should write a default scratch book with some initial how-to instructions
- Get `buildapp` working properly with this
	- Give the user a one-button interaction that turns a given notebook into a binary.
- Get poor-man's profiling built into cell results (use `local-time` timestamps for start/end time of operations; compute duration)

######## Front-end
- Things I still kinda want:
	- transpose-sexp
	- slurp-sexp (forward/backward)
	- barf-sexp (forward/backward)
- Add a `run-tests` option to the main menu. Have it evaluate all test cells in the current notebook.
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
- When a notebook is forked, it should create a copy of its package for the fork to use (so that users working on different forks of the same book don't clobber each others in-image changes)

### License

[AGPL3](https://www.gnu.org/licenses/agpl-3.0.html) (also found in the included copying.txt)

*Short version:*

Do whatever you like, BUT afford the same freedoms to anyone you give this software or derivative works (yes, this includes the new stuff you do) to, and anyone you expose it to as a service.

This project includes a copy of `quicklisp.lisp` for ease of use. The [`quicklisp` beta](http://www.quicklisp.org/beta/) is released under an [Expat-like license](http://www.quicklisp.org/beta/#license).

This project uses [CodeMirror](http://codemirror.net/) as a front-end editor. CodeMirror [is released](http://codemirror.net/#community) under the [MIT Expat license](http://codemirror.net/LICENSE).
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
