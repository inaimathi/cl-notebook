# cl-notebook
##### A notebook-style in-browser editor for Common Lisp.

> Tools, of course, can be the subtlest of traps.
> One day I know I must smash the ~~emerald~~ Emacs.
>
> *with apologies to Neil Gaiman*

# This is now a pre-beta
#### Use it at your own risk, and expect occasional, minor explosions

## Dependencies

`alexandria`, `anaphora`, `cl-fad`, `closer-mop`, `optima`, `local-time`, `fact-base`, `session-token`, `house`

## Usage

### Binary

Download [this](http://static.inaimathi.ca/cl-notebook-binaries/), run it (if you downloaded the tarball instead of the naked binary, unpack it first, obviously)

_At the moment, we've only got binaries for 64-bit Linux. Submissions for other architectures welcome._

### With [`quicklisp`](http://www.quicklisp.org/beta/)

- Install a Common Lisp (I suggest [`sbcl`](http://www.sbcl.org/platform-table.html))
- Install [`quicklisp`](http://www.quicklisp.org/beta/)
- Clone the [`house` server](https://github.com/Inaimathi/house), the [`fact-base` triple-store](https://github.com/Inaimathi/fact-base) and [this repo](https://github.com/Inaimathi/cl-notebook) into your `~quicklisp/local-projects.` directory.
- Hop into a Lisp and do `(ql:quickload :cl-notebook)`, followed by `(cl-notebook:main)`

### With [`roswell`](https://github.com/roswell/roswell) and [`qlot`](https://github.com/fukamachi/qlot)

_These help you manage Common Lisp distributions. They are usefull not only for running cl-notebook, but for any other CL project, so consider them regardless of whether you want this project._

- Install [`roswell`](https://github.com/roswell/roswell)
- Install [`qlot`](https://github.com/fukamachi/qlot)
- Clone [cl-notebook](https://github.com/Inaimathi/cl-notebook)

In the `cl-notebook` directory you cloned to, do:

```
qlot install
qlot exec roswell/cl-notebook.ros --port 4242
```

**Once `cl-notebook` is Running**

Hop into a browser and go to `localhost:4242/` (or whatever port you chose)

A quick-ish, and now slightly out-of-date video demo is available [here](https://vimeo.com/97623064) to get you sort-of-started.

## Sytem Docs

### Building the Binary

#### With [`roswell`](https://github.com/roswell/roswell) and [`qlot`](https://github.com/fukamachi/qlot)

- Install [`roswell`](https://github.com/roswell/roswell)
- Install [`qlot`](https://github.com/fukamachi/qlot)
- Run `qlot exec ros build roswell/cl-notebook.ros` in the `cl-notebook` directory

   That will create a binary with the appropriate name that you can directly run on any machine of your OS and processor architecture.
- Grab your binary at `roswell/cl-notebook`.

This should work under the Linux, OSX and Windows.

#### With [`buildapp`](https://www.xach.com/lisp/buildapp/)

In order to build the `cl-notebook` binary, you need to

- Install a Common Lisp (I suggest, and have only tried this with, [`sbcl`](http://www.sbcl.org/platform-table.html))
- Install [`quicklisp`](http://www.quicklisp.org/beta/)
- Install and build [`buildapp`](https://www.xach.com/lisp/buildapp/)
- Create an appropriate `build.manifest` file for loading `cl-notebook`
- Call `buildapp` with that `build.manifest` file, along with
	- a bunch of `--load-system` calls that include everything `cl-notebook` needs
    - an `--eval` call to `cl-notebook::read-statics` to include all the associated static files along with the binary
    - an `--entry` of `cl-notebook:main`
    - an `--output` of your choice of binary name (I suggest "`cl-notebook`")

That will create a binary with the appropriate name that you can directly run on any machine of your OS and processor architecture.

##### Linux

If you're on a Debian-based linux distro, there is a `build.lisp` and `build.sh` included in the `build/` subdirectory of this repo that do most of the above for you. All you need to do is make sure to install `sbcl`, then call `sh build.sh` in the `build` directory. This will result in a `buildapp` binary and a `cl-notebook` binary being generated for you. The `cl-notebook` binary can then be run on any linux machine _(of the same CPU architecture)_ without worrying about installing a Lisp.

##### OS X

TODO - patches welcome, since I'm not an OS X user

##### Windows

TODO - patches welcome, since I'm not a Windows user

### Source Deployment
### Usage
#### Keybindings
#### Building Programs/Executables
#### Notebook Exporters
#### Cell Compilers

#### Thoughts
- Charts need to support
	- Being saved to a pure HTML+CSS (no javascript) file
	- Resizing naturally with larger or smaller screen sizes
	- Data sets large enough that not all x-axis labels will fit
	- Print support is a nice-to-have, but conflicts heavily with the previous goal
- SVG can work for charts; CSS selectors work the same way as with regular HTML entities AND it can take % dimensions specifications. We basically have no other options for line/pie/doghnut charts.

- Do we want to provide a straight-up scratch REPL for each user?
	- No I don't think so, but we need additional logging buffers. Maybe one per notebook so that incremental debugging is a bit easier.
- Do we want to differentiate between "someone forked a book" and "someone started a new book"? Right now, there's no difference, but we may want to treat forks differently for multi-user purposes later on.

#### Features (in priority order by section)
####### Back-end
- Exports for projects, not just .lisp files (and the .lisp files should do something intelligent about the `package` forms).
	- Look into [compression options](http://www.cliki.net/compression) for the project part (it'll have to be handled as multiple files)
- Get `buildapp` working properly with this
	- Give the user a one-button interaction that turns a given notebook into a binary.
- Get poor-man's profiling built into cell results (use `local-time` timestamps for start/end time of operations; compute duration)
- Really REALLY need tags. Named checkpoints that you can jump to in book history.
	- Implemented as part of `:fact-base`, now we need to add the proper interface here (this includes a thing for adding checkpoints, and an addition to the history slider to let it specifically jump to tagged points, so this is really both a back-end thing and a front-end thing)
- Use `make-broadcast-stream` and some buffering-foo to send partial `*standard-output*` results from evaluations as they arrive. Replace them with evaluation results once those are available.
	- Suddenly more relevant because we definitely want incremental updates for proper `quicklisp` use
- If there are no existing notebooks, we should write a default scratch book with some initial how-to instructions
- Put together better storage for charts
	- Is this even possible? Sure, as long as we're ok with deferring computation until display-time. Storing the full HTML output is harder on disk use though. As in "noticeably"; the `BGG corpus charts` article weighs `80mb` on disk, and no other notebook has even cracked `2mb` yet.
- Add cell dependencies (child cells get evaluated whenever the parent is evaluated)
	- Really, what we want here is automatic resolution. When a cell is evaluated, see where its defined values are used, and re-evaluate any cells that apply.
- Let user configure where to check for a `quicklisp` folder (by default, check `~/quicklisp`, `~/.cl-notebook/quicklisp` and `quicklisp` in CWD) (Note that this task might be made unnecessary by `qlot` use)

####### Front-end
- Things I still kinda want:
	- transpose-sexp
	- slurp-sexp (forward/backward)
	- barf-sexp (forward/backward)
    not sure whether we've got the available keybingings for it though, unless there's some way of stealing them from the underlying browser.
- Add a `run-tests` option to the main menu. Have it evaluate all test cells in the current notebook. Maybe have a dedicated tests output display (or that additional logging buffer I keep kinda wanting)
- Already customizing the commonlisp mode all to hell; just go the whole nine and put in the proper Lisp-specific labels instead of this `variable-3`/`string-2` shit.
- Update to the latest version of `CodeMirror`
- Complete on local-scope symbols (such as those introduced by `let`, `let*`, `flet`, `labels`, `macrolet`) at a higher priority than global symbols
- Handle completion and arg-hints of symbols with package names (for example, `alexandria:hash-table-alist`)
- Forked notebook entries should be grouped with their parents in the open menu. Guess you could pull out parent relationships at load-time? Or fuck around with parsing filenames?
- For general use, we'll want to expose customizable keybindings somehow. A session-keyed config page would work fine.

####### Multi-user related
- Moving cells around isn't propagated to other users. It should be, just like any other front-end edit.
- If you join a book in the middle of an already running computation, you currently aren't notified of this. Figure something out.
- Authentication should be a thing. OAuth from GitGoogFaceHooExchange, possibly along with a local password/admin system.
- Saving user preferences should be a thing. Per user might be the right thing, but a `local storage` solution migh get us most of the way to this (and may actually be better if a single user tends to use a notebook from multiple devices).
- Give users an interface to upload new notebooks from their local environments to the notebook instances' local (really, may as well give them arbitrary disk access since this _is_ a remote programming environment, but we might also consider restricting uploads to some known folder in `~/.cl-notebook/`)
- Each user should be associated with a color. Show a halo around the cell other users' are editing (as well as an approximate cursor) in realtime
- When a notebook is forked, it should create a copy of its package for the fork to use (so that users working on different forks of the same book don't clobber each others in-image changes). Does `qlot` give us any of this for free?
- Move to a multi-threaded model with a cell-evaluation work-queue to make multi-user development easier (this involves both a thread-coordination subsystem, and a front-end interfaceto manage multiple threads)

## License

[AGPL3](https://www.gnu.org/licenses/agpl-3.0.html) (also found in the included copying.txt)

*Short version:*

Do whatever you like, BUT afford the same freedoms to anyone you give this software or derivative works (yes, this includes the new stuff you do) to, and anyone you expose it to as a service.

This project includes a copy of `quicklisp.lisp` for ease of use. The [`quicklisp` beta](http://www.quicklisp.org/beta/) is released under an [Expat-like license](http://www.quicklisp.org/beta/#license).

This project uses [CodeMirror](http://codemirror.net/) as a front-end editor. CodeMirror [is released](http://codemirror.net/#community) under the [MIT Expat license](http://codemirror.net/LICENSE).

## Credits

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
