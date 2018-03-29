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

## License

[AGPL3](https://www.gnu.org/licenses/agpl-3.0.html) (also found in the included copying.txt)

*Short version:*

Do whatever you like, BUT afford the same freedoms to anyone you give this software or derivative works (yes, this includes the new stuff you do) to, and anyone you expose it to as a service.

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
