# docsets

Writes [docsets](https://github.com/crazymevt/docset/blob/main/docs/format.md)
of Lisp libraries, for the spec browser and
[Cadre](https://github.com/crazymevt/cadre-editor)'s HyperSpec tab. A docset is written to
`~/.local/share/docsets/<library>/`, where both look for them.

This is for libraries whose authors don't ship a docset. A library you
maintain can build its own into its `docs/docset/` instead, as gtk4 and
raylib do.

## Setup

You need SBCL with [Quicklisp](https://www.quicklisp.org), and the
[docset library](https://github.com/crazymevt/docset), which isn't in
Quicklisp's dist. Check out both repos and link the library where Quicklisp
finds local projects:

```sh
cd ~/projects/lisp
git clone https://github.com/crazymevt/docset.git
git clone https://github.com/crazymevt/docsets.git
ln -s ~/projects/lisp/docset ~/quicklisp/local-projects/docset
cd docsets
```

If Quicklisp doesn't see a new link, run `(ql:register-local-projects)` once.
Quicklisp fetches the libraries you document; for one that isn't in its dist,
put that in `~/quicklisp/local-projects/` too.

## Making one

```bash
make docset LIBRARY=bordeaux-threads
```

It loads the library with Quicklisp, writes the docset, and checks it.
`make all` does every library in `libraries/`; `make list` names them.

Any library works, even with no file in `libraries/`. It's documented from
the docstrings and lambda lists of the external symbols of the package with
its name:

```bash
make docset LIBRARY=alexandria
```

## Where entries come from

- **Docstrings.** Each external symbol's definitions (function, generic
  function, macro, variable, constant, class, condition, structure, type) get
  an entry with a Syntax line from the lambda list and the docstring as the
  Description. Paragraphs, `- ` lists and indented code are kept. Uppercase
  names become parameters, links to other entries, or code.
- **Documentation pages**, when a library has HyperSpec-style Markdown pages:
  one per definition, titled like `Function ACQUIRE-LOCK, RELEASE-LOCK`, with
  `#### Syntax:`, `#### Arguments and values:`, `#### Description:` and so on.
  These are preferred over docstrings. Each subdirectory's `_index.md` is a
  dictionary in the contents; the top `_index.md` is the overview.

## Adding a library

Add `libraries/<name>.lisp`:

```lisp
(define-library "bordeaux-threads"
  :title "Bordeaux Threads"
  :source "https://github.com/sionescu/bordeaux-threads"
  :license "MIT"
  :packages (("BORDEAUX-THREADS-2" :nicknames ("BT2") :pages "docs/content/" :toc-title "APIv2 (BT2)")
             ("BORDEAUX-THREADS" :nicknames ("BT") :prefix "bt:" :toc-title "APIv1 (BT)"
              :see-also "BORDEAUX-THREADS-2")))
```

Per package:

- `:nicknames` are the names code uses, so `bt:make-thread` finds its entry.
- `:pages` is a directory of pages, relative to the library's system.
- `:prefix` tells apart entries of packages that export the same names.
- `:see-also` links each entry to the same name in another package.

The system defaults to the library's name; pass `:system` if it differs.

## Make targets

- `make docset LIBRARY=...` writes and checks one docset.
- `make all` writes and checks every library in `libraries/`.
- `make check` checks every docset in `~/.local/share/docsets/`.
- `make test` runs the tests.
