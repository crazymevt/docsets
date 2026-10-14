# docsets

Writes [docsets](https://github.com/crazymevt/docset/blob/main/docs/format.md)
of Lisp libraries, for [spec-browser](https://github.com/crazymevt/spec-browser) and
[Cadre](https://github.com/crazymevt/cadre-editor)'s HyperSpec tab. A docset is written to
`~/.local/share/docsets/<library>/`, where both look for them.

This is for libraries whose authors don't ship a docset. A library you
maintain can build its own into its `docs/docset/` instead, as [gtk4](https://github.com/crazymevt/gtk4) and
[raylib](https://github.com/crazymevt/raylib) do.

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
`make all` does every library in `libraries/`; `make list` names them:

| Library | Entries from |
| --- | --- |
| `bordeaux-threads` | Its documentation site's pages (APIv2) and docstrings (APIv1) |
| `alexandria` | Docstrings; also found as `a:`, the usual local nickname |
| `uiop` | Docstrings, with the contents grouped by UIOP's subpackages (`uiop/os`, `uiop/pathname`, ...) |
| `plump` | Docstrings, grouped by plump-dom, plump-parser and plump-lexer, and its README as the overview |
| `jzon` | Docstrings, and its README (its manual) as the overview; also found as `jzon:` |
| `dexador` | Its README as the overview, which documents the request functions; docstrings, which few symbols have |
| `parachute` | Docstrings, and its README (a guide to writing tests) as the overview |
| `cffi` | Its Texinfo manual: a full entry per definition, and its tutorial and chapters as pages; docstrings for what it leaves out, and its README as the overview |
| `cl-ppcre` | Docstrings, complete; also found as `ppcre:` |

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
- **A Texinfo manual**, when a library's reference is laid out the same way:
  a node per definition (`@deffn` and its kin, or macros expanding to them)
  with `@subheading Syntax`, `Arguments and Values`, `Description` and so
  on. These are preferred over docstrings too, which fill in a Description
  the manual leaves empty. Every other node is a page, nested in the contents
  as the manual's chapters and sections are.

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
- `:group-by-home-package` groups the contents by the package each symbol
  comes from, for a package that re-exports others, as UIOP does.

For the library, `:readme` names a Markdown file (relative to the system's
directory) to show as an Overview page, for a library whose README documents
more than its docstrings do. Its first `#` heading is left out when it names
the library, as a title does. `:texinfo` names a Texinfo manual (relative to
the system's directory too) documenting the first of the packages.

A symbol that one of the library's packages re-exports from another is
documented once, under the package it comes from.

The system defaults to the library's name; pass `:system` if it differs.

## Make targets

- `make docset LIBRARY=...` writes and checks one docset.
- `make all` writes and checks every library in `libraries/`.
- `make check` checks every docset in `~/.local/share/docsets/`.
- `make test` runs the tests.
