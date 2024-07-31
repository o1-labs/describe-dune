# describe-dune: convert opam file syntax to JSON

`describe-dune` finds [dune](https://dune.readthedocs.io/en/stable/reference/dune/index.html)
files in the directory and extracts their library and file dependencies.

Produced mapping doesn't perfectly match to dune structure in handling file dependencies. It's assumed that all stanzas in a dune file depend on the very same set of files, whereas in reality they might be not. It may lead to a circular dependency appearing in file dependency analysis whereas on level of stanzas there is none. This file dependency simplification is kept to ease format and semantics.
It's a advised to change structure of a project if two dune files cross-depend on generated files.

### JSON format

In schema below: "relative path" is a path relative to "src" key, while absolute is a path relative to project's root.

Generates a json array with one entry per non-empty dune file discovered (for dune files with no relevant stanzas no entries are added):

```json
[
  {
    "src": "path to directory containing a dune file",
    "subdirs": [ "relative path to subdirs containing a non-empty dune file" ],
    "units": [
      {
        "name": "name of a compilation unit",
        "public_name": "public name of a compilation unit",
        "type": "lib | exe | test",
        "deps": [ "ocaml library name" ],
        "package": "name of a package",
        "has_inline_tests": true,
        "modules": {
          "with_standard": false,
          "include": [ "module name to include" ],
          "exclude": [ "module name to exclude" ]
        },
        "implements": "name of a virtual library",
        "default_implementation": "name of a default implementation library"
      }
    ],
    "file_deps": [ "absolute path" ],
    "file_outs": { "absolute path of a file generated": "promote | standard | fallback" }
  }
]
```

Field `file_deps` contains only dependencies that are outside the dune file directory. Relatedly, `file_outs` declares only generated files on which some other dune files are dependent.

Some fields of a unit object (like `public_name`) are omitted if not specified by the dune file. Modules field represents a subset of syntax allowed by dune, namely at most one `:standard` meta-module with some files listed for inclusion and exclusion.

### Thanks

A big source of inspiration for this project was
[opam2json](https://github.com/tweag/opam2json).
