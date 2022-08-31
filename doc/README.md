## Efficient Implied Alignment


[![DOI:10.1186/s12859-020-03595-2][DOI-IMG]][DOI-URL]
[![Author       ][ Author-IMG]][ Author-URL]
[![BSD3 License ][License-IMG]][License-URL]

[![Maintained   ][Support-IMG]][Support-URL]
[![Haskell QA CI][  QA-CI-IMG]][  QA-CI-URL]

[![Release      ][Release-IMG]][Release-URL]
[![Release Date ][TagDate-IMG]][Release-URL]

[![New Commits  ][Commits-IMG]][Commits-URL]
[![Code Size    ][   Size-IMG]][   Size-URL]


This repository hosts the program `implied-align` implementing the algorithm described in the paper *Efficient Implied Alignment* and a script to replicate the results of the paper.


### Installation of `implied-align`

You can build and install `implied-align` from source using the Haskell build system `ghcup` via the supplied `makefile` from the source directory of this project:

```bash
$ make install
```

After the `make install` command has completed, the `implied-align` binary will be placed in this project's `bin` directory.


### Running `implied-align`

The `implied-align` program takes a number of command line arguments to specify inputs and outputs. For more information run the following command:

```bash
$ implied-align --help
```

### Replicating results of the paper

For convenience of replicating the results of the paper, a "replicate-results" script has been provided. All that is need to replicate the paper's results is to run the following command:

```bash
$ make replicate
```

This will create a `replicate-results` directory, with sub directories `csv`, `data`, `img`, `taxa`, and `tree`. 
 
 - The `taxa` directory holds temporary files used in pruning the data-sets. 
 - The `data` & `tree` directories hold the pruned input files for the data-sets.
 - The `csv` directory holds the timing information measured during the replication of the results.
 - The `img` directory holds the generated images from the results.


[    DOI-IMG]: https://zenodo.org/badge/DOI/10.1186/s12859-020-03595-2.svg
[    DOI-URL]: https://doi.org/10.1186/s12859-020-03595-2
[  QA-CI-IMG]: https://github.com/recursion-ninja/efficient-implied-alignment/actions/workflows/haskell.yaml/badge.svg?branch=master
[  QA-CI-URL]: https://github.com/recursion-ninja/efficient-implied-alignment/actions/workflows/haskell.yaml
[ Author-IMG]: https://img.shields.io/badge/author-Alex%20Washburn-blue.svg?color=134EA2
[ Author-URL]: mailto:academia@recursion.ninja
[   Size-IMG]: https://img.shields.io/github/languages/code-size/recursion-ninja/efficient-implied-alignment.svg?style=popout&color=yellowgreen
[   Size-URL]: https://github.com/recursion-ninja/efficient-implied-alignment/archive/master.zip
[Commits-IMG]: https://img.shields.io/github/commits-since/recursion-ninja/efficient-implied-alignment/latest.svg?style=popout&color=yellowgreen
[Commits-URL]: https://github.com/recursion-ninja/efficient-implied-alignment/commits/master
[License-IMG]: https://img.shields.io/badge/license-BSD3-blue.svg?color=134EA2
[License-URL]: https://github.com/recursion-ninja/efficient-implied-alignment/blob/master/doc/LICENSE
[Release-IMG]: https://img.shields.io/github/release-pre/recursion-ninja/efficient-implied-alignment.svg?style=popout&color=orange
[Release-URL]: https://github.com/recursion-ninja/efficient-implied-alignment/releases/latest
[TagDate-IMG]: https://img.shields.io/github/release-date-pre/recursion-ninja/efficient-implied-alignment.svg?style=popout&color=orange
[Support-IMG]: https://img.shields.io/maintenance/yes/2022.svg?style=popout
[Support-URL]: https://github.com/recursion-ninja/efficient-implied-alignment/graphs/contributors
