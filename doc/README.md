 Efficient Implied Alignment
=============================

[![DOI:10.1186/s12859-020-03595-2](https://zenodo.org/badge/DOI/10.1186/s12859-020-03595-2.svg)](https://doi.org/10.1186/s12859-020-03595-2)

This repository hosts the program `implied-align` implementing the algorithm described in the paper *Efficient Implied Alignment* and a script to replicate the results of the paper.

### Installation of `implied-align`

You can build and install `implied-align` from source using the Haskell build system `ghcup` via the supplied `makefile` from the source directory of this project:

```
$ make install
```

After the `make install` command has completed, the `implied-align` binary will be placed in this project's `bin` directory.

### Running `implied-align`

The `implied-align` program takes a number of command line arguments to specify inputs and outputs. For more information run the following command:

```
$ implied-align --help
```

### Replicating results of the paper

For convience of replicating the results of the paper, a "replicate-results" script has been provided. All that is need to replicate the paper's results is to run the following command:

```
$ make replicate
```

This will create a `replicate-results` directory, with sub directories `csv`, `data`, `img`, `taxa`, and `tree`. 
 
 - The `taxa` directory holds temporary files used in pruning the data-sets. 
 - The `data` & `tree` directories hold the pruned input files for the data-sets.
 - The `csv` directory holds the timing information measured during the replication of the results.
 - The `img` directory holds the generted images from the results.
 
