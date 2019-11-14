# Efficient Implied Alignment

This repository hosts the pre-print manuscript for the *Efficient Implied Alignment* paper, an program `tree-align` implementing the algorithm described in the paper, and a script to replicate the results of the paper.

### Installation of `tree-align`

You can build and install `tree-align` from source using the Haskell build system `stack`

To install `stack`, run the following:
```
$ wget -qO- https://get.haskellstack.org/ | sh
```

Once you have the build system installed, ask `stack` to install `tree-align` by runnning the following command from the source directory of this project:

```
$ stack install
```

After the `stack install` command has completed, the `tree-align` binary will be placed in this project's `bin` directory.

### Running `tree-align`

The `tree-align` program takes a number of command line arguments to specify inputs and outputs. For more information run the following command:

```
$ tree-align --help
```

### Replicating results of the paper

For convience of replicating the results of the paper, a "replicate-results" script has been provided. All tat is need to replicate the paper's results is to run the following command:

```
./replicate-results.sh
```

This will create a `replicate-results` directory, with sub directories `csv`, `data`, `png`, `taxa`, and `tree`. 
 
 - The `taxa` directory holds temporary files used in pruning the data-sets. 
 - The `data` & `tree` directories hold the pruned input files for the data-sets.
 - The `csv` directory holds the timing information measured during the replication of the results.
 - The `png` directory holds the generted images from the results.
 
