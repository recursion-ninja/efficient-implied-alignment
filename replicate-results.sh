#!/bin/bash


# # # # #
#
# Script to reproduce the results of the paper from a clean environment.
# Installs all required dependencies. Builds the required binaries.
# Run the experimental algorithm on datasets to generate the empirical results.
#
# # # # #


# Install dependencies required to replicate results:
#   * Load Python3 dependencies
which python3         &> /dev/null || apt-get install python3     --yes
which pip3            &> /dev/null || apt-get install python3-pip --yes
pip3  show matplotlib &> /dev/null || pip3 install --upgrade matplotlib

#   * Load R dependencies
which R &> /dev/null || sudo apt install r-base

#   * Load Haskell dependencies
which ghcup &> /dev/null || curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh


# Build binaries
ghcup run \
    --ghc 9.2.1 \
    --cabal 3.6.2.0 \
    -- cabal update && \
       cabal install \
        --installdir=./bin \
        --install-method=copy 


# Ensure output directories exist to place results data
mkdir -p replicate-results/csv
mkdir -p replicate-results/data
mkdir -p replicate-results/img
mkdir -p replicate-results/taxa
mkdir -p replicate-results/tree


# Run data sets to produce the results data
function generate()
{
    ./bin/generate-timings $5 \
        --data "data-sets/$1.afasta" \
        --tree "data-sets/$1.tree" \
        --tcm  "data-sets/tcm-${2}.tcm" \
        --output "$1-$2" \
        -n "$3" \
        -k "$4"
    
    python3 plot-figure.py \
        "replicate-results/csv/${1}-${2}.preorder.csv" \
        "replicate-results/img/${1}-preorder.eps"

    python3 plot-figure.py \
        "replicate-results/csv/${1}-${2}.postorder.csv" \
        "replicate-results/img/${1}-postorder.eps"
}

generate 'fungi'        '11' '[25,50,100,200,400,800,1553]' '[1%1,1%2,1%4,1%8,1%16,1%32]'
generate 'metazoa'      '11' '[25,50,100,200,400,800,1766]' '[1%1,1%2,1%4,1%8,1%16,1%32]'
generate 'pathological' '12' '[4,8,16,32,64,128,256]'       '[1%1,1%2,1%4,1%8,1%16,1%32,1%64]'
generate 'pathological' '31' '[4,8,16,32,64,128,256]'       '[1%1,1%2,1%4,1%8,1%16,1%32,1%64]' '--no-generate'
