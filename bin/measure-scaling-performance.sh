#!/bin/bash

# # # # #
#
# Function to call the requisite Haskell and Python binary/script (respectively)
# which will generate the results data and figures presented in the paper.
#
# # # # #

function using()
{
    ./bin/generate-timings $5 \
        --data "data-sets/$1.afasta" \
        --tree "data-sets/$1.tree" \
        --tcm  "data-sets/tcm-${2}.tcm" \
        --output "$1-$2" \
        -n "$3" \
        -k "$4"

    python3 ./bin/plot-figure.py \
        "replicate-results/csv/${1}-${2}.preorder.csv" \
        "replicate-results/img/${1}-preorder.eps"

    python3 ./bin/plot-figure.py \
        "replicate-results/csv/${1}-${2}.postorder.csv" \
        "replicate-results/img/${1}-postorder.eps"
}

"$@"
