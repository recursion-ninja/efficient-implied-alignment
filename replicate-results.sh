#!/bin/bash
#Script to run implied alingment data sets

#load Haskell dependencies
which stack || curl -sSL https://get.haskellstack.org/ | sh

#load Python3 dependencies
which python3        || apt-get install python3     --yes
which pip3           || apt-get install python3-pip --yes
pip3 show matplotlib || pip3 install --upgrade matplotlib

#load R dependencies
which R || sudo apt install r-base

#build binaries
stack install

#run data sets
./bin/generate-timings \
    --data data-sets/fungi.afasta \
    --tree data-sets/fungi.tree \
    --tcm  data-sets/tcm-11.tcm \
    --output fungi-11 \
    -n [25,50,100,200,400,800,1553] \
    -k [1%1,1%2,1%4,1%8,1%16,1%32]
python3 plot-figure.py replicate-results/csv/fungi-11.preorder.csv  replicate-results/png/fungi-preorder.png
python3 plot-figure.py replicate-results/csv/fungi-11.postorder.csv replicate-results/png/fungi-postorder.png

./bin/generate-timings \
    --data data-sets/metazoa.afasta \
    --tree data-sets/metazoa.tree \
    --tcm  data-sets/tcm-11.tcm \
    --output metazoa-11 \
    -n [25,50,100,200,400,800,1766] \
    -k [1%1,1%2,1%4,1%8,1%16,1%32]
python3 plot-figure.py replicate-results/csv/metazoa-11.preorder.csv  replicate-results/png/metazoa-preorder.png
python3 plot-figure.py replicate-results/csv/metazoa-11.postorder.csv replicate-results/png/metazoa-postorder.png

./bin/generate-timings \
    --data data-sets/pathological.afasta \
    --tree data-sets/pathological.tree \
    --tcm  data-sets/tcm-12.tcm \
    --output pathological-12 \
    -n [4,8,16,32,64] \
    -k [1%1,1%2,1%4,1%8,1%16]
python3 plot-figure.py replicate-results/csv/pathological-12.preorder.csv  replicate-results/png/pathological-12-preorder.png
python3 plot-figure.py replicate-results/csv/pathological-12.postorder.csv replicate-results/png/pathological-12-postorder.png

./bin/generate-timings \
    --no-generate \
    --data data-sets/pathological.afasta \
    --tree data-sets/pathological.tree \
    --tcm  data-sets/tcm-31.tcm \
    --output pathological-31 \
    -n [4,8,16,32,64] \
    -k [1%1,1%2,1%4,1%8,1%16]
python3 plot-figure.py replicate-results/csv/pathological-31.preorder.csv  replicate-results/png/pathological-31-preorder.png
python3 plot-figure.py replicate-results/csv/pathological-31.postorder.csv replicate-results/png/pathological-31-postorder.png
