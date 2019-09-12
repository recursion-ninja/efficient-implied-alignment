#!/bin/bash
#Script to run implied alingment data sets
./bin/generate-timings \
    --data data-sets/fungi.afasta \
    --tree data-sets/fungi.tree \
    --tcm  data-sets/tcm-11.tcm \
    --output fungi-11 \
    -n [25,50,100,200,400,800,1553] \
    -k [1%1,1%2,1%4,1%8,1%16,1%32]
python3 replicate-results/fungi-11.preorder.csv fungi-preorder.png

./bin/generate-timings \
    --data data-sets/metazoa.afasta \
    --tree data-sets/metazoa.tree \
    --tcm  data-sets/tcm-11.tcm \
    --output metazoa-11 \
    -n [25,50,100,200,400,800,1766] \
    -k [1%1,1%2,1%4,1%8,1%16,1%32]
python3 replicate-results/metazoa-11.preorder.csv metazoa-preorder.png

./bin/generate-timings \
    --data data-sets/pathological.afasta \
    --tree data-sets/pathological.tree \
    --tcm  data-sets/tcm-12.tcm \
    --output pathological-12
    -n [4,8,16,32,64] \
    -k [1%1,1%2,1%4,1%8,1%16]
python3 replicate-results/pathological-12.preorder.csv pathological-12-preorder.png

./bin/generate-timings \
    --no-generate \        
    --data data-sets/pathological.afasta \
    --tree data-sets/pathological.tree \
    --tcm  data-sets/tcm-21.tcm \
    --output pathological-21
    -n [4,8,16,32,64] \
    -k [1%1,1%2,1%4,1%8,1%16]
python3 replicate-results/pathological-21.preorder.csv pathological-21-preorder.png
