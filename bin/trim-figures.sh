#!/bin/bash

PATH_TO_FIGS=replicate-results/png/

convert -trim "${PATH_TO_FIGS}fungi-preorder.png"           "${PATH_TO_FIGS}fungi-preorder.png"
convert -trim "${PATH_TO_FIGS}metazoa-preorder.png"         "${PATH_TO_FIGS}metazoa-preorder.png"
convert -trim "${PATH_TO_FIGS}pathological-12-preorder.png" "${PATH_TO_FIGS}pathological-12-preorder.png"
convert -trim "${PATH_TO_FIGS}pathological-31-preorder.png" "${PATH_TO_FIGS}pathological-31-preorder.png"
