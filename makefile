# # # # #
#
# Script to reproduce the results of the paper from a clean environment.
# Installs all required dependencies. Builds the required binaries.
# Run the experimental algorithm on datasets to generate the empirical results.
#
# # # # #

bin-dir          := ./bin
data-dir         := replicate-results
executable       := $(bin-dir)/generate-timings
generate         := $(bin-dir)/measure-scaling-performance.sh using
biological-scale := [1%1,1%2,1%4,1%8,1%16,1%32]
customized-scale := [1%1,1%2,1%4,1%8,1%16,1%32,1%64]
customized-nodes := [4,8,16,32,64,128,256]


# All synonyms for replicating the paper's results.
all: results

replicate: results

reproduce: results

results: fungi metazoa pathological


# Install dependencies required to replicate results:
ensure-Haskell: $(bin-dir)/generate-timings
	@command -v ghcup &> /dev/null || curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ensure-Python:
	@command -v python3    &> /dev/null || apt-get install python3     --yes
	@command -v pip3       &> /dev/null || apt-get install python3-pip --yes
	@pip3  show matplotlib &> /dev/null || pip3 install --upgrade matplotlib

ensure-R:
	command -v &> /dev/null || sudo apt install r-base

compile-binaries: ensure-Haskell
	@echo "Copiling binaries"
	@ghcup run \
	    --ghc 9.2.1 \
	    --cabal 3.6.2.0 \
	    -- cabal update && \
	       cabal install \
	        --installdir=$(bin-dir) \
	        --install-method=copy 

ensure-workspace:
	@mkdir -p $(data-dir)/csv
	@mkdir -p $(data-dir)/data
	@mkdir -p $(data-dir)/img
	@mkdir -p $(data-dir)/taxa
	@mkdir -p $(data-dir)/tree

fungi: compile-binaries ensure-Python ensure-R ensure-workspace
	$(generate) \
	    'fungi'        '11' '[25,50,100,200,400,800,1553]' $(biological-scale)

metazoa: compile-binaries ensure-Python ensure-R ensure-workspace
	$(generate) \
	    'metazoa'      '11' '[25,50,100,200,400,800,1766]' $(biological-scale)

pathological: compile-binaries ensure-Python ensure-R ensure-workspace
	$(generate) \
	    'pathological' '12' $(customized-nodes) $(customized-scale)
	$(generate) \
	    'pathological' '31' $(customized-nodes) $(customized-scale) '--no-generate'
