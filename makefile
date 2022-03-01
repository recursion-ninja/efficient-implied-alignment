# # # # #
#
# Script to reproduce the results of the paper from a clean environment.
# Installs all required dependencies. Builds the required binaries.
# Run the experimental algorithm on datasets to generate the empirical results.
#
# # # # #

dir-bin         := ./bin
dir-data        := replicate-results
bin-aln         := implied-align
bin-aln-path    := $(dir-bin)/$(bin-aln)
bin-gen         := generate-timings
bin-gen-path    := $(dir-bin)/$(bin-gen)
measure-script  := measure-scaling-performance.sh
measure-dataset := $(dir-bin)/$(measure-script) using
prerequisites   := $(bin-aln-path) $(bin-aln-path) ensure-python ensure-workspace


# All synonyms for replicating the paper's results.
replicate: results

reproduce: results

results:   fungi metazoa pathological


# Install dependencies required to replicate results.
ensure-haskell:
	@command -v ghcup      >/dev/null 2>&1 || \
	    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ensure-python:
	@command -v python3    >/dev/null 2>&1 || apt-get install python3     --yes
	@command -v pip3       >/dev/null 2>&1 || apt-get install python3-pip --yes
	@pip3  show matplotlib >/dev/null 2>&1 || pip3 install --upgrade matplotlib

ensure-workspace:
	@$(eval make-workspace := mkdir -p $(dir-data))
	@$(make-workspace)/csv
	@$(make-workspace)/data
	@$(make-workspace)/img
	@$(make-workspace)/taxa
	@$(make-workspace)/tree


# Build binaries
require := ensure-haskell $(wildcard app/**/*.hs) $(wildcard src/**/*.hs)
install := ghcup run --ghc 9.2.1 --cabal 3.6.2.0 -- \
		cabal update &&	cabal install $(bin-gen) $(bin-aln) \
		--installdir=$(dir-bin) --install-method=copy

$(bin-aln-path): $(require)
	@$(install)

$(bin-gen-path): $(require)
	@$(install)


# Generate timing data of data sets
biological-scale := '\[1%1,1%2,1%4,1%8,1%16,1%32\]'
customized-scale := '\[1%1,1%2,1%4,1%8,1%16,1%32,1%64\]'
customized-nodes := '\[4,8,16,32,64,128,256\]'

fungi: $(prerequisites)
	@$(MAKE) --no-print-directory measure \
		name='fungi'   cost='11' sizes='\[25,50,100,200,400,800,1553\]' scale=$(biological-scale)

metazoa: $(prerequisites)
	@$(MAKE) --no-print-directory measure \
		name='metazoa' cost='11' sizes='\[25,50,100,200,400,800,1766\]' scale=$(biological-scale)

pathological: $(prerequisites)
	@$(MAKE) --no-print-directory measure \
		name='pathological' cost='12' sizes=$(customized-nodes) scale=$(customized-scale)

	@$(MAKE) --no-print-directory measure \
		name='pathological' cost='31' sizes=$(customized-nodes) scale=$(customized-scale) flags='--no-generate'

measure:
	./bin/generate-timings $(flags) \
		--data data-sets/$(name).afasta \
		--tree data-sets/$(name).tree \
		--tcm  data-sets/tcm-$(cost).tcm \
		--output  $(name)-$(cost) \
		--leaves  $(sizes) \
		--lengths $(scale)

	python3 ./bin/plot-figure.py \
		"replicate-results/csv/$(name)-$(cost).preorder.csv" \
		"replicate-results/img/$(name)-preorder.eps"
	python3 ./bin/plot-figure.py \
		"replicate-results/csv/$(name)-$(cost).postorder.csv" \
		"replicate-results/img/$(name)-postorder.eps"

# Clean up after replicating results
clean:
	@rm -fr replicate-results
	@rm -fr dist-newstyle
	@rm -f  $(bin-aln-path)
	@rm -f  $(bin-gen-path)
