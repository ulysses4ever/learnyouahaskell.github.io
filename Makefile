##
# Build LYAH web site from Markdown sources using Hakyll.
# Requires GHC and Cabal (for cabal-based builds) or Nix (for nix-based builds).
#

all: site
	./site build

site: site.hs lyah-site.cabal
	cabal build
	cp $$(cabal list-bin site) ./site

clean:
	rm -rf _site _hakyll_cache _hakyll_tmp
	cabal clean
	rm -f site

rebuild: clean site
	./site rebuild

watch: site
	./site watch

# Nix-based build rules
nix-build:
	nix-build

nix-clean:
	rm -rf result _site _hakyll_cache _hakyll_tmp

# end
