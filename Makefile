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
	rm -rf _site _cache _tmp
	cabal clean
	rm -f site

rebuild: clean site
	./site rebuild

watch: site
	./site watch

# Nix-based build rules
nix-build:
	nix-build
	cp -L result/bin/site ./site

nix-site: nix-build
	./site build

nix-clean:
	rm -rf result _site _cache _tmp
	rm -f site

# end
