##
# Build LYAH web site from Markdown sources using Hakyll
#

all: site
	./site build
	# Copy generated files from _site/ to docs/ for GitHub Pages
	rsync -av --delete --exclude=.git _site/ docs/

site: site.hs lyah-site.cabal
	cabal build
	cp $$(cabal list-bin site) ./site

clean:
	rm -rf _site _hakyll_cache _hakyll_tmp
	cabal clean
	rm -f site

rebuild: clean site
	./site rebuild
	# Copy generated files from _site/ to docs/ for GitHub Pages
	rsync -av --delete --exclude=.git _site/ docs/

watch: site
	./site watch

# end
