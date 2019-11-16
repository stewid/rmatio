# Determine package name and version from DESCRIPTION file
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

# Name of built package
PKG_TAR=$(PKG_NAME)_$(PKG_VERSION).tar.gz

# Install package
install:
	cd .. && R CMD INSTALL $(PKG_NAME)

# Build documentation with roxygen
# 1) Remove old doc
# 2) Generate documentation
roxygen:
	rm -f man/*.Rd
	cd .. && Rscript -e "library(roxygen2); roxygenize('$(PKG_NAME)')"

# Sync matio source code
sync:
	-cp -f ../matio/src/endian.c src/matio/
	-cp -f ../matio/src/inflate.c src/matio/
	-cp -f ../matio/src/mat.c src/matio/
	-cp -f ../matio/src/mat4.h src/matio/
	-cp -f ../matio/src/mat4.c src/matio/
	-cp -f ../matio/src/mat5.h src/matio/
	-cp -f ../matio/src/mat5.c src/matio/
	-cp -f ../matio/src/matio.h src/matio/
	-cp -f ../matio/src/matio_private.h src/matio/
	-cp -f ../matio/src/matvar_cell.c src/matio/
	-cp -f ../matio/src/matvar_struct.c src/matio/
	-cp -f ../matio/src/read_data.c src/matio/
	-cd src/matio && patch -i ../../patches/inflate.c.patch
	-cd src/matio && patch -i ../../patches/read_data.c.patch
	-cd src/matio && patch -i ../../patches/mat.c.patch
	-cd src/matio && patch -i ../../patches/matio.h.patch
	-cd src/matio && patch -i ../../patches/matio_private.h.patch

# Generate PDF output from the Rd sources
# 1) Rebuild documentation with roxygen
# 2) Generate pdf, overwrites output file if it exists
pdf: roxygen
	cd .. && R CMD Rd2pdf --force $(PKG_NAME)

# Build and check package
check:
	cd .. && R CMD build --no-build-vignettes $(PKG_NAME)
	cd .. && _R_CHECK_CRAN_INCOMING_=FALSE NOT_CRAN=true \
        R CMD check --as-cran --no-manual --no-vignettes \
        --no-build-vignettes --no-stop-on-test-error --run-dontrun $(PKG_TAR)

# Build and check package with valgrind
check_valgrind: clean
	cd .. && R CMD build --no-build-vignettes $(PKG_NAME)
	cd .. && R CMD check --as-cran --no-manual --no-vignettes --no-build-vignettes --use-valgrind $(PKG_TAR)

# Run all tests with valgrind
test_objects = $(wildcard tests/*.R)
valgrind:
	$(foreach var,$(test_objects),R -d "valgrind --tool=memcheck --leak-check=full" --vanilla < $(var);)

# Check reverse dependencies
#
# 1) Install packages (in ./revdep/lib) to check reverse dependencies.
# 2) Check reverse dependencies using 'R CMD check'.
# 3) Collect results from '00check.log' files.
revdep: revdep_install revdep_check revdep_results

# Install packages to check reverse dependencies
revdep_install: clean
	mkdir -p revdep/lib
	cd .. && R CMD INSTALL --library=$(PKG_NAME)/revdep/lib $(PKG_NAME)
	Rscript --vanilla \
          -e "options(repos = c(CRAN='https://cran.r-project.org'))" \
          -e "lib <- 'revdep/lib'" \
          -e "pkg <- tools::package_dependencies('$(PKG_NAME)', which = 'all', reverse = TRUE)" \
          -e "pkg <- as.character(unlist(pkg))" \
          -e "dep <- sapply(pkg, tools::package_dependencies, which = 'all')" \
          -e "dep <- as.character(unlist(dep))" \
          -e "if ('BiocInstaller' %in% dep) {" \
          -e "    source('https://bioconductor.org/biocLite.R')" \
          -e "    biocLite('BiocInstaller', lib = lib)" \
          -e "}" \
          -e "install.packages(pkg, lib = lib, dependencies = TRUE)" \
          -e "download.packages(pkg, destdir = 'revdep')"

# Check reverse dependencies with 'R CMD check'
revdep_check:
	$(foreach var,$(wildcard revdep/*.tar.gz),R_LIBS=revdep/lib \
          _R_CHECK_CRAN_INCOMING_=FALSE R --vanilla CMD check --as-cran \
          --no-stop-on-test-error --output=revdep $(var) \
          | tee --append revdep/00revdep.log;)

# Collect results from checking reverse dependencies
revdep_results:
	Rscript --vanilla \
          -e "options(repos = c(CRAN='https://cran.r-project.org'))" \
          -e "pkg <- tools::package_dependencies('$(PKG_NAME)', which = 'all', reverse = TRUE)" \
          -e "pkg <- as.character(unlist(pkg))" \
          -e "results <- do.call('rbind', lapply(pkg, function(x) {" \
          -e "    filename <- paste0('revdep/', x, '.Rcheck/00check.log')" \
          -e "    lines <- readLines(filename)" \
          -e "    status <- sub('^Status: ', '', lines[grep('^Status: ', lines)])" \
          -e "    data.frame(Package = x, Status = status)" \
          -e "}))" \
          -e "results <- results[order(results[, 'Status']), ]" \
          -e "rownames(results) <- NULL" \
          -e "cat('\n\n*** Results ***\n\n')" \
          -e "results" \
          -e "cat('\n\n')"

configure: configure.ac
	autoconf ./configure.ac > ./configure
	chmod +x ./configure

clean:
	./cleanup

.PHONY: install roxygen pdf check check_valgrind valgrind revdep revdep_install \
        revdep_check revdep_results configure clean
