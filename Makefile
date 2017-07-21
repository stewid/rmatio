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
	-cp -f ../matio/src/matio.h src/matio/
	-cp -f ../matio/src/matio_private.h src/matio/
	-cp -f ../matio/src/matvar_cell.c src/matio/
	-cp -f ../matio/src/matvar_struct.c src/matio/
	-cp -f ../matio/src/read_data.c src/matio/
	-cd src/matio && patch -i ../../patches/inflate.c.patch
	-cd src/matio && patch -i ../../patches/matvar_struct.c.patch
	-cd src/matio && patch -i ../../patches/read_data.c.patch
	-cd src/matio && patch -i ../../patches/mat.c.patch
	-cd src/matio && patch -i ../../patches/mat4.c.patch
	-cd src/matio && patch -i ../../patches/mat5.h.patch
	-cd src/matio && patch -i ../../patches/matio_private.h.patch

# Generate PDF output from the Rd sources
# 1) Rebuild documentation with roxygen
# 2) Generate pdf, overwrites output file if it exists
pdf: roxygen
	cd .. && R CMD Rd2pdf --force $(PKG_NAME)

# Build and check package
check: clean
	cd .. && R CMD build --no-build-vignettes $(PKG_NAME)
	cd .. && R CMD check --no-manual --no-vignettes --no-build-vignettes $(PKG_TAR)

# Build and check package with valgrind
check_valgrind: clean
	cd .. && R CMD build --no-build-vignettes $(PKG_NAME)
	cd .. && R CMD check --as-cran --no-manual --no-vignettes --no-build-vignettes --use-valgrind $(PKG_TAR)

# Run all tests with valgrind
test_objects = $(wildcard tests/*.R)
valgrind:
	$(foreach var,$(test_objects),R -d "valgrind --tool=memcheck --leak-check=full" --vanilla < $(var);)

clean:
	./cleanup

.PHONY: install roxygen pdf check check_valgrind valgrind clean
