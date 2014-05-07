doc:
	rm -f man/*.Rd
	cd .. && Rscript -e "library(methods); library(utils); library(roxygen2); roxygenize('rmatio')"

clean:
	-rm -f config.log
	-rm -f config.status
	-rm -f src/Makevars
	-rm -f src/*.o
	-rm -f src/*.so
	-rm -f src/matio/*.o

.PHONY: doc clean
