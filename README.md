rmatio
======

R interface to the C-library matio

rmatio is a package for reading and writing Matlab MAT files from R,
using the C library matio written by Christopher Hulbert
(http://sourceforge.net/projects/matio/).

You can track (and contribute to) development of `rmatio`
at https://github.com/stewid/rmatio.

Installation
------------

To install the development version of rmatio, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    library(devtools)
    install_github("rmatio", "stewid")

Usage
-----

Read a compressed version 5 MAT file from an URL

    library(rmatio)
    m <- read.mat("http://sourceforge.net/p/matio/matio_test_datasets/ci/master/tree/matio_test_cases_compressed_le.mat?format=raw")

View content

    str(m)

Write an uncompressed version 5 MAT file

    write.mat(m, filename="test.mat", compression=FALSE, version="MAT5")
