rmatio
======

`rmatio` is a package for reading and writing Matlab MAT files from R. `rmatio` supports reading MAT version 4, MAT version 5 and MAT compressed version 5. `rmatio` can write version 5 MAT files and version 5 files with variable compression.

Internally, the `rmatio` package uses the C library [matio](http://sourceforge.net/projects/matio/) for reading/writing MATLAB MAT files.

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

    write.mat(m, filename="test-uncompressed.mat", compression=FALSE, version="MAT5")

Write a compressed version 5 MAT file

    write.mat(m, filename="test-compressed.mat", compression=TRUE, version="MAT5")

Check that the content of the files are identical

    identical(read.mat("test-uncompressed.mat"), read.mat("test-compressed.mat"))

Included software
-----------------

- The C library [matio](http://sourceforge.net/projects/matio/) written by Christopher Hulbert

License
-------

The `rmatio` package is licensed under the GPLv3. See these files for additional details:

- LICENSE     - `rmatio` package license (GPLv3)
- inst/NOTICE - Copyright notices for additional included software
