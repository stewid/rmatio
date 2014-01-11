rmatio
======

`rmatio` is a package for reading and writing Matlab MAT files from R. `rmatio` supports reading MAT version 4, MAT version 5 and MAT compressed version 5. `rmatio` can write version 5 MAT files and version 5 files with variable compression.

Internally, the `rmatio` package uses the C library [matio](http://sourceforge.net/projects/matio/) for reading/writing MATLAB MAT files.

You can track (and contribute to) development of `rmatio`
at https://github.com/stewid/rmatio.

Installation
------------

To install the latest release on CRAN

```
install.packages('rmatio')
```

To install the development version of rmatio, it's easiest to use the `devtools` package:

```
# install.packages("devtools")
library(devtools)
install_github("rmatio", "stewid")
```

Usage
-----

```
library(rmatio)
```

Read a compressed version 5 MAT file from an URL

```
url <- paste("http://sourceforge.net/p/matio/matio_test_datasets/ci/",
             "master/tree/matio_test_cases_compressed_le.mat?format=raw",
             sep="")
m <- read.mat(url)
```

View content

```
str(m)
```

Write an uncompressed version 5 MAT file

```
write.mat(m, filename="test-uncompressed.mat", compression=FALSE, version="MAT5")
```

Write a compressed version 5 MAT file

```
write.mat(m, filename="test-compressed.mat", compression=TRUE, version="MAT5")
```

Check that the content of the files are identical

```
identical(read.mat("test-uncompressed.mat"), read.mat("test-compressed.mat"))
```

Read a version 4 MAT file with little-endian byte ordering

```
filename <- system.file('extdata/matio_test_cases_v4_le.mat', package='rmatio')
m <- read.mat(filename)
```

View content

```
str(m)
```

Read a version 4 MAT file with big-endian byte ordering

```
filename <- system.file('extdata/matio_test_cases_v4_be.mat', package='rmatio')
m <- read.mat(filename)
```

View content

```
str(m)
```

Included software
-----------------

- The C library [matio](http://sourceforge.net/projects/matio/) (http://sourceforge.net/projects/matio/)

License
-------

The `rmatio` package is licensed under the GPLv3. See these files for additional details:

- LICENSE     - `rmatio` package license (GPLv3)
- inst/NOTICE - Copyright notices for additional included software
