rmatio 0.14.0
-------------

BUG FIXES

* Fixed memory protection errors reported by the 'rchk' tool.

* Fixed memory leaks reported by valgrind.

rmatio 0.13.0
-------------

BUG FIXES

* Fixed memory protection errors reported by the 'rchk' tool.

* Removed an unused configuration variable.

* Fixed a potential segfault error by adding a check before usage of a
  non-null pointer to the variable name when reading data.

rmatio 0.12.0
-------------

* Fixed registration of native routines called from R.

* Added importFrom 'download.file' and 'packageVersion' from utils to
  NAMESPACE.

* Updated the bundled matio C library to commit '8b44851' (v1.5.10)
  from 14 Feb 2017.

rmatio 0.11.0
-------------

CHANGES

* Added SystemRequirements (zlib headers and library) field to
  DESCRIPTION

* Replaced ZLIB_LIBS with -lz in src/Makevars.win

* Updated maintainer email

* Changed 'Title' field in DESCRIPTION to title case

rmatio 0.10.0
-------------

CHANGES

* Instead of raising an error when reading a function class type, give
  a warning and read data structure as NULL.

rmatio 0.9.0
-------------

CHANGES

* Improved test code layout to give better information in case of
  error

* Each write/read test case run with a unique tempfile

* Depends on R(>=3.0.2)

BUG FIXES

* Fix memory leaks

* Removed usage of LENGTH on a S4SXP object

rmatio 0.8.0
-------------

CHANGES

* Added missing field in DESCRIPTION, Biarch: true

* Removed usage of testthat when testing the package.

rmatio 0.7.0
-------------

CHANGES

* Updated documentation

* Moved data files to inst/extdata

* Fix configuration

rmatio 0.6.0
-------------

CHANGES

* Fix Makevars and Makevars.win

rmatio 0.5.0
-------------

CHANGES

* Removed bundled zlib in src/zlib

rmatio 0.4.0
-------------

CHANGES

* Moved matio source code to subdirectory of src

* Moved zlib source code to subdirectory of src

rmatio 0.3.0
-------------

NEW FEATURES

* Updated package documentation.

rmatio 0.2.0
-------------

NEW FEATURES

* Supports reading MAT version 4, MAT version 5 and MAT compressed
  version 5.

* Supports writing version 5 MAT files and version 5 files with
  variable compression.

* Added compression with zlib.

* Updated matio to ver 1.5.2

rmatio 0.1.0
-------------

NEW FEATURES

* Added support to write a list data structure.

rmatio 0.0.8
-------------

NEW FEATURES

* Added support to write a logical vector.

* An integer vector is now written as a signed
  32 bit integer instead of as a double.

rmatio 0.0.7
-------------

NEW FEATURES

* Added support to write a character vector.

rmatio 0.0.6
-------------

NEW FEATURES

* Added support to read a dense logical matrix.

rmatio 0.0.5
-------------

NEW FEATURES

* Added support to read a sparse logical matrix.

* Added support to read struct and cell data types.

rmatio 0.0.4
-------------

NEW FEATURES

* Added support to read/write n-dimensional arrays.

rmatio 0.0.3
-------------

NEW FEATURES

* Added option for MAT file version to create in write.mat. Currently
  only support for Matlab level-5 file (MAT5) from rmatio package.

* Removed option overwrite from write.mat.

rmatio 0.0.2
-------------

NEW FEATURES

* Updated matio C library code to version 1.5.1

rmatio 0.0.1
-------------

NEW FEATURES

* Initial package structure
