## rmatio, a R interface to the C library matio, MAT File I/O Library.
## Copyright (C) 2013-2014  Stefan Widgren
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## rmatio is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(rmatio)

##
## Check write and read of array in MAT5 format:
## 1) without compression
## 2) with compression
##

##
## logical: case-1
##
a.in <- array(c(TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
                FALSE, TRUE,  TRUE,  TRUE,  TRUE,
                FALSE, FALSE, TRUE,  TRUE,  TRUE,
                FALSE, FALSE, FALSE, TRUE,  TRUE,
                FALSE, FALSE, FALSE, FALSE, TRUE),
              c(5L, 5L))
filename <- tempfile(fileext = ".mat")
write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    filename <- tempfile(fileext = ".mat")
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, a.in))
}

##
## logical: case-2
##
a.in <- new("lgCMatrix",
            i = c(6L, 0L, 4L, 5L, 0L, 2L, 4L, 1L, 4L, 6L, 7L, 3L, 4L),
            p = c(0L, 1L, 4L, 7L, 11L, 13L),
            Dim = c(10L, 5L),
            Dimnames = list(NULL, NULL),
            x = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                TRUE, TRUE, TRUE),
            factors = list())
filename <- tempfile(fileext = ".mat")
write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    filename <- tempfile(fileext = ".mat")
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, a.in))
}
