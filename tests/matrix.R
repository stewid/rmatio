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
## Check write and read of matrix in MAT5 format:
## 1) without compression
## 2) with compression
##

filename <- tempfile(fileext = ".mat")

##
## matrix: case-1
##
a.in <- matrix(1:9, nrow=3)

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
storage.mode(a.out) <- 'integer'
stopifnot(identical(a.out, a.in))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    storage.mode(a.out) <- 'integer'
    stopifnot(identical(a.out, a.in))
}

##
## matrix: case-2
##
a.in <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3)

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.in, a.out))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, a.in))
}
