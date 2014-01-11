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
## Check write and read of dgCMatrix in MAT5 format:
## 1) without compression
## 2) with compression
##

filename <- tempfile(fileext = ".mat")

##
## dgCMatrix: case-1
##
a.in <- Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 1, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 1),
               nrow=3,
               ncol=9,
               byrow=TRUE,
               sparse=TRUE)

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

# Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, a.in))
}

##
## dgCMatrix: case-2
##
a.in <- as(diag(1:5), 'dgCMatrix')

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

# Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, a.in))
}
