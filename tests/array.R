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
## array: case-1
##
a.in <- array(seq_len(32^3), c(32,32,32))
storage.mode(a.in) <- 'integer'
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
## array: case-2
##
a.in <- array(seq_len(32^3), c(32,32,32))
storage.mode(a.in) <- 'double'
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
## array: case-3
##
a.in <- array(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
                TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE,
                FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                FALSE, FALSE, TRUE), c(5L, 5L))
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
## array: case-4
##
a.in <- array(seq_len(32^3), c(32,32,32));
storage.mode(a.in) <- 'double'
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
## array: case-5
##
a.in <- array(seq_len(32^3), c(32,32,32));
storage.mode(a.in) <- 'integer'
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
## array: case-6
##
a.in <- array(c(seq_len(32767), 32767), c(32,32,32));
storage.mode(a.in) <- 'integer'
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
## array: case-7
##
a.in <- array(complex(real=seq(1, 2*32^3, 2), imaginary=seq(2, 2*32^3, 2)), c(32,32,32))
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
