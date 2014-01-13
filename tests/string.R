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
## Check write and read of character strings in MAT5 format:
## 1) without compression
## 2) with compression
##

##
## string: case-1
##
a.in <- c("abcdefghijklmnopqrstuvwxyz",
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
          "1234567890!@#$%^&*()-_=+`~", #
          "[{]}\\|;:'\",<.>/?          ")
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
## string: case-2
##
a.in <- c("a", "bb")
filename <- tempfile(fileext = ".mat")
write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, list("a", "bb")))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    filename <- tempfile(fileext = ".mat")
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, list("a", "bb")))
}

##
## string: case-3
##
a.in <- list(y=c("a", "bb"))
filename <- tempfile(fileext = ".mat")
write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, list(y=list('a', 'bb'))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    filename <- tempfile(fileext = ".mat")
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, list(y=list('a', 'bb'))))
}

##
## string: case-4
##
a.in <- list(c("a", "bb"))
filename <- tempfile(fileext = ".mat")
write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, list(list("a", "bb"))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    filename <- tempfile(fileext = ".mat")
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, list(list("a", "bb"))))
}
