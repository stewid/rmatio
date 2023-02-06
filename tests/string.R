## rmatio, a R interface to the C library matio, MAT File I/O Library.
## Copyright (C) 2013-2023  Stefan Widgren
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

## For debugging
sessionInfo()

##
## Check write and read of character strings in MAT5 format:
## 1) without compression
## 2) with compression
##

##
## Note:
## If the character vector contains elements of differents lengths i.e.
## a2_in <- list(c("a", "bb"), c("c", "dd"))                            # nolint
## then the expected result is not identical, since each element
## is saved in a cell and the expected result of a2_in is therefore
## a2_exp <- list("a", "bb")                                            # nolint
##

##
## string: case-1
##
a1_exp <- c("abcdefghijklmnopqrstuvwxyz",
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
          "1234567890!@#$%^&*()-_=+`~", #
          "[{]}\\|;:'\",<.>/?          ")
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a1_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a1_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a1_obs)
stopifnot(identical(a1_obs, a1_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a1_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a1_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a1_zlib_obs)
stopifnot(identical(a1_zlib_obs, a1_exp))

##
## string: case-2
##
a2_in <- c("a", "bb")
a2_exp <- list("a", "bb")
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a2_in), filename = filename, compression = FALSE,
          version = "MAT5")
a2_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a2_obs)
stopifnot(identical(a2_obs, a2_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a2_in), filename = filename, compression = TRUE,
          version = "MAT5")
a2_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a2_zlib_obs)
stopifnot(identical(a2_zlib_obs, a2_exp))

##
## string: case-3
##
a3_in <- list(y = c("a", "bb"))
a3_exp <- list(y = list("a", "bb"))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a3_in), filename = filename, compression = FALSE,
          version = "MAT5")
a3_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a3_obs)
stopifnot(identical(a3_obs, a3_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a3_in), filename = filename, compression = TRUE,
          version = "MAT5")
a3_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a3_zlib_obs)
stopifnot(identical(a3_zlib_obs, a3_exp))

##
## string: case-4
##
a4_in <- list(c("a", "bb"))
a4_exp <- list(list("a", "bb"))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a4_in), filename = filename, compression = FALSE,
          version = "MAT5")
a4_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a4_obs)
stopifnot(identical(a4_obs, a4_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a4_in), filename = filename, compression = TRUE,
          version = "MAT5")
a4_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a4_zlib_obs)
stopifnot(identical(a4_zlib_obs, a4_exp))
