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
## Check write and read of array in MAT5 format:
## 1) without compression
## 2) with compression
##

##
## array: case-1
##
a1_exp <- array(seq_len(32^3), c(32, 32, 32))
storage.mode(a1_exp) <- "integer"
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
## array: case-2
##
a2_exp <- array(seq_len(32^3), c(32, 32, 32))
storage.mode(a2_exp) <- "double"
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a2_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a2_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a2_obs)
stopifnot(identical(a2_obs, a2_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a2_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a2_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a2_zlib_obs)
stopifnot(identical(a2_zlib_obs, a2_exp))

##
## array: case-3
##
a3_exp <- array(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
                  TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE,
                  FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
                  FALSE, TRUE), c(5L, 5L))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a3_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a3_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a3_obs)
stopifnot(identical(a3_obs, a3_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a3_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a3_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a3_zlib_obs)
stopifnot(identical(a3_zlib_obs, a3_exp))

##
## array: case-4
##
a4_exp <- array(seq_len(32^3), c(32, 32, 32))
storage.mode(a4_exp) <- "double"
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a4_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a4_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a4_obs)
stopifnot(identical(a4_obs, a4_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a4_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a4_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a4_zlib_obs)
stopifnot(identical(a4_zlib_obs, a4_exp))

##
## array: case-5
##
a5_exp <- array(seq_len(32^3), c(32, 32, 32))
storage.mode(a5_exp) <- "integer"
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a5_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a5_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a5_obs)
stopifnot(identical(a5_obs, a5_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a5_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a5_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a5_zlib_obs)
stopifnot(identical(a5_zlib_obs, a5_exp))

##
## array: case-6
##
a6_exp <- array(c(seq_len(32767), 32767), c(32, 32, 32))
storage.mode(a6_exp) <- "integer"
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a6_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a6_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a6_obs)
stopifnot(identical(a6_obs, a6_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a6_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a6_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a6_zlib_obs)
stopifnot(identical(a6_zlib_obs, a6_exp))

##
## array: case-7
##
a7_exp <- array(complex(real = seq(1, 2 * 32^3, 2),
                        imaginary = seq(2, 2 * 32^3, 2)),
                c(32, 32, 32))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a7_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a7_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a7_obs)
stopifnot(identical(a7_obs, a7_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a7_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a7_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a7_zlib_obs)
stopifnot(identical(a7_zlib_obs, a7_exp))
