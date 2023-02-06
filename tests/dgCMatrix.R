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
library(Matrix)

## For debugging
sessionInfo()

##
## Check write and read of dgCMatrix in MAT5 format:
## 1) without compression
## 2) with compression
##

##
## dgCMatrix: case-1
##
a1_exp <- Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                 nrow = 3,
                 ncol = 9,
                 byrow = TRUE,
                 sparse = TRUE)
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a1_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a1_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a1_obs)
stopifnot(identical(a1_obs, a1_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a1_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a1_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a1_zlib_obs)
stopifnot(identical(a1_zlib_obs, a1_exp))

##
## dgCMatrix: case-2
##
a2_exp <- as(diag(1:5), "dgCMatrix")
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a2_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a2_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a2_obs)
stopifnot(identical(a2_obs, a2_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a2_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a2_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a2_zlib_obs)
stopifnot(identical(a2_zlib_obs, a2_exp))
