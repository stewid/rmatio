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
## Check write and read of structure arrays in MAT5 format:
## 1) without compression
## 2) with compression
##
## In rmatio, structure arrays are mapped to a named list

##
## Note:
## If the list contains elements of differents lengths i.e.
## a14_in <- list(y = c("a", "bb"), z = c(1, 2))                      # nolint
## then the expected result is not identical, since each element
## is saved in a cell and the expected result of a14_in is therefore
## a14_exp <- list(y = list("a", "bb"), z = list(c(1, 2)))            # nolint
##

##
## structure: case-1 (Empty structure array)
##
a1_exp <- structure(list(), .Names = character(0))
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
## structure: case-2 (Empty structure array with fields)
##
a2_exp <- list(field1 = list(), field2 = list())
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
## structure: case-3 (Structure array with empty fields)
##
a3_exp <- list(field1 = numeric(0), field2 = character(0),
               field3 = complex(0), filed4 = integer(0),
               field5 = logical(0))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a3_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a3_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a3_obs)
stopifnot(identical(a3_obs, a3_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a3_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a3_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a3_zlib_obs)
stopifnot(identical(a3_zlib_obs, a3_exp))

##
## structure: case-4
##
a4_exp <- list(field1 = list(1, 14),
               field2 = list(array(as.numeric(2:13), c(3, 4)),
                             array(as.numeric(15:26), c(3, 4))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a4_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a4_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a4_obs)
stopifnot(identical(a4_obs, a4_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a4_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a4_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a4_zlib_obs)
stopifnot(identical(a4_zlib_obs, a4_exp))

##
## structure: case-5
##
a5_exp <- list(field1 = list(1L, 14L),
               field2 = list(array(2:13, c(3, 4)),
                   array(15:26, c(3, 4))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a5_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a5_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a5_obs)
stopifnot(identical(a5_obs, a5_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a5_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a5_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a5_zlib_obs)
stopifnot(identical(a5_zlib_obs, a5_exp))

##
## structure: case-6
##
a6_exp <- list(field1 = list(1 + 51i, 14 + 64i),
               field2 = list(array(c(2 + 52i, 3 + 53i, 4 + 54i,
                                     5 + 55i, 6 + 56i, 7 + 57i,
                                     8 + 58i, 9 + 59i, 10 + 60i,
                                     11 + 61i, 12 + 62i, 13 + 63i),
                                   c(3, 4)),
                             array(c(15 + 65i, 16 + 66i, 17 + 67i,
                                     18 + 68i, 19 + 69i, 20 + 70i,
                                     21 + 71i, 22 + 72i, 23 + 73i,
                                     24 + 74i, 25 + 75i, 26 + 76i),
                                   c(3, 4))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a6_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a6_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a6_obs)
stopifnot(identical(a6_obs, a6_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a6_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a6_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a6_zlib_obs)
stopifnot(identical(a6_zlib_obs, a6_exp))

##
## structure: case-7
##
a7_exp <- list(field1 = list(triu(Matrix(1:20, nrow = 4, ncol = 5,
                                         sparse = TRUE))),
               field2 = list(tril(Matrix(1:20, nrow = 5, ncol = 4,
                                         sparse = TRUE, byrow = TRUE))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a7_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a7_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a7_obs)
stopifnot(identical(a7_obs, a7_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a7_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a7_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a7_zlib_obs)
stopifnot(identical(a7_zlib_obs, a7_exp))

##
## structure: case-8
##
a8_exp <- list(field1 = list(array(c(1 + 21i, 0 + 0i, 0 + 0i, 0 + 0i,
                                     5 + 25i, 6 + 26i, 0 + 0i, 0 + 0i,
                                     9 + 29i, 10 + 30i, 11 + 31i,
                                     0 + 0i, 13 + 33i, 14 + 34i,
                                     15 + 35i, 16 + 36i, 17 + 37i,
                                     18 + 38i, 19 + 39i, 20 + 40i),
                                   c(4, 5))),
               field2 = list(array(c(1 - 21i, 5 - 25i, 9 - 29i,
                                     13 - 33i, 17 - 37i, 0 + 0i,
                                     6 - 26i, 10 - 30i, 14 - 34i,
                                     18 - 38i, 0 + 0i, 0 + 0i,
                                     11 - 31i, 15 - 35i, 19 - 39i,
                                     0 + 0i, 0 + 0i, 0 + 0i, 16 - 36i,
                                     20 - 40i),
                                   c(5, 4))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a8_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a8_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a8_obs)
stopifnot(identical(a8_obs, a8_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a8_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a8_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a8_zlib_obs)
stopifnot(identical(a8_zlib_obs, a8_exp))

##
## structure: case-9
##
a9_exp <- list(field1 = c("abcdefghijklmnopqrstuvwxyz",
                   "1234567890!@#$%^&*()-_=+`~"), #
               field2 = c("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                   "[{]}\\|;:'\",<.>/?          "))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a9_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a9_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a9_obs)
stopifnot(identical(a9_obs, a9_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a9_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a9_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a9_zlib_obs)
stopifnot(identical(a9_zlib_obs, a9_exp))

##
## structure: case-10 (Structure array with empty fields)
##
a10_exp <- list(field1 = numeric(0))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a10_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a10_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a10_obs)
stopifnot(identical(a10_obs, a10_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a10_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a10_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
stopifnot(identical(a10_zlib_obs, a10_exp))

##
## structure: case-11
##
a11_exp <- list(field1 = list(1))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a11_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a11_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a11_obs)
stopifnot(identical(a11_obs, a11_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a11_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a11_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a11_zlib_obs)
stopifnot(identical(a11_zlib_obs, a11_exp))

##
## structure: case-12
##
a12_exp <- structure(list(
    field1 = list(structure(c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                              FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                              FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                              FALSE, TRUE), .Dim = 4:5),
                  structure(c(TRUE, TRUE, TRUE, TRUE, TRUE,
                              FALSE, TRUE, TRUE, TRUE, TRUE,
                              FALSE, FALSE, TRUE, TRUE, TRUE,
                              FALSE, FALSE, FALSE, TRUE, TRUE,
                              FALSE, FALSE, FALSE, FALSE, TRUE),
                            .Dim = c(5L, 5L))),
    field2 = list(structure(c(TRUE, FALSE, TRUE, FALSE, TRUE,
                              FALSE, TRUE, FALSE, TRUE, FALSE,
                              TRUE, FALSE, TRUE, FALSE, TRUE,
                              FALSE, TRUE, FALSE, TRUE, FALSE),
                            .Dim = 4:5),
                  structure(c(TRUE, FALSE,
                              FALSE, FALSE, FALSE, TRUE, TRUE,
                              FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
                              FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,
                              FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
                            .Dim = c(5L, 5L)))),
    .Names = c("field1", "field2"))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a12_exp), filename = filename, compression = FALSE,
          version = "MAT5")
a12_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a12_obs)
stopifnot(identical(a12_obs, a12_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a12_exp), filename = filename, compression = TRUE,
          version = "MAT5")
a12_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a12_zlib_obs)
stopifnot(identical(a12_zlib_obs, a12_exp))

##
## structure: case-13
##
a13_exp <- structure(list(
    X = structure(list(
        x = list(structure(c(1, 4, 2, 5, 3, 6.2),
                           .Dim = 2:3)),
        y = list(list("Hello", "world!")),
        z = list(structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                           .Dim = c(14L, 14L)))),
        .Names = c("x", "y", "z"))),
    .Names = "X")
filename <- tempfile(fileext = ".mat")
write.mat(a13_exp, filename = filename, compression = FALSE,
          version = "MAT5")
a13_obs <- read.mat(filename)
unlink(filename)
str(a13_obs)
stopifnot(identical(a13_obs, a13_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a13_exp, filename = filename, compression = TRUE,
          version = "MAT5")
a13_zlib_obs <- read.mat(filename)
unlink(filename)
str(a13_zlib_obs)
stopifnot(identical(a13_zlib_obs, a13_exp))

##
## structure: case-14
##
a14_in <- list(y = c("a", "bb"), z = c(1, 2))
a14_exp <- list(y = list("a", "bb"), z = list(c(1, 2)))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a14_in), filename = filename, compression = FALSE,
          version = "MAT5")
a14_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a14_obs)
stopifnot(identical(a14_obs, a14_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a14_in), filename = filename, compression = TRUE,
          version = "MAT5")
a14_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a14_zlib_obs)
stopifnot(identical(a14_zlib_obs, a14_exp))

##
## structure: case-15
##
a15_in <- list(y = c("a", "bb"), z = c("c", "dd"))
a15_exp <- list(y = list("a", "bb"), z = list("c", "dd"))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a15_in), filename = filename, compression = FALSE,
          version = "MAT5")
a15_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a15_obs)
stopifnot(identical(a15_obs, a15_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a15_in), filename = filename, compression = TRUE,
          version = "MAT5")
a15_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a15_zlib_obs)
stopifnot(identical(a15_zlib_obs, a15_exp))

##
## structure: case-16
##
a16_in <- list(y = c("a", "b"), z = c(1, 2))
a16_exp <- list(a = list(y = list("a", "b"), z = list(c(1, 2))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a16_in), filename = filename, compression = FALSE,
          version = "MAT5")
a16_obs <- read.mat(filename)
unlink(filename)
str(a16_obs)
stopifnot(identical(a16_obs, a16_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a16_in), filename = filename, compression = TRUE,
          version = "MAT5")
a16_zlib_obs <- read.mat(filename)
unlink(filename)
str(a16_zlib_obs)
stopifnot(identical(a16_zlib_obs, a16_exp))

##
## structure: case-17
##
a17_in <- list(y = c("a", "b"), z = c(1, 2, 3))
a17_exp <- list(a = list(y = list("a", "b"), z = list(c(1, 2, 3))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a17_in), filename = filename, compression = FALSE,
          version = "MAT5")
a17_obs <- read.mat(filename)
unlink(filename)
str(a17_obs)
stopifnot(identical(a17_obs, a17_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a17_in), filename = filename, compression = TRUE,
          version = "MAT5")
a17_zlib_obs <- read.mat(filename)
unlink(filename)
str(a17_zlib_obs)
stopifnot(identical(a17_zlib_obs, a17_exp))

##
## structure: case-18
##
a18_in <- list(y = c("a", "bb"), z = c(1, 2, 3))
a18_exp <- list(a = list(y = list("a", "bb"), z = list(c(1, 2, 3))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a18_in), filename = filename, compression = FALSE,
          version = "MAT5")
a18_obs <- read.mat(filename)
unlink(filename)
str(a18_obs)
stopifnot(identical(a18_obs, a18_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a18_in), filename = filename, compression = TRUE,
          version = "MAT5")
a18_zlib_obs <- read.mat(filename)
unlink(filename)
str(a18_zlib_obs)
stopifnot(identical(a18_zlib_obs, a18_exp))

##
## structure: case-19
##
a19_in <- list(y = c("a", "bb"), z = c(1, 2))
a19_exp <- list(y = list("a", "bb"), z = c(1, 2))
filename <- tempfile(fileext = ".mat")
write.mat(a19_in, filename = filename, compression = FALSE,
          version = "MAT5")
a19_obs <- read.mat(filename)
unlink(filename)
str(a19_obs)
stopifnot(identical(a19_obs, a19_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a19_in, filename = filename, compression = TRUE,
          version = "MAT5")
a19_zlib_obs <- read.mat(filename)
unlink(filename)
str(a19_zlib_obs)
stopifnot(identical(a19_zlib_obs, a19_exp))

##
## structure: case-20
##
a20_in <- list(y = c("a", "bb"), z = c("c", "dd"))
a20_exp <- list(y = list("a", "bb"), z = list("c", "dd"))
filename <- tempfile(fileext = ".mat")
write.mat(a20_in, filename = filename, compression = FALSE,
          version = "MAT5")
a20_obs <- read.mat(filename)
unlink(filename)
str(a20_obs)
stopifnot(identical(a20_obs, a20_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a20_in, filename = filename, compression = TRUE,
          version = "MAT5")
a20_zlib_obs <- read.mat(filename)
unlink(filename)
str(a20_zlib_obs)
stopifnot(identical(a20_zlib_obs, a20_exp))

##
## structure: case-21
##
a21_exp <- list(y = c("a", "b"), z = c(1, 2))
filename <- tempfile(fileext = ".mat")
write.mat(a21_exp, filename = filename, compression = FALSE,
          version = "MAT5")
a21_obs <- read.mat(filename)
unlink(filename)
str(a21_obs)
stopifnot(identical(a21_obs, a21_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a21_exp, filename = filename, compression = TRUE,
          version = "MAT5")
a21_zlib_obs <- read.mat(filename)
unlink(filename)
str(a21_zlib_obs)
stopifnot(identical(a21_zlib_obs, a21_exp))

##
## structure: case-22
##
a22_exp <- list(y = c("a", "b"), z = c(1, 2, 3))
filename <- tempfile(fileext = ".mat")
write.mat(a22_exp, filename = filename, compression = FALSE,
          version = "MAT5")
a22_obs <- read.mat(filename)
unlink(filename)
str(a22_obs)
stopifnot(identical(a22_obs, a22_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a22_exp, filename = filename, compression = TRUE,
          version = "MAT5")
a22_zlib_obs <- read.mat(filename)
unlink(filename)
str(a22_zlib_obs)
stopifnot(identical(a22_zlib_obs, a22_exp))

##
## structure: case-23
##
a23_in <- list(y = c("a", "bb"), z = c(1, 2, 3))
a23_exp <- list(y = list("a", "bb"), z = c(1, 2, 3))
filename <- tempfile(fileext = ".mat")
write.mat(a23_in, filename = filename, compression = FALSE,
          version = "MAT5")
a23_obs <- read.mat(filename)
unlink(filename)
str(a23_obs)
stopifnot(identical(a23_obs, a23_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a23_in, filename = filename, compression = TRUE,
          version = "MAT5")
a23_zlib_obs <- read.mat(filename)
unlink(filename)
str(a23_zlib_obs)
stopifnot(identical(a23_zlib_obs, a23_exp))

##
## structure: case-24
##
a24_in <- list(y = c("a", "bb"), z = list(c("d", "eee")))
a24_exp <- list(a = list(y = list("a", "bb"),
                         z = list(list(list("d", "eee")))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a24_in), filename = filename, compression = FALSE,
          version = "MAT5")
a24_obs <- read.mat(filename)
unlink(filename)
str(a24_obs)
stopifnot(identical(a24_obs, a24_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a24_in), filename = filename, compression = TRUE,
          version = "MAT5")
a24_zlib_obs <- read.mat(filename)
unlink(filename)
str(a24_zlib_obs)
stopifnot(identical(a24_zlib_obs, a24_exp))

##
## structure: case-25
##
a25_in <- list(y = c("a", "bb"), z = list(c("d", "eee")))
a25_exp <- list(y = list("a", "bb"), z = list(list("d", "eee")))
filename <- tempfile(fileext = ".mat")
write.mat(a25_in, filename = filename, compression = FALSE,
          version = "MAT5")
a25_obs <- read.mat(filename)
unlink(filename)
str(a25_obs)
stopifnot(identical(a25_obs, a25_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a25_in, filename = filename, compression = TRUE,
          version = "MAT5")
a25_zlib_obs <- read.mat(filename)
unlink(filename)
str(a25_zlib_obs)
stopifnot(identical(a25_zlib_obs, a25_exp))

##
## structure: case-26
##
a26_in <- list(y = c("a", "bb"), z = Matrix(c(0, 0, 0, 0, 0, 0, 1, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0,
                                              1, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 1), nrow = 3,
                                            ncol = 9, byrow = TRUE,
                                            sparse = TRUE))
a26_exp <- list(y = list("a", "bb"), z = a26_in$z)
filename <- tempfile(fileext = ".mat")
write.mat(a26_in, filename = filename, compression = FALSE,
          version = "MAT5")
a26_obs <- read.mat(filename)
unlink(filename)
str(a26_obs)
stopifnot(identical(a26_obs, a26_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a26_in, filename = filename, compression = TRUE,
          version = "MAT5")
a26_zlib_obs <- read.mat(filename)
unlink(filename)
str(a26_zlib_obs)
stopifnot(identical(a26_zlib_obs, a26_exp))

##
## structure: case-27
##
a27_in <- list(y = c("a", "bb"), z = Matrix(c(0, 0, 0, 0, 0, 0, 1, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0,
                                              1, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 1), nrow = 3,
                                            ncol = 9, byrow = TRUE,
                                            sparse = TRUE))
a27_exp <- list(y = list("a", "bb"), z = list(a27_in$z))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a27_in),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a27_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a27_obs)
stopifnot(identical(a27_obs, a27_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a27_in),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a27_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a27_zlib_obs)
stopifnot(identical(a27_zlib_obs, a27_exp))

##
## structure: case-28
##
a28_in <- list(y = list(c("a", "bb")), z = list())
a28_exp <- list(y = list(list(list("a", "bb"))), z = list())
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a28_in),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a28_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a28_obs)
stopifnot(identical(a28_obs, a28_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a28_in),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a28_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a28_zlib_obs)
stopifnot(identical(a28_zlib_obs, a28_exp))

##
## structure: case-29
##
a29_in <- list(y = list(c("a", "bb")), z = list())
a29_exp <- list(y = list(list("a", "bb")), z = list())
filename <- tempfile(fileext = ".mat")
write.mat(a29_in, filename = filename, compression = FALSE,
          version = "MAT5")
a29_obs <- read.mat(filename)
unlink(filename)
str(a29_obs)
stopifnot(identical(a29_obs, a29_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(a29_in, filename = filename, compression = TRUE,
          version = "MAT5")
a29_zlib_obs <- read.mat(filename)
unlink(filename)
str(a29_zlib_obs)
stopifnot(identical(a29_zlib_obs, a29_exp))
