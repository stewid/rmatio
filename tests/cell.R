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
## Check write and read of cell arrays in MAT5 format:
## 1) without compression
## 2) with compression
##
## In rmatio, cell arrays are mapped to an unnamed list

##
## Note:
## If the list contains elements of differents lengths i.e.
## a14_in <- list(c("a", "bb"), c("c", "dd"))                         # nolint
## then the expected result is not identical, since each element
## is saved in a cell and the expected result of a14_in is therefore
## a14_exp <- list(list("a", "bb"), list("c", "dd"))                  # nolint
##

##
## cell: case-1
##
a1_exp <- list()
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
## cell: case-2
##
a2_exp <- list(complex(0), logical(0), character(0), numeric(0), integer(0))
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

##
## cell: case-3
##
a3_exp <- list(list(array(c(1, 3, 2, 4), c(2, 2)),
                    array(c(5, 8, 6, 9, 7, 10), c(2, 3)),
                    array(c(11, 15, 12, 16, 13, 17, 14, 18), c(2, 4))),
               list(array(c(19, 21, 20, 22), c(2, 2)),
                    array(c(23, 25, 27, 24, 26, 28), c(3L, 2L)),
                    array(c(29, 31, 33, 35, 30, 32, 34, 36), c(4, 2))))
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
## cell: case-4
##
a4_exp <- list(list(array(c(1L, 3L, 2L, 4L), c(2, 2)),
                   array(c(5L, 8L, 6L, 9L, 7L, 10L), c(2, 3)),
                   array(c(11L, 15L, 12L, 16L, 13L, 17L, 14L, 18L), c(2, 4))),
              list(array(c(19L, 21L, 20L, 22L), c(2, 2)),
                   array(c(23L, 25L, 27L, 24L, 26L, 28L), c(3L, 2L)),
                   array(c(29L, 31L, 33L, 35L, 30L, 32L, 34L, 36L), c(4, 2))))
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
## cell: case-5
##
a5_exp <- list(list(triu(Matrix(1:20, nrow = 4, ncol = 5, sparse = TRUE)),
                    tril(Matrix(1:20, nrow = 5, ncol = 4, sparse = TRUE,
                                byrow = TRUE))))
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
## cell: case-6
##
a6_exp <- list(array(c(1 + 21i, 0 + 0i, 0 + 0i, 0 + 0i, 5 + 25i,
                       6 + 26i, 0 + 0i, 0 + 0i, 9 + 29i, 10 + 30i,
                       11 + 31i, 0 + 0i, 13 + 33i, 14 + 34i, 15 + 35i,
                       16 + 36i, 17 + 37i, 18 + 38i, 19 + 39i,
                       20 + 40i),
                     c(4, 5)),
               array(c(1 - 21i, 5 - 25i, 9 - 29i, 13 - 33i, 17 - 37i,
                       0 + 0i, 6 - 26i, 10 - 30i, 14 - 34i, 18 - 38i,
                       0 + 0i, 0 + 0i, 11 - 31i, 15 - 35i, 19 - 39i,
                       0 + 0i, 0 + 0i, 0 + 0i, 16 - 36i, 20 - 40i),
                     c(5, 4)))
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
## cell: case-7
##
a7_exp <- list(list("abcdefghijklmnopqrstuvwxyz",
                    "1234567890!@#$%^&*()-_=+`~"), #
               list("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                    "[{]}\\|;:'\",<.>/?          "))
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
## cell: case-8
##
a8_exp <- list(structure(list(), .Names = character(0)),
               list(),
               structure(list(field1 = numeric(0),
                              field2 = character(0)),
                         .Names = c("field1", "field2")))
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
## cell: case-9
##
a9_exp <- list(list(structure(list(
    field1 = list(1, 14),
    field2 = list(
        structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                  .Dim = 3:4),
        structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26),
                  .Dim = 3:4))),
    .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1, 14),
        field2 = list(structure(c(2, 3, 4, 5, 6, 7, 8,
                                  9, 10, 11, 12, 13),
                                .Dim = 3:4),
                      structure(c(15, 16, 17, 18, 19,
                                  20, 21, 22, 23, 24,
                                  25, 26),
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1, 14),
        field2 = list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                  13),
                                .Dim = 3:4),
                      structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23,
                                  24, 25, 26),
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1, 14),
        field2 = list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                  13),
                                .Dim = 3:4),
                      structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23,
                                  24, 25, 26),
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1L, 14L),
        field2 = list(structure(2:13,
                                 .Dim = 3:4),
                      structure(15:26,
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1, 14),
        field2 = list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                  13),
                                .Dim = 3:4),
                      structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23,
                                  24, 25, 26),
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1L, 14L),
        field2 = list(structure(2:13,
                                .Dim = 3:4),
                      structure(15:26,
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1L, 14L),
        field2 = list(structure(2:13,
                                .Dim = 3:4),
                      structure(15:26,
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1L, 14L),
        field2 = list(structure(2:13,
                                .Dim = 3:4),
                      structure(15:26,
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
    structure(list(
        field1 = list(1L, 14L),
        field2 = list(structure(2:13,
                                .Dim = 3:4),
                      structure(15:26,
                                .Dim = 3:4))),
        .Names = c("field1", "field2"))),
    list(structure(list(
        field1 = list(1 + 51i, 14 + 64i),
        field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i, 5 + 55i,
                                  6 + 56i, 7 + 57i, 8 + 58i, 9 + 59i,
                                  10 + 60i, 11 + 61i, 12 + 62i,
                                  13 + 63i),
                                .Dim = 3:4),
                      structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                  18 + 68i, 19 + 69i, 20 + 70i,
                                  21 + 71i, 22 + 72i, 23 + 73i,
                                  24 + 74i, 25 + 75i, 26 + 76i),
                                .Dim = 3:4))),
        .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2")),
        structure(list(
            field1 = list(1 + 51i, 14 + 64i),
            field2 = list(structure(c(2 + 52i, 3 + 53i, 4 + 54i,
                                      5 + 55i, 6 + 56i, 7 + 57i,
                                      8 + 58i, 9 + 59i, 10 + 60i,
                                      11 + 61i, 12 + 62i, 13 + 63i),
                                    .Dim = 3:4),
                          structure(c(15 + 65i, 16 + 66i, 17 + 67i,
                                      18 + 68i, 19 + 69i, 20 + 70i,
                                      21 + 71i, 22 + 72i, 23 + 73i,
                                      24 + 74i, 25 + 75i, 26 + 76i),
                                    .Dim = 3:4))),
            .Names = c("field1", "field2"))))
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
## cell: case-10
##
a10_exp <- list(list(field1 = list(triu(Matrix(1:20, nrow = 4, ncol = 5,
                                               sparse = TRUE))),
                     field2 = list(tril(Matrix(1:20, nrow = 5, ncol = 4,
                                             sparse = TRUE,
                                             byrow = TRUE)))),
                list(field1 = list(array(c(1 + 21i, 0 + 0i, 0 + 0i,
                                           0 + 0i, 5 + 25i, 6 + 26i,
                                           0 + 0i, 0 + 0i, 9 + 29i,
                                           10 + 30i, 11 + 31i, 0 + 0i,
                                           13 + 33i, 14 + 34i,
                                           15 + 35i, 16 + 36i,
                                           17 + 37i, 18 + 38i,
                                           19 + 39i, 20 + 40i),
                                         c(4, 5))),
                     field2 = list(array(c(1 - 21i, 5 - 25i, 9 - 29i,
                                           13 - 33i, 17 - 37i, 0 + 0i,
                                           6 - 26i, 10 - 30i,
                                           14 - 34i, 18 - 38i, 0 + 0i,
                                           0 + 0i, 11 - 31i, 15 - 35i,
                                           19 - 39i, 0 + 0i, 0 + 0i,
                                           0 + 0i, 16 - 36i, 20 - 40i),
                                         c(5, 4)))))
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
write.mat(list(a = a10_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a10_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a10_zlib_obs)
stopifnot(identical(a10_zlib_obs, a10_exp))

##
## cell: case-11
##
a11_exp <- list(list(field1 = "abcdefghijklmnopqrstuvwxyz",
                     field2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
                list(field1 = "1234567890!@#$%^&*()-_=+`~", #
                     field2 = "[{]}\\|;:'\",<.>/?          "))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a11_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a11_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a11_obs)
stopifnot(identical(a11_obs, a11_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a11_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a11_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a11_zlib_obs)
stopifnot(identical(a11_zlib_obs, a11_exp))

##
## cell: case-12
##
a12_exp <- list(structure(c(FALSE, TRUE, FALSE, TRUE, FALSE,
                            TRUE, FALSE, TRUE, FALSE, TRUE,
                            FALSE, TRUE, FALSE, TRUE, FALSE,
                            TRUE, FALSE, TRUE, FALSE, TRUE),
                          .Dim = 4:5),
                structure(c(TRUE, FALSE, TRUE,
                            FALSE, TRUE, FALSE, TRUE, FALSE,
                            TRUE, FALSE, TRUE, FALSE, TRUE,
                            FALSE, TRUE, FALSE, TRUE, FALSE,
                            TRUE, FALSE),
                          .Dim = 4:5),
                structure(c(TRUE, TRUE, TRUE,
                            TRUE, TRUE, FALSE, TRUE, TRUE,
                            TRUE, TRUE, FALSE, FALSE, TRUE,
                            TRUE, TRUE, FALSE, FALSE, FALSE,
                            TRUE, TRUE, FALSE, FALSE, FALSE,
                            FALSE, TRUE),
                          .Dim = c(5L, 5L)),
                structure(c(TRUE, FALSE,
                            FALSE, FALSE, FALSE, TRUE, TRUE,
                            FALSE, FALSE, FALSE, TRUE, TRUE,
                            TRUE, FALSE, FALSE, TRUE, TRUE,
                            TRUE, TRUE, FALSE, TRUE, TRUE,
                            TRUE, TRUE, TRUE),
                          .Dim = c(5L, 5L)))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a12_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a12_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a12_obs)
stopifnot(identical(a12_obs, a12_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a12_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a12_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a12_zlib_obs)
stopifnot(identical(a12_zlib_obs, a12_exp))

##
## cell: case-13
##
a13_exp <- list(structure(list(),
                          .Names = character(0)),
                list(field1 = list(), field2 = list()),
                structure(list(field1 = numeric(0),
                               field2 = character(0)),
                          .Names = c("field1", "field2")))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a13_exp),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a13_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a13_obs)
stopifnot(identical(a13_obs, a13_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a13_exp),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a13_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a13_zlib_obs)
stopifnot(identical(a13_zlib_obs, a13_exp))

##
## cell: case-14
##
a14_in <- list(c("a", "bb"), c("c", "dd"))
a14_exp <- list(list("a", "bb"), list("c", "dd"))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a14_in),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a14_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a14_obs)
stopifnot(identical(a14_obs, a14_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a14_in),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a14_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a14_zlib_obs)
stopifnot(identical(a14_zlib_obs, a14_exp))

##
## cell: case-15
##
a15_in <- list(c("a", "bb"), list(c("d", "eee")))
a15_exp <- list(list("a", "bb"), list(list(list("d", "eee"))))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a15_in),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a15_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a15_obs)
stopifnot(identical(a15_obs, a15_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a15_in),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a15_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a15_zlib_obs)
stopifnot(identical(a15_zlib_obs, a15_exp))

##
## cell: case-16
##
a16_in <- list(c("a", "bb"),
               Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                      nrow = 3,
                      ncol = 9,
                      byrow = TRUE,
                      sparse = TRUE))
a16_exp <- list(list("a", "bb"), list(a16_in[[2]]))
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a16_in),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a16_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a16_obs)
stopifnot(identical(a16_obs, a16_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a16_in),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a16_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a16_zlib_obs)
stopifnot(identical(a16_zlib_obs, a16_exp))

##
## cell: case-17
##
a17_in <- list(list(c("a", "bb")), list())
a17_exp <- list(list(list(list("a", "bb"))), list())
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a17_in),
          filename = filename,
          compression = FALSE,
          version = "MAT5")
a17_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a17_obs)
stopifnot(identical(a17_obs, a17_exp))

## Run the same test with compression
filename <- tempfile(fileext = ".mat")
write.mat(list(a = a17_in),
          filename = filename,
          compression = TRUE,
          version = "MAT5")
a17_zlib_obs <- read.mat(filename)[["a"]]
unlink(filename)
str(a17_zlib_obs)
stopifnot(identical(a17_zlib_obs, a17_exp))
