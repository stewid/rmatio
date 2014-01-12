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
## Check write and read of cell arrays in MAT5 format:
## 1) without compression
## 2) with compression
##
## In rmatio, cell arrays are mapped to an unnamed list

filename <- tempfile(fileext = ".mat")

##
## cell: case-1
##
a.in <- list()

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-2
##
a.in <- list(complex(0),
             logical(0),
             character(0),
             numeric(0),
             integer(0))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-3
##
a.in <- list(list(array(c(1, 3, 2, 4), c(2, 2)),
                  array(c(5, 8, 6, 9, 7, 10), c(2,3)),
                  array(c(11, 15, 12, 16, 13, 17, 14, 18), c(2, 4))),
             list(array(c(19, 21, 20, 22), c(2, 2)),
                  array(c(23, 25, 27, 24, 26, 28), c(3L, 2L)),
                  array(c(29, 31, 33, 35, 30, 32, 34, 36), c(4, 2))))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-4
##
a.in <- list(list(array(c(1L, 3L, 2L, 4L), c(2, 2)),
                  array(c(5L, 8L, 6L, 9L, 7L, 10L), c(2,3)),
                  array(c(11L, 15L, 12L, 16L, 13L, 17L, 14L, 18L), c(2, 4))),
             list(array(c(19L, 21L, 20L, 22L), c(2, 2)),
                  array(c(23L, 25L, 27L, 24L, 26L, 28L), c(3L, 2L)),
                  array(c(29L, 31L, 33L, 35L, 30L, 32L, 34L, 36L), c(4, 2))))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-5
##
a.in <- list(list(triu(Matrix(1:20, nrow=4, ncol=5, sparse=TRUE)),
                  tril(Matrix(1:20, nrow=5, ncol=4, sparse=TRUE, byrow=TRUE))))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-6
##
a.in <- list(array(c(1+21i, 0+0i, 0+0i, 0+0i, 5+25i,
                     6+26i, 0+0i, 0+0i, 9+29i, 10+30i, 11+31i, 0+0i,
                     13+33i, 14+34i, 15+35i, 16+36i, 17+37i, 18+38i,
                     19+39i, 20+40i), c(4,5)),
             array(c(1-21i, 5-25i, 9-29i, 13-33i, 17-37i,
                     0+0i, 6-26i, 10-30i, 14-34i, 18-38i, 0+0i, 0+0i,
                     11-31i, 15-35i, 19-39i, 0+0i, 0+0i, 0+0i,
                     16-36i, 20-40i), c(5,4)))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-7
##
a.in <- list(list("abcdefghijklmnopqrstuvwxyz",
                  "1234567890!@#$%^&*()-_=+`~"), #
             list("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                  "[{]}\\|;:'\",<.>/?          "))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-8
##
a.in <- list(structure(list(),
                       .Names = character(0)),
             list(),
             structure(list(field1 = numeric(0),
                            field2 = character(0)),
                       .Names = c("field1", "field2")))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-9
##
filename <- tempfile(fileext = ".mat")
on.exit(unlink(filename))

a.in <- list(list(structure(list(
    field1 = list(1, 14),
    field2 = list(
        structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                  .Dim = 3:4),
        structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26 ),
                  .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1, 14),
                      field2 = list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                          .Dim = 3:4),
                          structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26),
                                    .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1, 14),
                      field2 = list( structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                          .Dim = 3:4),
                          structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26 ),
                                    .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1, 14),
                      field2 = list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                          .Dim = 3:4),
                          structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1L, 14L),
                      field2 = list( structure(2:13,
                          .Dim = 3:4),
                          structure(15:26,
                                    .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1, 14),
                      field2 = list( structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                          .Dim = 3:4),
                          structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26 ),
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
                      field2 = list( structure(2:13, .Dim = 3:4),
                          structure(15:26,
                                    .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1L, 14L),
                      field2 = list( structure(2:13, .Dim = 3:4),
                          structure(15:26,
                                    .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1L, 14L),
                      field2 = list( structure(2:13, .Dim = 3:4),
                          structure(15:26, .Dim = 3:4))),
                            .Names = c("field1", "field2"))),
             list(structure(list(
                 field1 = list(1+51i, 14+64i),
                 field2 = list(structure(c(2+52i, 3+53i, 4+54i, 5+55i, 6+56i,
                     7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                     13+63i),
                     .Dim = 3:4),
                     structure(c(15+65i, 16+66i,
                                 17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                 23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i),
                          .Dim = 3:4),
                          structure(c(15+65i, 16+66i,
                                      17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                      23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                                       17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                       23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                                       17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                       23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                                       17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                       23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                                       17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                       23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                                       17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                       23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                                       17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                       23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                                       17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                       23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2")),
                  structure(list(
                      field1 = list(1+51i, 14+64i),
                      field2 = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                          6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                          13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                                       17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                                       23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                            .Names = c("field1", "field2"))))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-10
##
a.in <- list(list(field1=list(triu(Matrix(1:20, nrow=4,
                      ncol=5, sparse=TRUE))),
                  field2=list(tril(Matrix(1:20, nrow=5, ncol=4,
                      sparse=TRUE, byrow=TRUE)))),
             list(field1=list(array(c(1+21i, 0+0i,
                      0+0i, 0+0i, 5+25i, 6+26i, 0+0i, 0+0i, 9+29i,
                      10+30i, 11+31i, 0+0i, 13+33i, 14+34i, 15+35i,
                      16+36i, 17+37i, 18+38i, 19+39i, 20+40i),
                      c(4,5))),
                  field2=list(array(c(1-21i, 5-25i,
                      9-29i, 13-33i, 17-37i, 0+0i, 6-26i, 10-30i,
                      14-34i, 18-38i, 0+0i, 0+0i, 11-31i, 15-35i,
                      19-39i, 0+0i, 0+0i, 0+0i, 16-36i, 20-40i),
                      c(5,4)))))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-11
##
a.in <- list(list(field1 = "abcdefghijklmnopqrstuvwxyz",
                  field2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
             list(field1 = "1234567890!@#$%^&*()-_=+`~", #
                  field2 = "[{]}\\|;:'\",<.>/?          "))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-12
##
a.in <- list(structure(c(FALSE, TRUE, FALSE, TRUE, FALSE,
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
                         FALSE, TRUE ),
                       .Dim = c(5L, 5L)),
             structure(c(TRUE, FALSE,
                         FALSE, FALSE, FALSE, TRUE, TRUE,
                         FALSE, FALSE, FALSE, TRUE, TRUE,
                         TRUE, FALSE, FALSE, TRUE, TRUE,
                         TRUE, TRUE, FALSE, TRUE, TRUE,
                         TRUE, TRUE, TRUE),
                       .Dim = c(5L, 5L)))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-13
##
a.in <- list(structure(list(),
                       .Names = character(0)),
             list(field1=list(), field2=list()),
             structure(list(field1 = numeric(0),
                            field2 = character(0)),
                       .Names = c("field1", "field2")))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, a.in))

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

##
## cell: case-14
##
a.in <- list(c("a", "bb"), c("c", "dd"))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, list(list("a", "bb"), list("c", "dd"))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, list(list("a", "bb"), list("c", "dd"))))
}

##
## cell: case-15
##
a.in <- list(c("a", "bb"), list(c('d', 'eee')))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, list(list("a", "bb"),
                           list(list(list("d", "eee"))))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, list(list("a", "bb"),
                               list(list(list("d", "eee"))))))
}

##
## cell: case-16
##
a.in <- list(c("a", "bb"), Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 1, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 1),
                                  nrow=3,
                                  ncol=9,
                                  byrow=TRUE,
                                  sparse=TRUE))

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, list(list("a", "bb"),
                           list(Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 1, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0, 1),
                                       nrow=3,
                                       ncol=9,
                                       byrow=TRUE,
                                       sparse=TRUE)))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, list(list("a", "bb"),
                               list(Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                             0, 0, 0, 0, 0, 0, 0, 1, 0,
                                             0, 0, 0, 0, 0, 0, 0, 0, 1),
                                           nrow=3,
                                           ncol=9,
                                           byrow=TRUE,
                                           sparse=TRUE)))))
}

##
## cell: case-17
##
a.in <- list(list(c("a", "bb")), list())

write.mat(list(a=a.in),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.out <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.out, list(list(list(list("a", "bb"))), list())))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.in),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.out <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.out, list(list(list(list("a", "bb"))), list())))
}
