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
## Check write and read of structure arrays in MAT5 format:
## 1) without compression
## 2) with compression
##
## In rmatio, structure arrays are mapped to a named list

filename <- tempfile(fileext = ".mat")

##
## structure: case-1 (Empty structure array)
##
a.1 <- structure(list(), .Names = character(0))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-2 (Empty structure array with fields)
##
a.1 <- list(field1=list(), field2=list())

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-3 (Structure array with empty fields)
##
a.1 <- list(field1=numeric(0),
            field2=character(0),
            field3=complex(0),
            filed4=integer(0),
            field5=logical(0))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-4
##
a.1 <- list(field1=list(1, 14),
            field2=list(array(as.numeric(2:13), c(3,4)),
                array(as.numeric(15:26), c(3,4))))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-5
##
a.1 <- list(field1=list(1L, 14L),
            field2=list(array(2:13, c(3,4)),
                array(15:26, c(3,4))))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-6
##
a.1 <- list(field1=list(1+51i, 14+64i),
            field2=list(array(c(2+52i, 3+53i, 4+54i, 5+55i, 6+56i, 7+57i,
                8+58i, 9+59i, 10+60i, 11+61i, 12+62i, 13+63i),
                c(3,4)),
                array(c(15+65i, 16+66i, 17+67i, 18+68i, 19+69i, 20+70i,
                        21+71i, 22+72i, 23+73i, 24+74i, 25+75i, 26+76i),
                      c(3,4))))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-7
##
a.1 <- list(field1=list(triu(Matrix(1:20, nrow=4, ncol=5, sparse=TRUE))),
            field2=list(tril(Matrix(1:20, nrow=5, ncol=4, sparse=TRUE, byrow=TRUE))))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-8
##
a.1 <- list(field1=list(array(c(1+21i, 0+0i, 0+0i, 0+0i, 5+25i,
                6+26i, 0+0i, 0+0i, 9+29i, 10+30i, 11+31i, 0+0i,
                13+33i, 14+34i, 15+35i, 16+36i, 17+37i, 18+38i,
                19+39i, 20+40i), c(4,5))),
            field2=list(array(c(1-21i, 5-25i, 9-29i, 13-33i, 17-37i,
                0+0i, 6-26i, 10-30i, 14-34i, 18-38i, 0+0i, 0+0i,
                11-31i, 15-35i, 19-39i, 0+0i, 0+0i, 0+0i,
                16-36i, 20-40i), c(5,4))))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-9
##
a.1 <- list(field1 = c("abcdefghijklmnopqrstuvwxyz",
                "1234567890!@#$%^&*()-_=+`~"), #
            field2 = c("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                "[{]}\\|;:'\",<.>/?          "))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-10 (Structure array with empty fields)
##
a.1 <- list(field1=numeric(0))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-11
##
a.1 <- list(field1=list(1))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-12
##
a.1 <- structure(list(field1 = list(structure(c(FALSE, TRUE,
                          FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                          FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                          FALSE, TRUE, FALSE, TRUE, FALSE,
                          TRUE), .Dim = 4:5),
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

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-13
##
a.1 <- structure(list(X = structure(list(x = list(structure(c(1, 4,
                                             2, 5, 3, 6.2),
                                             .Dim = 2:3)),
                          y = list(list("Hello", "world!")),
                          z = list(structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                              .Dim = c(14L, 14L)))),
                          .Names = c("x", "y", "z"))),
                 .Names = "X")

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, a.1))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, a.1))
}

##
## structure: case-14
##
a.1 <- list(y=c("a", "bb"), z=c(1, 2))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, list(y = list("a", "bb"), z = list(c(1, 2)))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, list(y = list("a", "bb"), z = list(c(1, 2)))))
}

##
## structure: case-15
##
a.1 <- list(y=c("a", "bb"), z=c("c", "dd"))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, list(y = list("a", "bb"),
                           z = list("c", "dd"))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, list(y = list("a", "bb"),
                               z = list("c", "dd"))))
}

##
## structure: case-16
##
a.1 <- list(y=c("a", "b"), z=c(1, 2))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(a = list(y = list("a", "b"),
                               z = list(c(1, 2))))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(a = list(y = list("a", "b"),
                                   z = list(c(1, 2))))))
}

##
## structure: case-17
##
a.1 <- list(y=c("a", "b"), z=c(1, 2, 3))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(a = list(y = list("a", "b"),
                               z = list(c(1, 2, 3))))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(a = list(y = list("a", "b"),
                                   z = list(c(1, 2, 3))))))
}

##
## structure: case-18
##
a.1 <- list(y=c("a", "bb"), z=c(1, 2, 3))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(a = list(y = list("a", "bb"),
                               z = list(c(1, 2, 3))))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(a = list(y = list("a", "bb"),
                                   z = list(c(1, 2, 3))))))
}

##
## structure: case-19
##
a.1 <- list(y=c("a", "bb"), z=c(1, 2))

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(y = list('a', 'bb'),
                           z = c(1, 2))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(y = list('a', 'bb'),
                               z = c(1, 2))))
}

##
## structure: case-20
##
a.1 <- list(y=c("a", "bb"), z=c("c", "dd"))

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(y = list("a", "bb"),
                           z = list("c", "dd"))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(y = list("a", "bb"),
                               z = list("c", "dd"))))
}

##
## structure: case-21
##
a.1 <- list(y=c("a", "b"), z=c(1, 2))

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(y = c("a", "b"),
                           z = c(1, 2))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(y = c("a", "b"),
                               z = c(1, 2))))
}

##
## structure: case-22
##
a.1 <- list(y=c("a", "b"), z=c(1, 2, 3))

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(y = c("a", "b"),
                           z = c(1, 2, 3))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(y = c("a", "b"),
                               z = c(1, 2, 3))))
}

##
## structure: case-23
##
a.1 <- list(y=c("a", "bb"), z=c(1, 2, 3))

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(y = list("a", "bb"),
                           z = c(1, 2, 3))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(y = list("a", "bb"),
                               z = c(1, 2, 3))))
}

##
## structure: case-24
##
a.1 <- list(y=c("a", "bb"), z=list(c('d', 'eee')))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(a = list(y = list("a", "bb"),
                               z = list(list(list("d", "eee")))))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(a = list(y = list("a", "bb"),
                                   z = list(list(list("d", "eee")))))))
}

##
## structure: case-25
##
a.1 <- list(y=c("a", "bb"), z=list(c('d', 'eee')))

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(y = list("a", "bb"),
                           z = list(list("d", "eee")))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(y = list("a", "bb"),
                               z = list(list("d", "eee")))))
}

##
## structure: case-26
##
a.1 <- list(y=c("a", "bb"), z=Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 1, 0,
                                0, 0, 0, 0, 0, 0, 0, 0, 1),
                                nrow=3,
                                ncol=9,
                                byrow=TRUE,
                                sparse=TRUE))

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(y=list("a", "bb"),
                           z=Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 1, 0,
                               0, 0, 0, 0, 0, 0, 0, 0, 1),
                               nrow=3,
                               ncol=9,
                               byrow=TRUE,
                               sparse=TRUE))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(y=list("a", "bb"),
                               z=Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 1, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 1),
                                   nrow=3,
                                   ncol=9,
                                   byrow=TRUE,
                                   sparse=TRUE))))
}

##
## structure: case-27
##
a.1 <- list(y=c("a", "bb"), z=Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 1, 0,
                                0, 0, 0, 0, 0, 0, 0, 0, 1),
                                nrow=3,
                                ncol=9,
                                byrow=TRUE,
                                sparse=TRUE))

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, list(y=list("a", "bb"),
                           z=list(Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 1, 0,
                               0, 0, 0, 0, 0, 0, 0, 0, 1),
                               nrow=3,
                               ncol=9,
                               byrow=TRUE,
                               sparse=TRUE)))))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, list(y=list("a", "bb"),
                               z=list(Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 1, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 1),
                                   nrow=3,
                                   ncol=9,
                                   byrow=TRUE,
                                   sparse=TRUE)))))
}

##
## structure: case-28
##
a.1 <- list(y=list(c("a", "bb")), z=list())

write.mat(list(a=a.1),
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)[['a']]
unlink(filename)
stopifnot(identical(a.2, list(y=list(list(list("a", "bb"))), z=list())))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(list(a=a.1),
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    unlink(filename)
    stopifnot(identical(a.2, list(y=list(list(list("a", "bb"))), z=list())))
}

##
## structure: case-29
##
a.1 <- list(y=list(c("a", "bb")), z=list())

write.mat(a.1,
          filename=filename,
          compression=FALSE,
          version='MAT5')
a.2 <- read.mat(filename)
unlink(filename)
stopifnot(identical(a.2, list(y = list(list("a", "bb")), z = list())))

## Run the same test with compression
if(rmatio:::have.zlib()) {
    write.mat(a.1,
              filename=filename,
              compression=TRUE,
              version='MAT5')
    a.2 <- read.mat(filename)
    unlink(filename)
    stopifnot(identical(a.2, list(y = list(list("a", "bb")), z = list())))
}
