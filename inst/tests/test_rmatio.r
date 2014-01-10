context("Argument checking")

test_that("Filename", {
    expect_error(write.mat(list(a=1:5), filename=NULL),
                 "'filename' must be a character vector of length one")

    expect_error(write.mat(list(a=1:5), filename=5),
                 "'filename' must be a character vector of length one")

    expect_error(write.mat(list(a=1:5), filename=c('a', 'b')),
                 "'filename' must be a character vector of length one")

    expect_error(write.mat(list(a=1:5), filename=''),
                 "'filename' must be a character vector of length one")
})

test_that("Compression", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    expect_error(write.mat(list(a=1:5),
                           filename=filename,
                           compression=NULL),
                 "'compression' must be a logical vector of length one")

    expect_error(write.mat(list(a=1:5),
                           filename=filename,
                           compression=5),
                 "'compression' must be a logical vector of length one")

    expect_error(write.mat(list(a=1:5),
                           filename=filename,
                           compression=c(TRUE, TRUE)),
                 "'compression' must be a logical vector of length one")

    expect_error(write.mat(list(a=1:5),
                           filename=filename,
                           compression=logical(0)),
                 "'compression' must be a logical vector of length one")
})

test_that("Names", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    expect_error(write.mat(list(1:5),
                           filename=filename,
                           compression=FALSE),
                 "All values in the list must have a unique name")

    expect_error(write.mat(list(a=1:5, 6:10),
                           filename=filename,
                           compression=FALSE),
                 "All values in the list must have a unique name")

    expect_error(write.mat(list(a=1:5, a=6:10),
                           filename=filename,
                           compression=FALSE),
                 "All values in the list must have a unique name")
})

context("dgCMatrix")

test_that("dgCMatrix: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 1, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 1),
                  nrow=3,
                  ncol=9,
                  byrow=TRUE,
                  sparse=TRUE)

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("dgCMatrix: case-2", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- as(diag(1:5), 'dgCMatrix')

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

context("vector")

test_that("vector: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    write.mat(list(a=1:5),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a <- read.mat(filename)[['a']]
    storage.mode(a) <- 'integer'
    expect_identical(a, 1:5)

    if(have.zlib()) {
        write.mat(list(a=1:5),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a <- read.mat(filename)[['a']]
        storage.mode(a) <- 'integer'
        expect_identical(a, 1:5)
    }
})

test_that("vector: case-2", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    write.mat(list(a=c(1,2,3,4,5)),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a <- read.mat(filename)[['a']]
    expect_identical(a, c(1,2,3,4,5))

    if(have.zlib()) {
        write.mat(list(a=c(1,2,3,4,5)),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a <- read.mat(filename)[['a']]
        expect_identical(a, c(1,2,3,4,5))
    }
})

test_that("vector: case-3", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    write.mat(list(a=1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a <- read.mat(filename)[['a']]
    expect_identical(a, 1)

    if(have.zlib()) {
        write.mat(list(a=1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a <- read.mat(filename)[['a']]
        expect_identical(a, 1)
    }
})

context("matrix")

test_that("matrix: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    write.mat(list(a=matrix(1:9, nrow=3)),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a <- read.mat(filename)[['a']]
    storage.mode(a) <- 'integer'
    expect_identical(a, matrix(1:9, nrow=3))

    if(have.zlib()) {
        write.mat(list(a=matrix(1:9, nrow=3)),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a <- read.mat(filename)[['a']]
        storage.mode(a) <- 'integer'
        expect_identical(a, matrix(1:9, nrow=3))
    }
})

test_that("matrix: case-2", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    write.mat(list(a=matrix(c(1,2,3,4,5,6,7,8,9), nrow=3)),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a <- read.mat(filename)[['a']]
    expect_identical(a, matrix(c(1,2,3,4,5,6,7,8,9), nrow=3))

    if(have.zlib()) {
        write.mat(list(a=matrix(c(1,2,3,4,5,6,7,8,9), nrow=3)),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a <- read.mat(filename)[['a']]
        expect_identical(a, matrix(c(1,2,3,4,5,6,7,8,9), nrow=3))
    }
})

context("array")

test_that("array: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(seq_len(32^3), c(32,32,32))
    storage.mode(a.1) <- 'integer'

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("array: case-2", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(seq_len(32^3), c(32,32,32))
    storage.mode(a.1) <- 'double'

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("array: case-3", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
                   TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE,
                   FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                   FALSE, FALSE, TRUE), c(5L, 5L))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("array: case-4", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(seq_len(32^3), c(32,32,32));
    storage.mode(a.1) <- 'double'

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})


test_that("array: case-5", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(seq_len(32^3), c(32,32,32));
    storage.mode(a.1) <- 'integer'

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})


test_that("array: case-6", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(c(seq_len(32767), 32767), c(32,32,32));
    storage.mode(a.1) <- 'integer'

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("array: case-7", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(complex(real=seq(1, 2*32^3, 2), imaginary=seq(2, 2*32^3, 2)), c(32,32,32))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

context("complex")

test_that("complex: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(complex(real=1:20,imaginary=21:40), c(4,5))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

context("string")

test_that("string: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- c("abcdefghijklmnopqrstuvwxyz",
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
             "1234567890!@#$%^&*()-_=+`~", #
             "[{]}\\|;:'\",<.>/?          ")

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("string: case-2", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- c("a", "bb")

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list("a", "bb"))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list("a", "bb"))
    }
})

test_that("string: case-3", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list(y=list('a', 'bb')))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(y=list('a', 'bb')))
    }
})

test_that("string: case-4", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(c("a", "bb"))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list(list("a", "bb")))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(list("a", "bb")))
    }
})

context("logical")

test_that("logical: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- array(c(TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
                   FALSE, TRUE,  TRUE,  TRUE,  TRUE,
                   FALSE, FALSE, TRUE,  TRUE,  TRUE,
                   FALSE, FALSE, FALSE, TRUE,  TRUE,
                   FALSE, FALSE, FALSE, FALSE, TRUE),
                 c(5L, 5L))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("logical: case-2", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- new("lgCMatrix",
               i = c(6L, 0L, 4L, 5L, 0L, 2L, 4L, 1L, 4L, 6L, 7L, 3L, 4L),
               p = c(0L, 1L, 4L, 7L, 11L, 13L),
               Dim = c(10L, 5L),
               Dimnames = list(NULL, NULL),
               x = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                   TRUE, TRUE, TRUE),
               factors = list())

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

context("struct")

test_that("struct: case-1 (Empty structure array)", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- structure(list(), .Names = character(0))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-2 (Empty structure array with fields)", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(field1=list(), field2=list())

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-3 (Structure array with empty fields)", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

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
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-4", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(field1=list(1, 14),
                field2=list(array(as.numeric(2:13), c(3,4)),
                    array(as.numeric(15:26), c(3,4))))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-5", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(field1=list(1L, 14L),
                field2=list(array(2:13, c(3,4)),
                    array(15:26, c(3,4))))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-6", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

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
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-7", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(field1=list(triu(Matrix(1:20, nrow=4, ncol=5, sparse=TRUE))),
                field2=list(tril(Matrix(1:20, nrow=5, ncol=4, sparse=TRUE, byrow=TRUE))))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-8", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

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
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-9", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(field1 = c("abcdefghijklmnopqrstuvwxyz",
                    "1234567890!@#$%^&*()-_=+`~"), #
                field2 = c("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                    "[{]}\\|;:'\",<.>/?          "))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-10 (Structure array with empty fields)", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(field1=numeric(0))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-11", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(field1=list(1))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-12", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

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
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-13", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

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
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, a.1)
    }
})

test_that("struct: case-14", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"), z=c(1, 2))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list(y = list("a", "bb"), z = list(c(1, 2))))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(y = list("a", "bb"), z = list(c(1, 2))))
    }
})

test_that("struct: case-15", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"), z=c("c", "dd"))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list(y = list("a", "bb"),
                               z = list("c", "dd")))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(y = list("a", "bb"),
                                   z = list("c", "dd")))
    }
})

test_that("struct: case-16", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "b"), z=c(1, 2))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(a = list(y = list("a", "b"),
                                   z = list(c(1, 2)))))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(a = list(y = list("a", "b"),
                                       z = list(c(1, 2)))))
    }
})

test_that("struct: case-17", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "b"), z=c(1, 2, 3))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(a = list(y = list("a", "b"),
                                   z = list(c(1, 2, 3)))))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(a = list(y = list("a", "b"),
                                       z = list(c(1, 2, 3)))))
    }
})

test_that("struct: case-18", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"), z=c(1, 2, 3))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(a = list(y = list("a", "bb"),
                                   z = list(c(1, 2, 3)))))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(a = list(y = list("a", "bb"),
                                       z = list(c(1, 2, 3)))))
    }
})

test_that("struct: case-19", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"), z=c(1, 2))

    write.mat(a.1,
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(y = list('a', 'bb'),
                               z = c(1, 2)))

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(y = list('a', 'bb'),
                                   z = c(1, 2)))
    }
})

test_that("struct: case-20", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"), z=c("c", "dd"))

    write.mat(a.1,
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(y = list("a", "bb"),
                               z = list("c", "dd")))

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(y = list("a", "bb"),
                                   z = list("c", "dd")))
    }
})

test_that("struct: case-21", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "b"), z=c(1, 2))

    write.mat(a.1,
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(y = c("a", "b"),
                               z = c(1, 2)))

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(y = c("a", "b"),
                                   z = c(1, 2)))
    }
})

test_that("struct: case-22", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "b"), z=c(1, 2, 3))

    write.mat(a.1,
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(y = c("a", "b"),
                               z = c(1, 2, 3)))

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(y = c("a", "b"),
                                   z = c(1, 2, 3)))
    }
})

test_that("struct: case-23", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"), z=c(1, 2, 3))

    write.mat(a.1,
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(y = list("a", "bb"),
                               z = c(1, 2, 3)))

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(y = list("a", "bb"),
                                   z = c(1, 2, 3)))
    }
})

test_that("struct: case-24", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"), z=list(c('d', 'eee')))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(a = list(y = list("a", "bb"),
                                   z = list(list(list("d", "eee"))))))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(a = list(y = list("a", "bb"),
                                       z = list(list(list("d", "eee"))))))
    }
})

test_that("struct: case-25", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=c("a", "bb"), z=list(c('d', 'eee')))

    write.mat(a.1,
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(y = list("a", "bb"),
                               z = list(list("d", "eee"))))

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(y = list("a", "bb"),
                                   z = list(list("d", "eee"))))
    }
})

test_that("struct: case-26", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

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
    expect_identical(a.2, list(y=list("a", "bb"),
                               z=Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 1, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 1),
                                   nrow=3,
                                   ncol=9,
                                   byrow=TRUE,
                                   sparse=TRUE)))

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(y=list("a", "bb"),
                                   z=Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 1, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 1),
                                       nrow=3,
                                       ncol=9,
                                       byrow=TRUE,
                                       sparse=TRUE)))
    }
})

test_that("struct: case-27", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

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
    expect_identical(a.2, list(y=list("a", "bb"),
                               z=list(Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 1, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 1),
                                   nrow=3,
                                   ncol=9,
                                   byrow=TRUE,
                                   sparse=TRUE))))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(y=list("a", "bb"),
                                   z=list(Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 1, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 1),
                                       nrow=3,
                                       ncol=9,
                                       byrow=TRUE,
                                       sparse=TRUE))))
    }
})

test_that("struct: case-28", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=list(c("a", "bb")), z=list())

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list(y=list(list(list("a", "bb"))), z=list()))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(y=list(list(list("a", "bb"))), z=list()))
    }
})

test_that("struct: case-29", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(y=list(c("a", "bb")), z=list())

    write.mat(a.1,
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)
    expect_identical(a.2, list(y = list(list("a", "bb")), z = list()))

    if(have.zlib()) {
        write.mat(a.1,
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)
        expect_identical(a.2, list(y = list(list("a", "bb")), z = list()))
    }
})

context("cell")

test_that("cell: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list()

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-2", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(complex(0),
                logical(0),
                character(0),
                numeric(0),
                integer(0))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-3", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(list(array(c(1, 3, 2, 4), c(2, 2)),
                     array(c(5, 8, 6, 9, 7, 10), c(2,3)),
                     array(c(11, 15, 12, 16, 13, 17, 14, 18), c(2, 4))),
                list(array(c(19, 21, 20, 22), c(2, 2)),
                     array(c(23, 25, 27, 24, 26, 28), c(3L, 2L)),
                     array(c(29, 31, 33, 35, 30, 32, 34, 36), c(4, 2))))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-4", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(list(array(c(1L, 3L, 2L, 4L), c(2, 2)),
                     array(c(5L, 8L, 6L, 9L, 7L, 10L), c(2,3)),
                     array(c(11L, 15L, 12L, 16L, 13L, 17L, 14L, 18L), c(2, 4))),
                list(array(c(19L, 21L, 20L, 22L), c(2, 2)),
                     array(c(23L, 25L, 27L, 24L, 26L, 28L), c(3L, 2L)),
                     array(c(29L, 31L, 33L, 35L, 30L, 32L, 34L, 36L), c(4, 2))))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-5", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(list(triu(Matrix(1:20, nrow=4, ncol=5, sparse=TRUE)),
                     tril(Matrix(1:20, nrow=5, ncol=4, sparse=TRUE, byrow=TRUE))))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-6", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(array(c(1+21i, 0+0i, 0+0i, 0+0i, 5+25i,
                        6+26i, 0+0i, 0+0i, 9+29i, 10+30i, 11+31i, 0+0i,
                        13+33i, 14+34i, 15+35i, 16+36i, 17+37i, 18+38i,
                        19+39i, 20+40i), c(4,5)),
                array(c(1-21i, 5-25i, 9-29i, 13-33i, 17-37i,
                        0+0i, 6-26i, 10-30i, 14-34i, 18-38i, 0+0i, 0+0i,
                        11-31i, 15-35i, 19-39i, 0+0i, 0+0i, 0+0i,
                        16-36i, 20-40i), c(5,4)))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-7", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(list("abcdefghijklmnopqrstuvwxyz",
                     "1234567890!@#$%^&*()-_=+`~"), #
                list("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                     "[{]}\\|;:'\",<.>/?          "))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-8", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(structure(list(),
                          .Names = character(0)),
                list(),
                structure(list(field1 = numeric(0),
                               field2 = character(0)),
                          .Names = c("field1", "field2")))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})


test_that("cell: case-9", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- list(list(structure(list(field1 = list(1, 14), field2 =
                  list( structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                  12, 13), .Dim = 3:4), structure(c(15, 16, 17, 18,
                  19, 20, 21, 22, 23, 24, 25, 26 ), .Dim = 3:4))),
                  .Names = c("field1", "field2")), structure(list(
                  field1 = list(1, 14), field2 = list(structure(c(2,
                  3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), .Dim = 3:4),
                  structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                  25, 26), .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list(field1 = list(1, 14),
                  field2 = list( structure(c(2, 3, 4, 5, 6, 7, 8, 9,
                  10, 11, 12, 13), .Dim = 3:4), structure(c(15, 16,
                  17, 18, 19, 20, 21, 22, 23, 24, 25, 26 ), .Dim =
                  3:4))), .Names = c("field1", "field2")),
                  structure(list( field1 = list(1, 14), field2 =
                  list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                  13), .Dim = 3:4), structure(c(15, 16, 17, 18, 19,
                  20, 21, 22, 23, 24, 25, 26), .Dim = 3:4))), .Names =
                  c("field1", "field2")), structure(list(field1 =
                  list(1L, 14L), field2 = list( structure(2:13, .Dim =
                  3:4), structure(15:26, .Dim = 3:4))), .Names =
                  c("field1", "field2")), structure(list(field1 =
                  list(1, 14), field2 = list( structure(c(2, 3, 4, 5,
                  6, 7, 8, 9, 10, 11, 12, 13), .Dim = 3:4),
                  structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                  25, 26 ), .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list( field1 = list(1L, 14L),
                  field2 = list(structure(2:13, .Dim = 3:4),
                  structure(15:26, .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list(field1 = list(1L, 14L),
                  field2 = list( structure(2:13, .Dim = 3:4),
                  structure(15:26, .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list(field1 = list(1L, 14L),
                  field2 = list( structure(2:13, .Dim = 3:4),
                  structure(15:26, .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list(field1 = list(1L, 14L),
                  field2 = list( structure(2:13, .Dim = 3:4),
                  structure(15:26, .Dim = 3:4))), .Names = c("field1",
                  "field2"))), list(structure(list(field1 =
                  list(1+51i, 14+64i), field2 =
                  list(structure(c(2+52i, 3+53i, 4+54i, 5+55i, 6+56i,
                  7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2"))))

  write.mat(list(a=a.1),
            filename=filename,
            compression=FALSE,
            version='MAT5')
  a.2 <- read.mat(filename)[['a']]
  expect_identical(a.2, a.1)

  if(have.zlib()) {
      write.mat(list(a=a.1),
                filename=filename,
                compression=TRUE,
                version='MAT5')
      a.2 <- read.mat(filename)[['a']]
      expect_identical(a.2, a.1)
  }
})

test_that("cell: case-10", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(list(field1=list(triu(Matrix(1:20, nrow=4,
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

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-11", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(list(field1 = "abcdefghijklmnopqrstuvwxyz",
                     field2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
                list(field1 = "1234567890!@#$%^&*()-_=+`~", #
                     field2 = "[{]}\\|;:'\",<.>/?          "))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-12", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(structure(c(FALSE, TRUE, FALSE, TRUE, FALSE,
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

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-13", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(structure(list(),
                          .Names = character(0)),
                list(field1=list(), field2=list()),
                structure(list(field1 = numeric(0),
                               field2 = character(0)),
                          .Names = c("field1", "field2")))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, a.1)

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})

test_that("cell: case-14", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(c("a", "bb"), c("c", "dd"))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list(list("a", "bb"), list("c", "dd")))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(list("a", "bb"), list("c", "dd")))
    }
})

test_that("cell: case-15", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(c("a", "bb"), list(c('d', 'eee')))

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list(list("a", "bb"),
                               list(list(list("d", "eee")))))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(list("a", "bb"),
                                   list(list(list("d", "eee")))))
    }
})

test_that("cell: case-16", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(c("a", "bb"), Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
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
    expect_identical(a.2, list(list("a", "bb"),
                               list(Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                             0, 0, 0, 0, 0, 0, 0, 1, 0,
                                             0, 0, 0, 0, 0, 0, 0, 0, 1),
                                           nrow=3,
                                           ncol=9,
                                           byrow=TRUE,
                                           sparse=TRUE))))
    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(list("a", "bb"),
                                   list(Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                                                 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                                 0, 0, 0, 0, 0, 0, 0, 0, 1),
                                               nrow=3,
                                               ncol=9,
                                               byrow=TRUE,
                                               sparse=TRUE))))
    }
})

test_that("cell: case-17", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))

    a.1 <- list(list(c("a", "bb")), list())

    write.mat(list(a=a.1),
              filename=filename,
              compression=FALSE,
              version='MAT5')
    a.2 <- read.mat(filename)[['a']]
    expect_identical(a.2, list(list(list(list("a", "bb"))), list()))

    if(have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, list(list(list(list("a", "bb"))), list()))
    }
})

context("Read: small_v4_le.mat")

test_that("matio_test_cases_compressed_le.mat", {
  expect_identical(read.mat('../extdata/small_v4_le.mat')$x, pi)
})

context("Write MAT5 uncompressed: small_v4_le.mat")

test_that("matio_test_cases_compressed_le.mat", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(read.mat('../extdata/small_v4_le.mat'),
            filename=filename,
            compression=FALSE,
            version='MAT5')
  expect_identical(read.mat(filename)$x, pi)
})

context("Write MAT5 compressed: small_v4_le.mat")

test_that("matio_test_cases_compressed_le.mat", {
    if(have.zlib()) {
        filename <- tempfile(fileext = ".mat")
        on.exit(unlink(filename))

        write.mat(read.mat('../extdata/small_v4_le.mat'),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        expect_identical(read.mat(filename)$x, pi)
    }
})

context("Read: small_v4_be.mat")

test_that("small_v4_be.mat", {
  expect_identical(read.mat('../extdata/small_v4_be.mat')$x, pi)
})

context("Write MAT5 uncompressed: small_v4_be.mat")

test_that("small_v4_be.mat", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(read.mat('../extdata/small_v4_be.mat'),
            filename=filename,
            compression=FALSE,
            version='MAT5')
  expect_identical(read.mat(filename)$x, pi)
})

context("Write MAT5 compressed: small_v4_be.mat")

test_that("small_v4_be.mat", {
    if(have.zlib()) {
        filename <- tempfile(fileext = ".mat")
        on.exit(unlink(filename))

        write.mat(read.mat('../extdata/small_v4_be.mat'),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        expect_identical(read.mat(filename)$x, pi)
    }
})

test_mat_v4_file <- function(filename) {
    m <- read.mat(filename)

    ## var1 read as double
    var1 <- array(seq_len(20), c(4,5))
    storage.mode(var1) <- 'double'
    expect_identical(m$var1, var1)

    ## var11 read as complex
    var11 <- array(c(1+21i, 2+22i, 3+23i, 4+24i, 5+25i, 6+26i, 7+27i,
                     8+28i, 9+29i, 10+30i, 11+31i, 12+32i, 13+33i,
                     14+34i, 15+35i, 16+36i, 17+37i, 18+38i, 19+39i,
                     20+40i), c(4,5))
    expect_identical(m$var11, var11)

    ## var21 read as a sparse matrix
    var21 <- as(diag(1:5), 'dgCMatrix')
    expect_identical(m$var21, var21)

    ## var22 read as a complex matrix
    var22 <- structure(c(1+6i, 0+0i, 0+0i, 0+0i, 0+0i,
                         0+0i, 2+7i, 0+0i, 0+0i, 0+0i,
                         0+0i, 0+0i, 3+8i, 0+0i, 0+0i,
                         0+0i, 0+0i, 0+0i, 4+9i, 0+0i,
                         0+0i, 0+0i, 0+0i, 0+0i, 5+10i),
                       .Dim = c(5L, 5L))
    expect_identical(m$var22, var22)

    ## var24 read as character vector
    expect_identical(m$var24, c("abcdefghijklmnopqrstuvwxyz",
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                "1234567890!@#$%^&*()-_=+`~",
                                "[{]}\\|;:'\",<.>/?          "))
}

context("Read: matio_test_cases_v4_le.mat")

test_that("matio_test_cases_uncompressed_le.mat", {
    test_mat_v4_file('../extdata/matio_test_cases_v4_le.mat')
})

context("Write MAT5 uncompressed: matio_test_cases_v4_le.mat")

test_that("matio_test_cases_uncompressed_le.mat", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(read.mat('../extdata/matio_test_cases_v4_le.mat'),
            filename=filename,
            compression=FALSE,
            version='MAT5')
  test_mat_v4_file(filename)
})

context("Write MAT5 compressed: matio_test_cases_v4_le.mat")

test_that("matio_test_cases_uncompressed_le.mat", {
    if(have.zlib()) {
        filename <- tempfile(fileext = ".mat")
        on.exit(unlink(filename))

        write.mat(read.mat('../extdata/matio_test_cases_v4_le.mat'),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        test_mat_v4_file(filename)
    }
})

context("Read: matio_test_cases_v4_be.mat")

test_that("matio_test_cases_uncompressed_be.mat", {
    test_mat_v4_file('../extdata/matio_test_cases_v4_be.mat')
})

context("Write MAT5 uncompressed: matio_test_cases_v4_be.mat")

test_that("matio_test_cases_uncompressed_be.mat", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(read.mat('../extdata/matio_test_cases_v4_be.mat'),
            filename=filename,
            compression=FALSE,
            version='MAT5')
  test_mat_v4_file(filename)
})

context("Write MAT5 compressed: matio_test_cases_v4_be.mat")

test_that("matio_test_cases_uncompressed_be.mat", {
    if(have.zlib()) {
        filename <- tempfile(fileext = ".mat")
        on.exit(unlink(filename))

        write.mat(read.mat('../extdata/matio_test_cases_v4_be.mat'),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        test_mat_v4_file(filename)
    }
})

test_mat_v5_file <- function(filename) {
    m <- read.mat(filename)

    ## var1, ..., var4 read as double
    var1 <- array(seq_len(20), c(4,5))
    storage.mode(var1) <- 'double'
    expect_identical(m$var1, var1)
    expect_identical(m$var2, var1)
    expect_identical(m$var3, var1)
    expect_identical(m$var4, var1)

    ## var5 read as integer
    var5 <- array(seq_len(20), c(4,5))
    storage.mode(var5) <- 'integer'
    expect_identical(m$var5, var5)

    ## var6 read as double
    expect_identical(m$var6, var1)

    ## var7, ..., var10 read as integer
    expect_identical(m$var7, var5)
    expect_identical(m$var8, var5)
    expect_identical(m$var9, var5)
    expect_identical(m$var10, var5)

    ## var11, ..., var20 read as complex
    var11 <- array(c(1+21i, 2+22i, 3+23i, 4+24i, 5+25i, 6+26i, 7+27i,
                     8+28i, 9+29i, 10+30i, 11+31i, 12+32i, 13+33i,
                     14+34i, 15+35i, 16+36i, 17+37i, 18+38i, 19+39i,
                     20+40i), c(4,5))
    expect_identical(m$var11, var11)
    expect_identical(m$var12, var11)
    expect_identical(m$var13, var11)
    expect_identical(m$var14, var11)
    expect_identical(m$var15, var11)
    expect_identical(m$var16, var11)
    expect_identical(m$var17, var11)
    expect_identical(m$var18, var11)
    expect_identical(m$var19, var11)
    expect_identical(m$var20, var11)

    ## var21 read as a sparse matrix
    var21 <- as(diag(1:5), 'dgCMatrix')
    expect_identical(m$var21, var21)

    ## var22 read as a dense complex matrix
    var22 <- array(c(1+6i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 2+7i, 0+0i,
    0+0i, 0+0i, 0+0i, 0+0i, 3+8i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 4+9i,
    0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 5+10i), c(5L, 5L))
    expect_identical(m$var22, var22)

    ## var23 read as double
    expect_identical(m$var23, numeric(0))

    ## var24 read as character vector
    expect_identical(m$var24, c("abcdefghijklmnopqrstuvwxyz",
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                "1234567890!@#$%^&*()-_=+`~",
                                "[{]}\\|;:'\",<.>/?          "))

    ## Structure Variables
    ## var25 read as an empty named list
    var25 <- structure(list(), .Names = character(0))
    expect_identical(m$var25, var25)

    var26 <- list(field1=list(), field2=list())
    expect_identical(m$var26, var26)

    var27 <- list(field1=numeric(0), field2=character(0))
    expect_identical(m$var27, var27)

    ## var28, ..., var31 read as double
    var28 <- list(field1=list(1, 14),
                  field2=list(array(as.numeric(2:13), c(3,4)),
                      array(as.numeric(15:26), c(3,4))))

    expect_identical(m$var28, var28)
    expect_identical(m$var29, var28)
    expect_identical(m$var30, var28)
    expect_identical(m$var31, var28)

    ## var32 read as integer
    var32 <- list(field1=list(1L, 14L),
                  field2=list(array(2:13, c(3,4)),
                      array(15:26, c(3,4))))

    expect_identical(m$var32, var32)

    ## var33 read as double
    expect_identical(m$var33, var28)

    ## var34, ..., var37 read as integer
    expect_identical(m$var34, var32)
    expect_identical(m$var35, var32)
    expect_identical(m$var36, var32)
    expect_identical(m$var37, var32)

    ## var38, ..., var47 read as complex
    var38 <- list(field1=list(1+51i, 14+64i),
                  field2=list(array(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), c(3,4)), array(c(15+65i, 16+66i, 17+67i,
                  18+68i, 19+69i, 20+70i, 21+71i, 22+72i, 23+73i,
                  24+74i, 25+75i, 26+76i), c(3,4))))

    expect_identical(m$var38, var38)
    expect_identical(m$var39, var38)
    expect_identical(m$var40, var38)
    expect_identical(m$var41, var38)
    expect_identical(m$var42, var38)
    expect_identical(m$var43, var38)
    expect_identical(m$var44, var38)
    expect_identical(m$var45, var38)
    expect_identical(m$var46, var38)
    expect_identical(m$var47, var38)

    var48 <- list(field1=list(triu(Matrix(1:20, nrow=4, ncol=5, sparse=TRUE))),
                  field2=list(tril(Matrix(1:20, nrow=5, ncol=4, sparse=TRUE, byrow=TRUE))))
    expect_identical(m$var48, var48)

    var49 <- list(field1=list(array(c(1+21i, 0+0i, 0+0i, 0+0i, 5+25i,
                      6+26i, 0+0i, 0+0i, 9+29i, 10+30i, 11+31i, 0+0i,
                      13+33i, 14+34i, 15+35i, 16+36i, 17+37i, 18+38i,
                      19+39i, 20+40i), c(4,5))),
                  field2=list(array(c(1-21i, 5-25i, 9-29i, 13-33i, 17-37i,
                      0+0i, 6-26i, 10-30i, 14-34i, 18-38i, 0+0i, 0+0i,
                      11-31i, 15-35i, 19-39i, 0+0i, 0+0i, 0+0i,
                      16-36i, 20-40i), c(5,4))))

    expect_identical(m$var49, var49)

    var50 <- list(field1 = c("abcdefghijklmnopqrstuvwxyz",
                      "1234567890!@#$%^&*()-_=+`~"),
                  field2 = c("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                      "[{]}\\|;:'\",<.>/?          "))
    expect_identical(m$var50, var50)

    ## Cell-Array Variables
    var51 <- list()
    expect_identical(m$var51, var51)

    var52 <- list(numeric(0), numeric(0), integer(0), integer(0), integer(0),
                  integer(0), integer(0), integer(0), integer(0), integer(0))
    expect_identical(m$var52, var52)

    ## var53, ..., var56 read as double
    var53 <- list(list(array(c(1, 3, 2, 4), c(2, 2)),
                       array(c(5, 8, 6, 9, 7, 10), c(2,3)),
                       array(c(11, 15, 12, 16, 13, 17, 14, 18), c(2, 4))),
                  list(array(c(19, 21, 20, 22), c(2, 2)),
                       array(c(23, 25, 27, 24, 26, 28), c(3L, 2L)),
                       array(c(29, 31, 33, 35, 30, 32, 34, 36), c(4, 2))))
    expect_identical(m$var53, var53)
    expect_identical(m$var54, var53)
    expect_identical(m$var55, var53)
    expect_identical(m$var56, var53)

    ## var57 read as integer
    var57 <- list(list(array(c(1L, 3L, 2L, 4L), c(2, 2)),
                       array(c(5L, 8L, 6L, 9L, 7L, 10L), c(2,3)),
                       array(c(11L, 15L, 12L, 16L, 13L, 17L, 14L, 18L), c(2, 4))),
                  list(array(c(19L, 21L, 20L, 22L), c(2, 2)),
                       array(c(23L, 25L, 27L, 24L, 26L, 28L), c(3L, 2L)),
                       array(c(29L, 31L, 33L, 35L, 30L, 32L, 34L, 36L), c(4, 2))))
    expect_identical(m$var57, var57)

    ## var58 read as double
    expect_identical(m$var58, var53)

    ## var59, ..., var62 read as integer
    expect_identical(m$var59, var57)
    expect_identical(m$var60, var57)
    expect_identical(m$var61, var57)
    expect_identical(m$var62, var57)

    var63 <- list(list(triu(Matrix(1:20, nrow=4, ncol=5, sparse=TRUE)),
                       tril(Matrix(1:20, nrow=5, ncol=4, sparse=TRUE, byrow=TRUE))))
    expect_identical(m$var63, var63)

    var64 <- list(array(c(1+21i, 0+0i, 0+0i, 0+0i, 5+25i,
                          6+26i, 0+0i, 0+0i, 9+29i, 10+30i, 11+31i, 0+0i,
                          13+33i, 14+34i, 15+35i, 16+36i, 17+37i, 18+38i,
                          19+39i, 20+40i), c(4,5)),
                  array(c(1-21i, 5-25i, 9-29i, 13-33i, 17-37i,
                          0+0i, 6-26i, 10-30i, 14-34i, 18-38i, 0+0i, 0+0i,
                          11-31i, 15-35i, 19-39i, 0+0i, 0+0i, 0+0i,
                          16-36i, 20-40i), c(5,4)))
    expect_identical(m$var64, var64)

    var65 <- list(list("abcdefghijklmnopqrstuvwxyz",
                       "1234567890!@#$%^&*()-_=+`~"),
                  list("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                       "[{]}\\|;:'\",<.>/?          "))
    expect_identical(m$var65, var65)

    var66 <- list(structure(list(),
                            .Names = character(0)),
                  list(field1=list(), field2=list()),
                  structure(list(field1 = numeric(0),
                                 field2 = character(0)),
                            .Names = c("field1", "field2")))
    expect_identical(m$var66, var66)

    var67 <- list(list(structure(list(field1 = list(1, 14), field2 =
                  list( structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                  12, 13), .Dim = 3:4), structure(c(15, 16, 17, 18,
                  19, 20, 21, 22, 23, 24, 25, 26 ), .Dim = 3:4))),
                  .Names = c("field1", "field2")), structure(list(
                  field1 = list(1, 14), field2 = list(structure(c(2,
                  3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), .Dim = 3:4),
                  structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                  25, 26), .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list(field1 = list(1, 14),
                  field2 = list( structure(c(2, 3, 4, 5, 6, 7, 8, 9,
                  10, 11, 12, 13), .Dim = 3:4), structure(c(15, 16,
                  17, 18, 19, 20, 21, 22, 23, 24, 25, 26 ), .Dim =
                  3:4))), .Names = c("field1", "field2")),
                  structure(list( field1 = list(1, 14), field2 =
                  list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                  13), .Dim = 3:4), structure(c(15, 16, 17, 18, 19,
                  20, 21, 22, 23, 24, 25, 26), .Dim = 3:4))), .Names =
                  c("field1", "field2")), structure(list(field1 =
                  list(1L, 14L), field2 = list( structure(2:13, .Dim =
                  3:4), structure(15:26, .Dim = 3:4))), .Names =
                  c("field1", "field2")), structure(list(field1 =
                  list(1, 14), field2 = list( structure(c(2, 3, 4, 5,
                  6, 7, 8, 9, 10, 11, 12, 13), .Dim = 3:4),
                  structure(c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                  25, 26 ), .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list( field1 = list(1L, 14L),
                  field2 = list(structure(2:13, .Dim = 3:4),
                  structure(15:26, .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list(field1 = list(1L, 14L),
                  field2 = list( structure(2:13, .Dim = 3:4),
                  structure(15:26, .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list(field1 = list(1L, 14L),
                  field2 = list( structure(2:13, .Dim = 3:4),
                  structure(15:26, .Dim = 3:4))), .Names = c("field1",
                  "field2")), structure(list(field1 = list(1L, 14L),
                  field2 = list( structure(2:13, .Dim = 3:4),
                  structure(15:26, .Dim = 3:4))), .Names = c("field1",
                  "field2"))), list(structure(list(field1 =
                  list(1+51i, 14+64i), field2 =
                  list(structure(c(2+52i, 3+53i, 4+54i, 5+55i, 6+56i,
                  7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2")),
                  structure(list(field1 = list(1+51i, 14+64i), field2
                  = list( structure(c(2+52i, 3+53i, 4+54i, 5+55i,
                  6+56i, 7+57i, 8+58i, 9+59i, 10+60i, 11+61i, 12+62i,
                  13+63i), .Dim = 3:4), structure(c(15+65i, 16+66i,
                  17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
                  23+73i, 24+74i, 25+75i, 26+76i), .Dim = 3:4))),
                  .Names = c("field1", "field2"))))
    expect_identical(m$var67, var67)

    var68 <- list(list(field1=list(triu(Matrix(1:20, nrow=4, ncol=5, sparse=TRUE))),
                       field2=list(tril(Matrix(1:20, nrow=5, ncol=4, sparse=TRUE, byrow=TRUE)))),
                  list(field1=list(array(c(1+21i, 0+0i, 0+0i, 0+0i, 5+25i, 6+26i, 0+0i, 0+0i, 9+29i,
                                           10+30i, 11+31i, 0+0i, 13+33i, 14+34i, 15+35i, 16+36i,
                                           17+37i, 18+38i, 19+39i, 20+40i), c(4,5))),
                       field2=list(array(c(1-21i, 5-25i, 9-29i, 13-33i, 17-37i, 0+0i, 6-26i, 10-30i,
                                14-34i, 18-38i, 0+0i, 0+0i, 11-31i, 15-35i, 19-39i, 0+0i, 0+0i, 0+0i,
                                16-36i, 20-40i), c(5,4)))))
    expect_identical(m$var68, var68)

    var69 <- list(list(field1 = "abcdefghijklmnopqrstuvwxyz",
                       field2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
                  list(field1 = "1234567890!@#$%^&*()-_=+`~",
                       field2 = "[{]}\\|;:'\",<.>/?          "))
    expect_identical(m$var69, var69)

    ## var70, ..., var73 read as double
    var70 = array(seq_len(32^3), c(32,32,32));
    storage.mode(var70) <- 'double'
    expect_identical(m$var70, var70)
    expect_identical(m$var71, var70)
    expect_identical(m$var72, var70)
    expect_identical(m$var73, var70)

    ## var74 read as integer
    var74 = array(seq_len(32^3), c(32,32,32));
    storage.mode(var74) <- 'integer'
    expect_identical(m$var74, var74)

    ## var75 read as double
    expect_identical(m$var75, var70)

    ## var76 read as integer
    ## var76 = reshape(int16(1:32*32*32),32,32,32);
    var76 = array(c(seq_len(32767), 32767), c(32,32,32));
    storage.mode(var76) <- 'integer'
    expect_identical(m$var76, var76)

    ## var77 read as integer
    ## var77 = reshape(uint16(1:32*32*32),32,32,32);
    expect_identical(m$var77, var74)

    ## var78 read as integer
    var78_slab <- array(c(1L, 37L, -55L, -70L, -48L, -84L, -96L, -93L,
    -91L, -24L, -123L, -92L, 39L, 109L, -69L, 68L, 76L, -42L, -4L,
    36L, 45L, -89L, -60L, -19L, 99L, 85L, 76L, 109L, 96L, -60L, 24L,
    112L, 74L, -52L, -57L, 2L, 106L, -34L, -77L, 92L, 30L, -126L,
    -55L, 3L, 95L, -75L, -77L, -83L, -112L, 69L, -57L, -97L, 36L, 37L,
    90L, -76L, 117L, 12L, -108L, 51L, -35L, -105L, -1L, 99L, -103L,
    55L, -77L, 27L, 59L, -38L, -96L, 32L, -52L, -32L, 123L, 63L, 122L,
    59L, -19L, 84L, -99L, -27L, 121L, 25L, -22L, 8L, -70L, 9L, 40L,
    27L, 90L, 122L, 124L, 7L, -2L, -84L, -45L, -79L, -120L, 39L, 88L,
    -55L, -95L, -27L, -5L, -11L, 48L, 125L, 94L, 36L, 115L, -121L,
    99L, 47L, -108L, -99L, -9L, -70L, 67L, -89L, 107L, -80L, 113L,
    27L, 45L, 94L, 124L, 7L, 54L, 76L, -54L, -48L),
                   c(11L, 6L, 2L))
    expect_identical(m$var78[seq(2,32,3), seq(4,32,5), seq(8,32,16)], var78_slab)

    ## var79 read as integer
    var79_slab <- structure(c(36L, 154L, 198L, 69L, 227L, 194L, 140L,
                              19L, 11L, 17L, 151L, 74L, 65L, 249L,
                              151L, 109L, 131L, 194L, 237L, 190L,
                              218L, 35L, 142L, 205L, 42L, 43L, 2L,
                              15L, 177L, 240L, 164L, 112L, 138L, 112L,
                              97L, 154L, 26L, 191L, 53L, 214L, 216L,
                              229L, 187L, 192L, 148L, 230L, 21L, 210L,
                              13L, 28L, 216L, 1L, 21L, 20L, 179L,
                              136L, 111L, 22L, 103L, 188L, 221L, 4L,
                              139L, 238L, 175L, 207L, 193L, 24L, 236L,
                              141L, 144L, 226L, 48L, 36L, 207L, 199L,
                              30L, 249L, 124L, 83L, 90L, 102L, 213L,
                              227L, 255L, 152L, 212L, 62L, 252L, 18L,
                              133L, 214L, 101L, 22L, 176L, 101L, 233L,
                              106L, 39L, 237L, 241L, 212L, 19L, 102L,
                              212L, 123L, 57L, 225L, 27L, 178L, 98L,
                              88L, 40L, 115L, 185L, 226L, 23L, 22L,
                              160L, 111L, 100L, 44L, 145L, 144L, 223L,
                              123L, 48L, 190L, 70L, 115L, 113L, 7L),
                              .Dim = c(11L, 6L, 2L))
    expect_identical(m$var79[seq(2,32,3), seq(4,32,5), seq(8,32,16)], var79_slab)

    ## var80, ..., var85 read as complex
    var80 = array(complex(real=seq(1, 2*32^3, 2), imaginary=seq(2, 2*32^3, 2)), c(32,32,32))
    expect_identical(m$var80, var80)
    expect_identical(m$var81, var80)
    expect_identical(m$var82, var80)
    expect_identical(m$var83, var80)
    expect_identical(m$var84, var80)
    expect_identical(m$var85, var80)

    ## var86 read as complex
    var86_slab <- structure(c(31419+12074i, 11550+31935i,
                              14036-25970i, 12950-26466i,
                              11549-13221i, 8802-13913i, -8540+20686i,
                              82-2536i, 6556+25498i, -22761-19987i,
                              -14602-32033i, -16493-9400i,
                              15688+20383i, -10204-15596i,
                              14390-20232i, -21008+3161i,
                              22712-11293i, 16110-30851i, 6298-2954i,
                              -10732-25971i, -15133+15777i,
                              -23705+32679i, 2771-10261i,
                              -25754-31533i, 27007-3499i, -7948-4034i,
                              1047-17085i, 28781-6213i, 22132+27840i,
                              -2300-1878i, 14919-19351i,
                              -32577+15858i, 28973-16332i,
                              -13965+16888i, -1934-19892i,
                              24973-17196i, 29557-23198i,
                              -26196-27443i, 17138+17163i,
                              4173+22551i, 21456-372i, -12554-766i,
                              -11899+26000i, -13223-7784i,
                              -25303+5509i, 16352+363i, 9743-9728i,
                              6783-16717i, 27364-31769i, 845-5483i,
                              -5927+30238i, 9080+5283i, 25263-12552i,
                              -6594+9643i, -21471-8832i, 13196+15195i,
                              -12123-23263i, -10874+25982i,
                              -11012-7600i, -10870-24078i,
                              -4181+21428i, 19441-25234i,
                              27199-11909i, -4204+14748i,
                              -12638+6522i, -21381+3874i, 10009-1227i,
                              23342+16514i, 27792-11483i,
                              -16194-25596i, 9911-20183i,
                              -28341+22322i, 3508-7880i, 20369-482i,
                              -4578+21351i, 18426+31578i,
                              -26016+17152i, 23621+10035i,
                              -26955-8794i, 3884+18834i, 25669+17940i,
                              19480-25157i, -24243-10527i,
                              4464+28510i, -6616-24233i, -14599-1762i,
                              -19424+6424i, -29786+14220i,
                              -22333-20060i, -28080-30034i,
                              -24965+30996i, 11506-19238i,
                              -5438+17227i, 6325+19908i, 16761+20643i,
                              18192-20736i, -18629+17622i,
                              2759-21496i, 15764+18250i, -28782+3888i,
                              -26055-4279i, 17075-5598i, 15629+19686i,
                              -27961-5378i, 22850-7463i, -17585+883i,
                              -1421-6768i, 28956-7833i, 19321-31347i,
                              22739-27552i, -5804-12847i,
                              -19613+7820i, -24500+23384i,
                              6332-32352i, 9084+8866i, 12787+16795i,
                              27522+650i, 7219-25890i, -16811-5838i,
                              22569+6221i, 21757-28407i, -2817+10293i,
                              -666-21153i, 6202+23652i, -27772+7728i,
                              24485+27017i, 7898+4210i, 8062+28365i,
                              -28910-21963i, 7298-10445i,
                              29980+25776i, 5019-2720i), .Dim = c(11L,
                              6L, 2L))
    expect_identical(m$var86[seq(2,32,3), seq(4,32,5), seq(8,32,16)], var86_slab)

    ## var87 read as complex
    var87_slab <- structure(c(63490+42712i, 29714+8391i, 61483+15861i,
                              15850+33874i, 55082+3631i, 45390+5685i,
                              10423+60745i, 44728+38383i,
                              43170+36676i, 46711+20162i,
                              49188+52357i, 48615+54935i,
                              11083+51765i, 2973+2285i, 47407+28409i,
                              33701+18840i, 34873+47323i, 13711+7195i,
                              6046+45393i, 44211+53418i, 16574+31089i,
                              41210+28626i, 14063+54325i,
                              15062+33495i, 51022+34454i,
                              60470+39080i, 44448+44287i,
                              64996+30009i, 283+64783i, 18983+191i,
                              3436+45327i, 44524+18230i, 15729+21345i,
                              12093+4405i, 8834+7721i, 54420+31822i,
                              26675+29151i, 42660+29044i,
                              65323+50276i, 7139+32010i, 47157+29016i,
                              35840+18163i, 15379+14923i,
                              30214+63617i, 47337+59129i,
                              62332+27780i, 3623+2694i, 11530+27741i,
                              4141+36801i, 64250+50570i, 39722+49989i,
                              26118+40150i, 28214+21931i,
                              25798+27469i, 62573+58367i, 25576+6335i,
                              12201+17545i, 49659+37636i,
                              28187+58668i, 3135+5269i, 16458+14517i,
                              31662+28316i, 38430+36198i,
                              47450+50404i, 201+49160i, 50221+54517i,
                              47469+42818i, 7124+35280i, 34741+37493i,
                              44064+39694i, 6995+4154i, 7531+33600i,
                              54978+37775i, 15416+23755i,
                              27259+51376i, 64300+6331i, 1212+37261i,
                              8920+61180i, 29466+54541i, 65042+23250i,
                              1978+56271i, 14377+47601i, 12353+11382i,
                              8336+45819i, 45530+55892i, 8908+40510i,
                              11258+60909i, 56638+57033i,
                              45303+55200i, 4881+31280i, 38207+48851i,
                              807+7069i, 50473+21558i, 6544+29624i,
                              52787+2034i, 5986+58345i, 57054+22602i,
                              2935+55316i, 42851+16897i, 10001+43967i,
                              48614+38397i, 14572+48016i,
                              25265+24936i, 1715+40096i, 37345+13265i,
                              11025+46066i, 37554+31865i, 4791+53489i,
                              22789+15282i, 23465+10050i,
                              58589+45954i, 9073+25270i, 20846+33025i,
                              22905+59697i, 22981+42347i,
                              24828+62213i, 46887+36422i,
                              50013+60501i, 29781+52236i,
                              37210+42165i, 37785+60771i,
                              16219+17867i, 59934+13526i,
                              58464+17576i, 34801+59568i,
                              20649+30475i, 55999+42307i, 4464+16054i,
                              23302+47556i, 24764+55871i,
                              22331+53630i, 63541+34455i), .Dim =
                              c(11L, 6L, 2L))
    expect_identical(m$var87[seq(2,32,3), seq(4,32,5), seq(8,32,16)], var87_slab)

    ## var88 read as complex
    var88_slab <- structure(c(24-109i, -38+98i, -32+69i, 1-58i,
                              106+37i, 23-118i, 86-117i, 3-10i,
                              -94+111i, 43-69i, 37+24i, 5+103i,
                              -71-109i, -9+76i, 52-40i, -48+1i,
                              -1-71i, 43+106i, 121-39i, 68+45i, 9+7i,
                              49-122i, 79-98i, -125-9i, -81-45i,
                              -30+6i, 109-65i, -111-88i, -21-57i,
                              -59-22i, 64-47i, 38+36i, 32+84i,
                              -11-41i, -4-5i, 111-12i, -79+23i,
                              118-68i, 60-49i, -31-47i, 98-49i,
                              19-84i, 114-19i, 126+112i, 3-95i,
                              -69-113i, -24-55i, -40-78i, 81-95i,
                              108-15i, -37+5i, 24-97i, 65+17i,
                              -79-19i, -54-36i, -99-15i, -92-70i,
                              115+62i, -29-52i, -121+61i, -62+101i,
                              -72+52i, -110-111i, 126+124i, 85-44i,
                              -32+42i, 10+105i, -88-104i, 82+96i,
                              98-85i, -66-27i, 83-12i, -18-32i,
                              58-92i, -59-89i, -45+16i, -36-68i,
                              -120-94i, 69+16i, 5+114i, 80+42i, -7+4i,
                              74+84i, 83+76i, -74+93i, -124+7i,
                              -61+54i, 3-83i, -79-87i, -50+8i,
                              -13-48i, 51-21i, -95+3i, -115-5i,
                              117+120i, 66+29i, 64+10i, 54-101i,
                              51+74i, 55-21i, -78+49i, -61+22i,
                              74-73i, -26+71i, 81+0i, -45-123i,
                              -8+58i, 63-124i, -83-112i, 97+123i,
                              19+106i, 18+47i, -39-73i, -47-60i,
                              -102-120i, -17-102i, 18+66i, -12-59i,
                              -41+34i, 46+51i, -61-40i, 47-65i,
                              -103-35i, 127+30i, 28+16i, -72-13i,
                              -76-67i, -46+33i, 40+109i, -117+25i,
                              103+12i, 89-9i), .Dim = c(11L, 6L, 2L))
    expect_identical(m$var88[seq(2,32,3), seq(4,32,5), seq(8,32,16)], var88_slab)

    ## var89 read as complex
    var89_slab <- structure(c(233+19i, 99+152i, 194+176i, 219+39i,
                              182+250i, 198+121i, 70+99i, 250+205i,
                              154+19i, 20+127i, 55+248i, 98+36i,
                              129+239i, 246+207i, 42+97i, 209+14i,
                              114+150i, 245+204i, 62+143i, 192+87i,
                              88+125i, 254+59i, 145+191i, 23+137i,
                              229+96i, 17+104i, 158+68i, 85+154i,
                              207+145i, 112+116i, 135+182i, 138+79i,
                              118+171i, 87+191i, 187+27i, 224+67i,
                              96+230i, 66+32i, 196+72i, 239+53i,
                              15+197i, 111+188i, 225+144i, 190+181i,
                              30+24i, 149+230i, 56+158i, 114+96i,
                              242+79i, 82+206i, 106+114i, 8+202i,
                              87+217i, 138+126i, 26+138i, 142+157i,
                              186+204i, 139+252i, 130+202i, 197+221i,
                              169+34i, 160+148i, 131+129i, 255+157i,
                              93+4i, 140+149i, 182+191i, 45+138i,
                              41+32i, 74+142i, 173+106i, 14+5i,
                              56+15i, 159+205i, 218+172i, 235+235i,
                              253+46i, 212+183i, 121+33i, 193+87i,
                              95+150i, 75+167i, 139+111i, 241+161i,
                              62+17i, 165+198i, 249+244i, 207+210i,
                              51+193i, 81+133i, 42+147i, 219+48i,
                              185+124i, 65+8i, 120+240i, 24+19i,
                              104+3i, 23+34i, 88+216i, 46+45i,
                              90+128i, 171+187i, 105+53i, 23+168i,
                              111+218i, 91+101i, 90+72i, 178+168i,
                              172+79i, 176+42i, 16+249i, 137+65i,
                              149+145i, 87+154i, 111+32i, 179+46i,
                              164+95i, 121+106i, 125+144i, 215+171i,
                              48+141i, 110+132i, 234+197i, 22+223i,
                              229+72i, 169+26i, 128+186i, 87+139i,
                              152+77i, 17+58i, 215+242i, 62+229i ),
                              .Dim = c(11L, 6L, 2L))
    expect_identical(m$var89[seq(2,32,3), seq(4,32,5), seq(8,32,16)], var89_slab)

    var90 <- array(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
                     TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE,
                     FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                     FALSE, FALSE, TRUE), c(5L, 5L))
    expect_identical(m$var90, var90)

    var91 <- structure(list(field1 = list(structure(c(FALSE, TRUE,
                                FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                                FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                                FALSE, TRUE, FALSE, TRUE, FALSE,
                                TRUE), .Dim = 4:5), structure(c(TRUE,
                                TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
                                TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,
                                TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
                                TRUE, FALSE, FALSE, FALSE, FALSE,
                                TRUE), .Dim = c(5L, 5L))), field2 =
                                list( structure(c(TRUE, FALSE, TRUE,
                                FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                                FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                                FALSE, TRUE, FALSE, TRUE, FALSE), .Dim
                                = 4:5), structure(c(TRUE, FALSE,
                                FALSE, FALSE, FALSE, TRUE, TRUE,
                                FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
                                FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,
                                FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
                                .Dim = c(5L, 5L)))), .Names =
                                c("field1", "field2"))
    expect_identical(m$var91, var91)

    var92 <- list(structure(c(FALSE, TRUE, FALSE, TRUE, FALSE,
                              TRUE, FALSE, TRUE, FALSE, TRUE,
                              FALSE, TRUE, FALSE, TRUE, FALSE,
                              TRUE, FALSE, TRUE, FALSE, TRUE),
                            .Dim = 4:5),
                  structure(c(TRUE, FALSE, TRUE, FALSE, TRUE,
                              FALSE, TRUE, FALSE, TRUE, FALSE,
                              TRUE, FALSE, TRUE, FALSE, TRUE,
                              FALSE, TRUE, FALSE, TRUE, FALSE),
                            .Dim = 4:5),
                  structure(c(TRUE, TRUE, TRUE, TRUE, TRUE,
                              FALSE, TRUE, TRUE, TRUE, TRUE,
                              FALSE, FALSE, TRUE, TRUE, TRUE,
                              FALSE, FALSE, FALSE, TRUE, TRUE,
                              FALSE, FALSE, FALSE, FALSE, TRUE),
                            .Dim = c(5L, 5L)),
                  structure(c(TRUE, FALSE, FALSE, FALSE, FALSE,
                              TRUE, TRUE, FALSE, FALSE, FALSE,
                              TRUE, TRUE, TRUE, FALSE, FALSE,
                              TRUE, TRUE, TRUE, TRUE, FALSE,
                              TRUE, TRUE, TRUE, TRUE, TRUE),
                            .Dim = c(5L, 5L)))
    expect_identical(m$var92, var92)
}

context("Read: matio_test_cases_compressed_le.mat")

test_that("matio_test_cases_compressed_le.mat", {
    if(have.zlib()) {
        test_mat_v5_file('../extdata/matio_test_cases_compressed_le.mat')
    }
})

context("Write MAT5 uncompressed: matio_test_cases_compressed_le.mat")

test_that("matio_test_cases_compressed_le.mat", {
    if(have.zlib()) {
        filename <- tempfile(fileext = ".mat")
        on.exit(unlink(filename))

        write.mat(read.mat('../extdata/matio_test_cases_compressed_le.mat'),
                  filename=filename,
                  compression=FALSE,
                  version='MAT5')
        test_mat_v5_file(filename)
    }
})

context("Write MAT5 compressed: matio_test_cases_compressed_le.mat")

test_that("matio_test_cases_compressed_le.mat", {
    if(have.zlib()) {
        filename <- tempfile(fileext = ".mat")
        on.exit(unlink(filename))

        write.mat(read.mat('../extdata/matio_test_cases_compressed_le.mat'),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        test_mat_v5_file(filename)
    }
})
