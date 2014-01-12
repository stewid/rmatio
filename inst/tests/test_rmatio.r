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

    if(rmatio:::have.zlib()) {
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

    if(rmatio:::have.zlib()) {
        write.mat(list(a=a.1),
                  filename=filename,
                  compression=TRUE,
                  version='MAT5')
        a.2 <- read.mat(filename)[['a']]
        expect_identical(a.2, a.1)
    }
})
