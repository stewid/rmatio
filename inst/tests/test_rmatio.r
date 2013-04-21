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

test_that("Overwrite", {
  filename <- tempfile(fileext = ".mat")
  write.mat(list(a=1:5), filename=filename)
  
  expect_error(write.mat(list(a=1:5), filename=filename),
               sprintf("File exists"))

  expect_error(write.mat(list(a=1:5), filename=filename, overwrite=3),
               sprintf("File exists"))

  expect_error(write.mat(list(a=1:5), filename=filename, overwrite=1),
               sprintf("File exists"))

  expect_error(write.mat(list(a=1:5), filename=filename, overwrite=0),
               sprintf("File exists"))

  expect_error(write.mat(list(a=1:5), filename=filename, overwrite=FALSE),
               sprintf("File exists"))

  expect_true(is.null(write.mat(list(a=1:5), filename=filename, overwrite=TRUE)))
  
  unlink(filename)  
})

test_that("Names", {
  expect_error(write.mat(list(1:5), filename="a.mat"),
               "All values in the list must have a unique name")

  expect_error(write.mat(list(a=1:5, 6:10), filename="a.mat"),
               "All values in the list must have a unique name")

  expect_error(write.mat(list(a=1:5, a=6:10), filename="a.mat"),
               "All values in the list must have a unique name")
})

context("dgCMatrix")

test_that("dgCMatrix: case-1", {
  filename <- tempfile(fileext = ".mat")
  
  a.1 <- Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 1, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 1),
                nrow=3,
                ncol=9,
                byrow=TRUE,
                sparse=TRUE)

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]

  expect_identical(a.2, a.1)

  unlink(filename)
})

context("vector")

test_that("vector: case-1", {
  filename <- tempfile(fileext = ".mat")

  write.mat(list(a=1:5), filename=filename)
  a <- read.mat(filename)[['a']]
  storage.mode(a) <- 'integer'

  expect_identical(a, 1:5)
  
  unlink(filename)
})

test_that("vector: case-2", {
  filename <- tempfile(fileext = ".mat")

  write.mat(list(a=c(1,2,3,4,5)), filename=filename)
  a <- read.mat(filename)[['a']]

  expect_identical(a, c(1,2,3,4,5))
  
  unlink(filename)
})

test_that("vector: case-3", {
  filename <- tempfile(fileext = ".mat")

  write.mat(list(a=1), filename=filename)
  a <- read.mat(filename)[['a']]

  expect_identical(a, 1)
  
  unlink(filename)
})

context("matrix")

test_that("matrix: case-1", {
  filename <- tempfile(fileext = ".mat")

  write.mat(list(a=matrix(1:9, nrow=3)), filename=filename)
  a <- read.mat(filename)[['a']]
  storage.mode(a) <- 'integer'

  expect_identical(a, matrix(1:9, nrow=3))
  
  unlink(filename)
})

test_that("matrix: case-2", {
  filename <- tempfile(fileext = ".mat")

  write.mat(list(a=matrix(c(1,2,3,4,5,6,7,8,9), nrow=3)), filename=filename)
  a <- read.mat(filename)[['a']]

  expect_identical(a, matrix(c(1,2,3,4,5,6,7,8,9), nrow=3))
  
  unlink(filename)
})
