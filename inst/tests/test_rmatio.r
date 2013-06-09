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

test_that("dgCMatrix: case-2", {
  filename <- tempfile(fileext = ".mat")

  a.1 <- as(diag(1:5), 'dgCMatrix')

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

context("array")

test_that("array: case-1", {
  filename <- tempfile(fileext = ".mat")

  a.1 <- array(seq_len(32^3), c(32,32,32))

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
  storage.mode(a.2) <- 'integer'

  expect_identical(a.2, a.1)
  
  unlink(filename)
})

test_that("array: case-2", {
  filename <- tempfile(fileext = ".mat")

  a.1 <- array(seq_len(32^3), c(32,32,32))
  storage.mode(a.1) <- 'double'

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]

  expect_identical(a.2, a.1)
  
  unlink(filename)
})

context("complex")

test_that("complex: case-1", {
  filename <- tempfile(fileext = ".mat")

  a.1 <- array(complex(real=1:20,imaginary=21:40), c(4,5))

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]

  expect_identical(a.2, a.1)
  
  unlink(filename)
})

