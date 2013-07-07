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
  on.exit(unlink(filename))
  
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
})

test_that("dgCMatrix: case-2", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- as(diag(1:5), 'dgCMatrix')

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]

  expect_identical(a.2, a.1)
})

context("vector")

test_that("vector: case-1", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(list(a=1:5), filename=filename)
  a <- read.mat(filename)[['a']]
  storage.mode(a) <- 'integer'

  expect_identical(a, 1:5)
})

test_that("vector: case-2", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(list(a=c(1,2,3,4,5)), filename=filename)
  a <- read.mat(filename)[['a']]

  expect_identical(a, c(1,2,3,4,5))
})

test_that("vector: case-3", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(list(a=1), filename=filename)
  a <- read.mat(filename)[['a']]

  expect_identical(a, 1)
})

context("matrix")

test_that("matrix: case-1", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(list(a=matrix(1:9, nrow=3)), filename=filename)
  a <- read.mat(filename)[['a']]
  storage.mode(a) <- 'integer'

  expect_identical(a, matrix(1:9, nrow=3))
})

test_that("matrix: case-2", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  write.mat(list(a=matrix(c(1,2,3,4,5,6,7,8,9), nrow=3)), filename=filename)
  a <- read.mat(filename)[['a']]

  expect_identical(a, matrix(c(1,2,3,4,5,6,7,8,9), nrow=3))
})

context("array")

test_that("array: case-1", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- array(seq_len(32^3), c(32,32,32))
  storage.mode(a.1) <- 'integer'

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]

  expect_identical(a.2, a.1)
})

test_that("array: case-2", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- array(seq_len(32^3), c(32,32,32))
  storage.mode(a.1) <- 'double'

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]

  expect_identical(a.2, a.1)
})

context("complex")

test_that("complex: case-1", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- array(complex(real=1:20,imaginary=21:40), c(4,5))

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]

  expect_identical(a.2, a.1)
})

context("string")

test_that("string: case-1", {
    filename <- tempfile(fileext = ".mat")
    on.exit(unlink(filename))
    
    a.1 <- c("abcdefghijklmnopqrstuvwxyz",
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
             "1234567890!@#$%^&*()-_=+`~",
             "[{]}\\|;:'\",<.>/?          ")
    
    write.mat(list(a=a.1), filename=filename)
    a.2 <- read.mat(filename)[['a']]
    
    expect_identical(a.2, a.1)
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

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

context("struct")

test_that("struct: case-1 (Empty structure array)", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))
    
  a.1 <- structure(list(), .Names = character(0))

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

test_that("struct: case-2 (Empty structure array with fields)", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- list(field1=list(), field2=list())

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

test_that("struct: case-3 (Structure array with empty fields)", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- list(field1=numeric(0),
              field2=character(0),
              field3=complex(0),
              filed4=integer(0),
              field5=logical(0))

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

test_that("struct: case-4", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- list(field1=list(1, 14),
              field2=list(array(as.numeric(2:13), c(3,4)),
                  array(as.numeric(15:26), c(3,4))))
  
  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

test_that("struct: case-5", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))
  
  a.1 <- list(field1=list(1L, 14L),
              field2=list(array(2:13, c(3,4)),
                  array(15:26, c(3,4))))
  
  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

test_that("struct: case-6", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))
  
  a.1 <- list(field1=list(1+51i, 14+64i), field2=list(array(c(2+52i,
              3+53i, 4+54i, 5+55i, 6+56i, 7+57i, 8+58i, 9+59i, 10+60i,
              11+61i, 12+62i, 13+63i), c(3,4)), array(c(15+65i,
              16+66i, 17+67i, 18+68i, 19+69i, 20+70i, 21+71i, 22+72i,
              23+73i, 24+74i, 25+75i, 26+76i), c(3,4))))
  
  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})
    
test_that("struct: case-7", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- list(field1=list(triu(Matrix(1:20, nrow=4, ncol=5, sparse=TRUE))),
              field2=list(tril(Matrix(1:20, nrow=5, ncol=4, sparse=TRUE, byrow=TRUE))))

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
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
  
  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

test_that("struct: case-9", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- list(field1 = c("abcdefghijklmnopqrstuvwxyz",
                  "1234567890!@#$%^&*()-_=+`~"),
              field2 = c("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                  "[{]}\\|;:'\",<.>/?          "))
  
  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

test_that("struct: case-10 (Structure array with empty fields)", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- list(field1=numeric(0))

  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})

test_that("struct: case-11", {
  filename <- tempfile(fileext = ".mat")
  on.exit(unlink(filename))

  a.1 <- list(field1=list(1))
  
  write.mat(list(a=a.1), filename=filename)
  a.2 <- read.mat(filename)[['a']]
    
  expect_identical(a.2, a.1)
})
