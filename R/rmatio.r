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

##' \pkg{rmatio}: reading and writing Matlab MAT files from R
##'
##' Reading and writing Matlab MAT files from R
##'
##' \code{rmatio} supports reading MAT version 4, MAT version 5 and
##' MAT compressed version 5.
##'
##' \code{rmatio} can write version 5 MAT files and version 5 files
##' with variable compression.
##' @import Matrix
##' @import methods
##' @import lattice
##' @name rmatio
##' @references \itemize{
##'   \item Christopher C. Hulbert, MATIO User Manual for version 1.5.2.\cr
##'   \url{http://sourceforge.net/projects/matio/files/matio/1.5.2/matio_user_guide.pdf}
##'
##'   \item The MathWorks Inc., MATLAB - MAT-File Format, version R2013b, September 2013.\cr
##'   \url{http://www.mathworks.com/help/pdf_doc/matlab/matfile_format.pdf}
##' }
##' @docType package
##' @author Stefan Widgren
##' @examples
##' \dontrun{
##' library(rmatio)
##'
##' ## Read a compressed version 5 MAT file from an URL
##' url <- paste("http://sourceforge.net/p/matio/matio_test_datasets/ci/",
##'              "master/tree/matio_test_cases_compressed_le.mat?format=raw",
##'              sep="")
##' m <- read.mat(url)
##'
##' ## View content
##' str(m)
##'
##' ## Write an uncompressed version 5 MAT file
##' write.mat(m, filename="test-uncompressed.mat", compression=FALSE, version="MAT5")
##'
##' ## Write a compressed version 5 MAT file
##' write.mat(m, filename="test-compressed.mat", compression=TRUE, version="MAT5")
##'
##' ## Check that the content of the files are identical
##' identical(read.mat("test-uncompressed.mat"), read.mat("test-compressed.mat"))
##' }
NULL

##' Writes the values in a list to a mat-file.
##'
##' Writes the values in the list to a mat-file. All values in the
##' list must have unique names.
##' @note
##' \itemize{
##'   \item A vector is saved as a \code{1 x length} array
##'
##'   \item Support for writing a sparse matrix of type 'dgCMatrix' or 'lgCMatrix'
##'     to file
##' }
##' @name write.mat-methods
##' @aliases write.mat
##' @aliases write.mat-methods
##' @aliases write.mat,list-method
##' @docType methods
##' @title Write Matlab file
##' @param object The \code{object} to write.
##' @param filename The MAT file to write.
##' @param compression Use compression when writing variables. Defaults to TRUE.
##' @param version MAT file version to create. Currently only support
##' for Matlab level-5 file (MAT5) from rmatio package.
##' @return invisible NULL
##' @keywords methods
##' @export
##' @useDynLib rmatio
##' @author Stefan Widgren
##' @examples
##' \dontrun{
##' filename <- tempfile(fileext = ".mat")
##'
##' ## Example how to read and write an integer vector with rmatio
##' write.mat(list(a=1:5), filename=filename)
##' a <- as.integer(read.mat(filename)[["a"]])
##'
##' stopifnot(identical(a, 1:5))
##'
##' unlink(filename)
##'
##' ## Example how to read and write a S4 class with rmatio
##' ## Create 'DemoS4Mat' class
##' setClass("DemoS4Mat",
##'          representation(a = "dgCMatrix",
##'                         b = "integer",
##'                         c = "matrix",
##'                         d = "numeric"))
##'
##' ## Create a function to coerce a 'DemoS4Mat' object to a list.
##' setAs(from="DemoS4Mat",
##'       to="list",
##'       def=function(from)
##'       {
##'         return(list(a=from@@a,
##'                     b=from@@b,
##'                     c=from@@c,
##'                     d=from@@d))
##'       }
##' )
##'
##' ## Create a function to coerce a list to a 'DemoS4Mat' object.
##' setAs(from="list",
##'       to="DemoS4Mat",
##'       def=function(from)
##'       {
##'         return(new("DemoS4Mat",
##'                     a=from[["a"]],
##'                     b=as.integer(from[["b"]]),
##'                     c=from[["c"]],
##'                     d=from[["d"]]))
##'       }
##' )
##'
##' ## Define a method to write a 'DemoS4Mat' object to a MAT file.
##' setMethod("write.mat",
##'           signature(object = "DemoS4Mat"),
##'           function(object,
##'                    filename,
##'                    compression,
##'                    version)
##'           {
##'             ## Coerce the 'DemoS4Mat' object to a list and
##'             ## call 'rmatio' 'write.mat' with the list.
##'             return(write.mat(as(object, "list"),
##'                              filename,
##'                              compression,
##'                              version))
##'           }
##' )
##'
##' ## Create a new 'DemoS4Mat' object
##' demoS4mat <- new("DemoS4Mat",
##'                  a = Matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0,
##'                               0, 0, 0, 0, 0, 0, 0, 1, 0,
##'                               0, 0, 0, 0, 0, 0, 0, 0, 1),
##'                               nrow=3,
##'                               ncol=9,
##'                               byrow=TRUE,
##'                               sparse=TRUE),
##'                  b = 1:5,
##'                  c = matrix(as.numeric(1:9), nrow=3),
##'                  d = c(6.0, 7.0, 8.0))
##'
##' ## Write to MAT file
##' write.mat(demoS4mat, filename)
##'
##' ## Read the MAT file
##' demoS4mat.2 <- as(read.mat(filename), "DemoS4Mat")
##'
##' ## Check result
##' stopifnot(identical(demoS4mat, demoS4mat.2))
##'
##' unlink(filename)
##' }
setGeneric("write.mat",
           signature = "object",
           function(object,
                    filename = NULL,
                    compression = TRUE,
                    version = c('MAT5')) standardGeneric("write.mat"))
