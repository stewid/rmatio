## rmatio, a R interface to the C library matio, MAT File I/O Library.
## Copyright (C) 2013  Stefan Widgren
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

##' Writes the values in a list to a mat-file.
##'
##' Writes the values in the list to a mat-file using the C library
##' MATIO. All values in the list must have unique names. The
##' following data structures are implemented.\cr
##' 
##' \strong{Vectors and matrices}
##' \tabular{lllll}{
##'   \bold{R data structure} \tab \bold{R storage mode} \tab \bold{MATIO CLASS} \tab \bold{MATIO TYPE} \tab \bold{Dimension}\cr
##'   \code{\link[base:integer]{vector}} \tab integer \tab MAT_C_DOUBLE \tab MAT_T_DOUBLE \tab 1 x length \cr
##'   \code{\link[base:double]{vector}} \tab double  \tab MAT_C_DOUBLE \tab MAT_T_DOUBLE \tab 1 x length \cr
##'   \code{\link[base:matrix]{matrix}} \tab integer \tab MAT_C_DOUBLE \tab MAT_T_DOUBLE \tab row x col  \cr
##'   \code{\link[base:matrix]{matrix}} \tab double  \tab MAT_C_DOUBLE \tab MAT_T_DOUBLE \tab row x col  \cr
##' }
##'
##' \strong{Sparse numeric matrices}
##' \tabular{lllll}{
##'   \bold{R data structure} \tab \bold{MATIO CLASS} \tab \bold{MATIO TYPE} \tab \bold{Dimension}\cr
##'   \code{\link[=dgCMatrix-class]{dgCMatrix}} \tab MAT_C_SPARSE \tab MAT_T_DOUBLE \tab row x col \cr
##' }
##' 
##' @name write.mat-methods
##' @aliases write.mat
##' @aliases write.mat-methods
##' @aliases write.mat,list-method
##' @docType methods
##' @title Write Matlab file
##' @param object The \code{object} to write.
##' @param filename The MAT file to write.
##' @param overwrite logical or 'NULL' (the default), specifying if
##' the method should overwrite an existing file or signal error. By
##' default, it signals error.
##' @return invisible NULL
##' @references \itemize{
##'   \item Christopher C. Hulbert, MATIO User Manual for version 1.5.1.\cr
##'   \url{http://sourceforge.net/projects/matio/files/matio/1.5.1/matio_user_guide.pdf}
##' }
##' @keywords methods
##' @export
##' @useDynLib rmatio
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
##'                    overwrite)
##'           {
##'             ## Coerce the 'DemoS4Mat' object to a list and
##'             ## call 'rmatio' 'write.mat' with the list.
##'             return(write.mat(as(object, "list"),
##'                              filename,
##'                              overwrite))
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
                    overwrite = NULL) standardGeneric("write.mat"))

setMethod("write.mat",
          signature(object = "list"),
          function(object,
                   filename,
                   overwrite)
          {
            ## Check filename
            if(any(!is.character(filename),
                   !identical(length(filename), 1L),
                   nchar(filename) < 1)) {
              stop("'filename' must be a character vector of length one")
            }

            ## Check overwrite
            if(is.null(overwrite) || !is.logical(overwrite) || !overwrite) {
              if(file.exists(filename)) {
                stop("File exists")
              }
            }

            ## Check names in object          
            if(any(is.null(names(object)),
                   !all(nchar(names(object))),
                   any(duplicated(names(object))))) {
              stop("All values in the list must have a unique name")
            }                        
            
            .Call("write_mat", object, filename)

            invisible(NULL)
          }
)
