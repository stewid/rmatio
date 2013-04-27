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

##' Reads the values in a mat-file to a list.
##'
##' Reads the values in a mat-file using the C library MATIO and
##' stores them in a list. The following data structures are
##' implemented.\cr
##' 
##' \strong{Vectors}
##' \tabular{llll}{
##'   \bold{MATIO CLASS} \tab \bold{Dimension} \tab \bold{R data structure} \tab \bold{Length}\cr
##'   MAT_C_DOUBLE \tab 1 x n \tab \code{\link[base:double]{vector}} \tab n\cr
##'   MAT_C_DOUBLE \tab n x 1 \tab \code{\link[base:double]{vector}} \tab n\cr
##' }
##' 
##' \strong{Matrices}
##' \tabular{llll}{
##'   \bold{MATIO CLASS} \tab \bold{Dimension} \tab \bold{R data structure} \tab \bold{Dim}\cr
##'   MAT_C_DOUBLE \tab row x col \tab \code{\link[base:matrix]{matrix}} \tab row x col\cr
##'   MAT_C_DOUBLE \tab row x col \tab \code{\link[base:matrix]{matrix}} \tab row x col\cr
##' }
##'
##' \strong{Sparse numeric matrices}
##' \tabular{llll}{
##'   \bold{MATIO CLASS} \tab \bold{Dimension} \tab \bold{R data structure} \tab \bold{Dim}\cr
##'   MAT_C_SPARSE \tab row x col \tab \code{\link[=dgCMatrix-class]{dgCMatrix}} \tab row x col\cr
##' }
##' 
##' @title Read Matlab file
##' @param filename  The MAT file to read.
##' @return list
##' @references \itemize{
##'   \item Christopher C. Hulbert, MATIO User Manual for version 1.5.1.\cr
##'   \url{http://sourceforge.net/projects/matio/files/matio/1.5.1/matio_user_guide.pdf}
##' }
##' @seealso See \code{\link[rmatio:write.mat]{write.mat}} for more details and examples.
##' @export
##' @useDynLib rmatio
read.mat <- function(filename) {
    ## Argument checking
    stopifnot(is.character(filename),
              identical(length(filename), 1L),
              nchar(filename) > 0)

    if(!file.exists(filename)) {
        stop(sprintf("File don't exists: %s", filename))
    }

    return(.Call('read_mat', filename))
}

