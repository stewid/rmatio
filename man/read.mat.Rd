\name{read.mat}
\alias{read.mat}
\title{Read Matlab file}
\usage{
  read.mat(filename)
}
\arguments{
  \item{filename}{The MAT file to read.}
}
\value{
  list
}
\description{
  Reads the values in a mat-file to a list.
}
\details{
  Reads the values in a mat-file using the C library MATIO
  and stores them in a list. The following data structures
  are implemented.\cr

  \strong{Vectors} \tabular{llll}{ \bold{MATIO CLASS} \tab
  \bold{Dimension} \tab \bold{R data structure} \tab
  \bold{Length}\cr MAT_C_DOUBLE \tab 1 x n \tab
  \code{\link[base:double]{vector}} \tab n\cr MAT_C_DOUBLE
  \tab n x 1 \tab \code{\link[base:double]{vector}} \tab
  n\cr }

  \strong{Matrices} \tabular{llll}{ \bold{MATIO CLASS} \tab
  \bold{Dimension} \tab \bold{R data structure} \tab
  \bold{Dim}\cr MAT_C_DOUBLE \tab row x col \tab
  \code{\link[base:matrix]{matrix}} \tab row x col\cr
  MAT_C_DOUBLE \tab row x col \tab
  \code{\link[base:matrix]{matrix}} \tab row x col\cr }

  \strong{Sparse numeric matrices} \tabular{llll}{
  \bold{MATIO CLASS} \tab \bold{Dimension} \tab \bold{R
  data structure} \tab \bold{Dim}\cr MAT_C_SPARSE \tab row
  x col \tab \code{\link[=dgCMatrix-class]{dgCMatrix}} \tab
  row x col\cr }
}
\references{
  \itemize{ \item Christopher C. Hulbert, MATIO User Manual
  for version 1.5.\cr
  \url{http://sourceforge.net/projects/matio/files/matio/1.5.0/matio_user_guide.pdf}
  }
}
\seealso{
  See \code{\link[rmatio:write.mat]{write.mat}} for more
  details and examples.
}

