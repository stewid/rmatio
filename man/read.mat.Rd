% rmatio, a R interface to the C library matio, MAT File I/O Library.
% Copyright (C) 2013  Stefan Widgren
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% rmatio is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

\name{read.mat}
\alias{read.mat}
\title{Read Matlab file}
\usage{
  read.mat(filename)
}
\arguments{
  \item{filename}{Character string, with the MAT file or
  URL to read.}
}
\value{
  A list with the variables read.
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
\examples{
\dontrun{
## Read a mat file from an URL
m <- read.mat('http://sourceforge.net/p/matio/matio_test_datasets/ci/master/tree/matio_test_cases_compressed_le.mat?format=raw')
}
}
\references{
  \itemize{ \item Christopher C. Hulbert, MATIO User Manual
  for version 1.5.1.\cr
  \url{http://sourceforge.net/projects/matio/files/matio/1.5.1/matio_user_guide.pdf}
  }
}
\seealso{
  See \code{\link[rmatio:write.mat]{write.mat}} for more
  details and examples.
}

