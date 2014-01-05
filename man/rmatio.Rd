\docType{package}
\name{rmatio}
\alias{rmatio}
\alias{rmatio-package}
\title{\pkg{rmatio}: reading and writing Matlab MAT files from R}
\description{
  Reading and writing Matlab MAT files from R
}
\details{
  \code{rmatio} supports reading MAT version 4, MAT version
  5 and MAT compressed version 5.

  \code{rmatio} can write version 5 MAT files and version 5
  files with variable compression.
}
\examples{
\dontrun{
library(rmatio)

## Read a compressed version 5 MAT file from an URL
url <- paste("http://sourceforge.net/p/matio/matio_test_datasets/ci/",
             "master/tree/matio_test_cases_compressed_le.mat?format=raw",
             sep="")
m <- read.mat(url)

## View content
str(m)

## Write an uncompressed version 5 MAT file
write.mat(m, filename="test-uncompressed.mat", compression=FALSE, version="MAT5")

## Write a compressed version 5 MAT file
write.mat(m, filename="test-compressed.mat", compression=TRUE, version="MAT5")

## Check that the content of the files are identical
identical(read.mat("test-uncompressed.mat"), read.mat("test-compressed.mat"))
}
}
\author{
  Stefan Widgren
}
\references{
  \itemize{ \item Christopher C. Hulbert, MATIO User Manual
  for version 1.5.2.\cr
  \url{http://sourceforge.net/projects/matio/files/matio/1.5.2/matio_user_guide.pdf}

  \item The MathWorks Inc., MATLAB - MAT-File Format,
  version R2013b, September 2013.\cr
  \url{http://www.mathworks.com/help/pdf_doc/matlab/matfile_format.pdf}
  }
}

