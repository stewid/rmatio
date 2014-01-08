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

setMethod("write.mat",
          signature(object = "list"),
          function(object,
                   filename,
                   compression,
                   version)
          {
            ## Check filename
            if(any(!is.character(filename),
                   !identical(length(filename), 1L),
                   nchar(filename) < 1)) {
              stop("'filename' must be a character vector of length one")
            }

            ## Check compression
            if(any(!is.logical(compression),
                   !identical(length(compression), 1L))) {
              stop("'compression' must be a logical vector of length one")
            }

            if(identical(compression, TRUE)) {
                compression = 1L
            } else {
                compression = 0L
            }

            ## Check version
            version <- match.arg(version)
            if(identical(version, 'MAT5')) {
              version <- 0x0100L
              header <- sprintf("MATLAB 5.0 MAT-file, Platform: %s, Created By: rmatio v%s on %s",
                                R.version$platform[[1]],
                                packageVersion('rmatio'),
                                date())
            ## } else if(identical(version, 'MAT73')) {
            ##   version <- 0x0200L
            } else {
              stop('Undefined version')
            }

            ## Check names in object
            if(any(is.null(names(object)),
                   !all(nchar(names(object))),
                   any(duplicated(names(object))))) {
              stop("All values in the list must have a unique name")
            }

            .Call("write_mat", object, filename, compression, version, header)

            invisible(NULL)
          }
)
