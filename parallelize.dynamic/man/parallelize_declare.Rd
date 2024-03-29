% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rparallel.R
\name{parallelize_declare}
\alias{parallelize_declare}
\title{Declare static parallelization environment for parallel calls.}
\usage{
parallelize_declare(
  source = NULL,
  packages = NULL,
  copy = NULL,
  reset = TRUE,
  config = NULL,
  salt = NULL,
  activeDictionary = "native"
)
}
\arguments{
\item{source}{Overwrite the \code{sourceFiles} entry in
\code{Lapply_config}.}

\item{packages}{Overwrite the \code{library} entry in
\code{Lapply_config}.}

\item{copy}{Vector of pathes that is recursively copied if needed.}

\item{reset}{If true (the default), values are replaced otherwise values are appended}
}
\value{
Value \code{NULL} is returned.
}
\description{
This includes files to be 
sourced, packages to be loaded and files to be copied. This function performs a subset of
\code{parallelize_initialize} and its intended use is to factor out certain parameters from
\code{parallelize_initialize} calls.
}
\examples{

  ## Not run:
  # run ensuing parallelizations locally, ignore result produced earlier
  parallelize_declare(source = 'mySourceFile.R', packages = 'glmnet');

}
\seealso{
\code{\link{parallelize}}, \code{\link{parallelize_call}}, 
 \code{\linkS4class{ParallelizeBackend}},
 \code{\linkS4class{ParallelizeBackendLocal}},
  \code{\linkS4class{ParallelizeBackendSnow}},
  \code{\linkS4class{ParallelizeBackendOGSremote}}
}
\author{
Stefan Böhringer <r-packages@s-boehringer.org>
}
