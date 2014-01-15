\name{parallelize_initialize}
\alias{Lapply_initialize}
\alias{parallelize_initialize}
\title{Initialize dynamic parallelization of ensuing parallelize calls}
\usage{
  parallelize_initialize(Lapply_config = Lapply_config_default,
    stateClass = "LapplyState", backend = "local",
    freezerClass = "LapplyFreezer", ...,
    force_rerun = FALSE, sourceFiles = NULL,
    libraries = NULL, parallel_count = NULL,
    copy_environments = NULL)
}
\arguments{
  \item{Lapply_config}{A list describing possible
  configurations of the parallelization process. See
  Details.}

  \item{stateClass}{A class name representing
  parallelization states. Needs only be supplied if custom
  extensions have been made to the package.}

  \item{backend}{The name of the backend used. See Details
  and Examples.}

  \item{freezerClass}{The freezerClass used to store
  unevaluated calls that are to be executed in parallel.
  Needs only be supplied if custom extensions have been
  made to the package.}

  \item{\dots}{Extra arguments passed to the initializer of
  the stateClass.}

  \item{force_rerun}{So called offline computations are
  stateful. If a given rampUp has been completed an ensuing
  call - even a rerun of the script in a new R interpreter
  - reuses previous result. If set to TRUE force_rerun
  ignores previous results and recomputes the whole
  computation.}

  \item{sourceFiles}{Overwrite the \code{sourceFiles} entry
  in \code{Lapply_config}.}

  \item{parallel_count}{Overwrite the \code{parallel_count}
  entry in \code{Lapply_config}.}
}
\value{
  Value \code{NULL} is returned.
}
\description{
  Initialzes the parallelization process. The config
  argument describes all parameters for as many backends as
  are available. Remaining arguments select a configuration
  for the ensuing parallelization from that description.
}
\details{
  \code{config} is a list with the following elements
  \code{ config = list(max_depth = 5, parallel_count = 24,
  offline = TRUE, backends = list(...)  ); }
}
\examples{
config = list(max_depth = 5, parallel_count = 24, offline = TRUE, backends = list(
    snow = list(
      localNodes = 1, splitN = 1, sourceFiles = c('RgenericAll.R', 'Rgenetics.R', 'RlabParallel.R')
    ),
    local = list(
      path = sprintf('\%s/tmp/parallelize', tempdir())
    ),
    `ogs-1` = list(
      backend = 'OGS',
      freezerClass = 'LapplyPersistentFreezer',
      sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
      stateDir = sprintf('\%s/tmp/remote', tempdir()),
      qsubOptions = sprintf('--queue all.q --logLevel \%d', 2),
      doNotReschedulde = TRUE
    ),
    `ogs-2` = list(
      backend = 'OGS',
      freezerClass = 'LapplyPersistentFreezer',
      sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
      stateDir = sprintf('\%s/tmp/remote', tempdir()),
      qsubOptions = sprintf('--queue subordinate.q --logLevel \%d', 2),
      doSaveResult = TRUE
    ),
    `ogs-3` = list(
      backend = 'OGSremote',
      remote = 'user\\
}
\author{
  Stefan Böhringer <r-packages@s-boehringer.org>
}
\seealso{
  \code{\link{parallelize}}, \code{\link{parallelize_call}}
}
\keyword{~kwd1}
\keyword{~kwd2}

