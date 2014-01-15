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
  \code{Lapply_config} is a list with the following
  elements \itemize{ \item max_depth: maximal depth to
  investigate during probing \item parallel_count: provide
  default for the number of parallel jobs to generate,
  overwritten by the function argument \item offline: this
  option determines whether parallelize returns before
  performing the ramp-down. This is relevant for backends
  running on remote machines. See especially the
  \code{OGSremote} backend.  \item backends: a list that
  contains parameters specific for backends. The name of
  each element should be the name of a backend without the
  prefix \code{ParallelizeBackend}. Each of the elements is
  itself a list with the paramters. If several
  configurations are required for a certain backend - e.g.
  a batch-queuing system for which different queues are to
  be used - the name can be chosen arbitrarily. Then the
  element must contain an element with name \code{backend}
  that specifies the backend as above (example below). For
  the backend specific parameters see the class
  documentation of the backends.  }
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
      remote = 'user@localhost:tmp/remote/test',
      freezerClass = 'LapplyPersistentFreezer',
      sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
      stateDir = sprintf('\%s/tmp/remote/test_local', tempdir()),
      qsubOptions = sprintf('--queue all.q --logLevel \%d', 2),
      doSaveResult = TRUE
    )
  ));
  # run ensuing parallelizations locally, ignore result produced earlier
  parallelize_initialize(config, backend = "local", force_rerun = FALSE);
  # run ensuing parallelizations on the snow cluster defined in the snow backend section
  parallelize_initialize(config, backend = "local");
  # run ensuing parallelizations on a local Open Grid Scheduler
  parallelize_initialize(config, backend = "ogs-1");
  # run same analysis as above with different scheduling options
  parallelize_initialize(config, backend = "ogs-2");
  # run same analysis on a remote Opend Grid Scheduler
  # user 'user' on machine 'localhost' is used
  parallelize_initialize(config, backend = "ogs-3");
}
\author{
  Stefan Böhringer <r-packages@s-boehringer.org>
}
\seealso{
  \code{\link{parallelize}},
  \code{\link{parallelize_call}},
  \code{\linkS4class{ParallelizeBackend}},
  \code{\linkS4class{ParallelizeBackendLocal}},
  \code{\linkS4class{ParallelizeBackendSnow}},
  \code{\linkS4class{ParallelizeBackendOGSremote}}
}

