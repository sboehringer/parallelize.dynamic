% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rparallel.back.ogsremote.R
\docType{class}
\name{ParallelizeBackendOGSremote-class}
\alias{ParallelizeBackendOGSremote-class}
\alias{getResult,ParallelizeBackendOGSremote-method}
\alias{initialize,ParallelizeBackendOGSremote-method}
\alias{initScheduling,ParallelizeBackendOGSremote-method}
\alias{lapply_dispatchFinalize,ParallelizeBackendOGSremote-method}
\alias{performParallelizationStep,ParallelizeBackendOGSremote-method}
\alias{pollParallelization,ParallelizeBackendOGSremote-method}
\title{Class \code{"ParallelizeBackendOGSremote"}}
\description{
Backend class supporting Open Grid Scheduler use from remote machines
}
\section{Objects from the Class}{

Objects can be created by calls of the form
\code{new("ParallelizeBackendOGSremote", config, ...)}.
During normal operation you do not have to create objects of this class yourself. Instead, \code{parallelize_initialize} will create such instances for you. The class can be configured with the following field in the \code{Lapply_config} argument of \code{parallelize_initialize}.
\itemize{
  \item freezerClass: defaults to \code{LapplyPersistentFreezer}. It is recommended to use \code{LapplyGroupingFreezer} for this backend as it is the most efficient freezer. Currently, \code{LapplyGroupingFreezer} is only supported for this backend.
  \item stateDir: directory to store results from computations. This location is passed to \code{LapplyPersistentFreezer}. If temporary behavior is desired it can be set to: \code{sprintf('\%s/tmp/remote', tempdir())}.
   \item sourceFiles: a vector of files to be sourced prior to parallel execution
   \item libraries: a vector of package names to be loaded prior to parallel execution
   \item remote: a scp path to a folder on the server that can be used to store temporary files, e.g. 'user@localhost:tmp/remote/test'. A unique subfolder per computation is created within this folder to store files (unique tempfolder).
    \item qsubOptions: extra options that are passed to the \code{qsub.pl} utility included in the package that is used to submit jobs. Execute \code{./qsub.pl --help} in the \code{inst/Perl} folder of the package to see all options and examples. Important options include \code{--queue} to specify the queue, \code{--memory} to set an upper bound for the needed memory (.e.g. \code{--memory 4G}) and \code{--logLevel} to set verbosity of output (level 5 produces detailed output).
}
To use this backend you have to have access password-less ssh access to a linux server running the Open Grid Scheduler (OGS) or the Sun Grid engine (SGE). You can install OGS locally (see \link{http://gridscheduler.sourceforge.net/CompileGridEngineSource.html}). \code{ssh} and \code{scp} have to be installed on the local machine.
Job output (stdout, stderr) as well as \code{qsub.pl} output is stored in subfolder of the unique tempfolder starting with 'qsubOutput'.
}

\examples{

showClass("ParallelizeBackendOGSremote")

}
\seealso{
%% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
\code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\linkS4class{ParallelizeBackend}},
\code{\linkS4class{ParallelizeBackendLocal}},
\code{\linkS4class{ParallelizeBackendSnow}},
\code{\linkS4class{ParallelizeBackendOGSremote}}
}
\author{
Stefan Böhringer <r-packages@s-boehringer.org>
}
\keyword{classes}
