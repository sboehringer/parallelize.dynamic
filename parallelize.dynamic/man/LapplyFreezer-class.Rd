% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rparallel.R
\docType{class}
\name{LapplyFreezer-class}
\alias{LapplyFreezer-class}
\alias{LapplyFreezerClass}
\title{Class \code{"LapplyFreezer"}}
\description{
This class encapsulates storage of calls and their results. Interaction with
this is done from backends and subclassing is only required if a new storage
mechanism of unevaluated calls or results thereof is needed. The end user
does not interact with this class.
}

\section{Extends}{


All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}.
}

\examples{

showClass("LapplyFreezer")

}
\seealso{
\code{\link{LapplyPersistentFreezer-class}} %% ~~or
\code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\author{
Stefan Böhringer <r-packages@s-boehringer.org>
}
\keyword{classes}
