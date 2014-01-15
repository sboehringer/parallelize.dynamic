\name{Log}
\alias{Log}
\alias{Log.level}
\alias{Log.setLevel}
\title{Log a message to stderr.}
\usage{
  Log(o, level = get("DefaultLogLevel", envir = Log_env__))
}
\arguments{
  \item{o}{Message to be printed.}

  \item{level}{If \code{Log.setLevel} was called with this
  value, subsequent calls to \code{Log} with values of
  \code{level} smaller or equal to this value will be
  printed.}
}
\description{
  Log a message to stderr. Indicate a logging level to
  control verbosity.
}
\details{
  This function prints a message to stderr if the condition
  is met that a global log-level is set to greater or equal
  the value indicated by \code{level}. \code{Log.level}
  returns the current logging level.
}
\examples{
Log.setLevel(4);
	Log('hello world', 4);
	Log.setLevel(3);
	Log('hello world', 4);
}
\author{
  Stefan Böhringer <r-packages@s-boehringer.org>
}
\seealso{
  \code{\link{Log.setLevel}}, ~~~
}
\keyword{~kwd1}
\keyword{~kwd2}

