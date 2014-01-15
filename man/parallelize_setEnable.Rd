\name{parallelize_setEnable}
\alias{parallelize_setEnable}
\title{Turn on/off the parallelization mechanism}
\usage{
  parallelize_setEnable(state)
}
\arguments{
  \item{state}{%% ~~Describe \code{state} here~~}
}
\value{
  The returned value is undefined.
}
\description{
  This function changes the definition of
  Apply/Lapply/Sapply/parallelize/parallelize_call to turn
  the parallelization mechanism on or off.
}
\details{
  This function changes the definition of
  Apply/Lapply/Sapply/parallelize/parallelize_call to turn
  the parallelization mechanism on or off. The global
  environment is modified by a call to this function.
}
\examples{
parallelize_setEnable(FALSE);
	Lapply
	parallelize_setEnable(TRUE);
	Lapply
}
\author{
  Stefan Böhringer <r-packages@s-boehringer.org>
}
\seealso{
  \code{\link{parallelize}},
  \code{\link{parallelize_call}},
  \code{\link{parallelize_initialize}}
}
\keyword{~kwd1}
\keyword{~kwd2}

