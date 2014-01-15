\name{tempcodefile}
\alias{tempcodefile}
\title{Create a temporary file that contains the function definition of the
argument.}
\usage{
  tempcodefile(fcts)
}
\arguments{
  \item{fcts}{%% ~~Describe \code{fcts} here~~}
}
\value{
  Returns the path to the file written.
}
\description{
  Create a temporary file that contains the function
  definition of the argument so that this file can be
  sourced to re-instantiate the function.
}
\details{
  Create a temporary file that contains the function
  definition of the argument so that this file can be
  sourced to re-instantiate the function. The temporary
  file is written to the temporary folder of the current R
  session.
}
\examples{
# code to be parallelized
  parallel8 = function(e) log(1:e) \%*\% log(1:e);
  parallel2 = function(e) rep(e, e) \%*\% 1:e * 1:e;
  parallel1 = function(e) Lapply(rep(e, 15), parallel2);
  parallel0 = function() {
    r = sapply(Lapply(1:50, parallel1),
      function(e)sum(as.vector(unlist(e))));
    r0 = Lapply(1:49, parallel8);
    r
  }

  codeFile = tempcodefile(c(parallel0, parallel1, parallel2, parallel8));
  cat(readFile(codeFile));
}
\author{
  Stefan Böhringer <r-packages@s-boehringer.org>
}
\keyword{~kwd1}
\keyword{~kwd2}

