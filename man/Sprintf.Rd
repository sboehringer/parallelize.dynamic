\name{Sprintf}
\alias{Sprintf}
\title{Produce string by substituting placeholders}
\usage{
  Sprintf(fmt, ...)
}
\arguments{
  \item{s}{template string}

  \item{d}{values to substitute into \code{s}}

  \item{template}{template for substitution pattern. Within
  this pattern \code{__DICT_KEY__} is substituted for a key
  in \code{d}. This string \code{k} is substituted in
  \code{s} with \code{d[[k]]}.}
}
\description{
  The function behaves similar to sprintf, except that
  character sequences to be substituted are indicated by
  name. To be implemented: *-specifications, vectorization
}
\examples{
Sprintf('These are N \%{N} characters.', list(N = 10));
}

