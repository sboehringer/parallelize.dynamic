\name{Substr}
\alias{Substr}
\title{Return sub-strings indicated by positions or produce a string by substituting those strings with
	replacements}
\usage{
  Substr(s, start, length, replacement)
}
\arguments{
  \item{s}{template string}

  \item{start}{vector of start positions of substrings to
  substitute}

  \item{length}{vector of lengthes of substrings to
  substitute}

  \item{replacement}{vector of strings to subsitute. If
  missing, \code{Substr} returns sub-strings indicated by
  start/length}
}
\description{
  The function behaves similar to sprintf, except that
  character sequences to be substituted are indicated by
  name.
}
\examples{
print(Substr("abc", c(2, 3), c(1, 1), c("def", 'jkl')));
print(Substr("abcdef", c(2, 3, 5), c(1, 1, 1), c("123", '456', '789')));
print(Substr("abcdef", c(1, 3, 5), c(1, 1, 1), c("123", '456', '789')));
print(Substr("abcdef", c(1, 3, 5), c(0, 1, 0), c("123", '456', '789')));
}

