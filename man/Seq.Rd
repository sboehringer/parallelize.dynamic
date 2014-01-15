\name{Seq}
\alias{Seq}
\title{Produce constrained sequences}
\usage{
  Seq(from, to, ..., ascending = T,
    descending = !ascending)
}
\arguments{
  \item{ascending}{restrict sequences to be ascending;
  return empty list if to < from}

  \item{descending}{restrict sequences to be descending;
  return empty list if from < to}
}
\description{
  This is a wrapper around seq that adds constraints.
  Setting ascending, descending to NA reverts to standard
  \code{seq} behaviour.
}
\examples{
Seq(1, 10, ascending = T)
Seq(1, 10, descending = T)
Seq(10, 1, ascending = NA)
}

