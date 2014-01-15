\name{order_align}
\alias{order_align}
\title{Return vector that reorders v to equal reference.}
\usage{
  order_align(reference, v)
}
\description{
  Assuming that two arguments are permutaions of each
  other, return a vector of indeces such that
  \code{all(reference == v[order_align(reference, v)]) ==
  T} for all vectors \code{reference, v}.
}
\examples{
sapply(1:10, function(i){v = sample(1:5); v[order_align(5:1, v)]})
sapply(1:10, function(i){v = runif(1e2); v1 = sample(v, length(v)); all(v1[order_align(v, v1)] == v)})
}

