\name{subListFromRaggedLists}
\alias{subListFromRaggedLists}
\title{Extract parts of nested lists based on the range from..to}
\usage{
  subListFromRaggedLists(ls, from = 1,
    to = sum(sapply(ls, length)))
}
\arguments{
  \item{ls}{nested list structure (currently only two
  levels supported)}
}
\value{
  Return list of list, where each basic list contains key
  \code{segment} (which of the elements of Ns) and key
  \code{range}, a list with elements \code{from} and
  \code{to}, specifying which elements to use from that
  segment.
}
\description{
  Extract parts of nested lists based on the range from..to
}

