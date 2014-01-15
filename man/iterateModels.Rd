\name{iterateModels}
\alias{iterateModels}
\title{Iterate combinations of parameters}
\usage{
  iterateModels(modelList, f = function(...) list(...),
    ..., .constraint = NULL, .clRunLocal = T,
    .resultsOnly = F, .unlist = 0, lapply__ = Lapply,
    callWithList = F, symbolizer = NULL, restrictArgs = T,
    selectIdcs = NULL)
}
\arguments{
  \item{selectIdcs}{restrict models to the given indeces}
}
\description{
  This function takes a list of parameters for which
  several values are to be evaluated. These values can be
  vectors of numbers or lists that contain blocks of
  parameters. All combinations are formed and passed to a
  user supplied function \code{f}. This functions takes an
  index of the combination together with parameter values.
  Argument \code{callWithList} controls whether there is
  exactly one argument per parameter position or wether one
  more step of unlisting takes place. In case that a block
  of parameters is supplied, all values of the block are
  passed as individual arguments to \code{f} in case
  \code{callWithList == F}.
}
\examples{
modelList = list(global = list(list(a=1, b=2)), N = c(1, 2, 3));
print(iterateModels(modelList));
modelList = list(N = c(1, 2, 3), parsAsBlock = list(list(list(c = 1, d = 2)), list(list(c = 3, d = 4))));
print(iterateModels(modelList));
}

