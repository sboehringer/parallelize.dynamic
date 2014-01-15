\name{reshape.long}
\alias{reshape.long}
\title{Convert data in wide format to long format}
\usage{
  reshape.long(d, vars = NULL, factorColumn = "factor",
    valueColumn = "value", factors = as.factor(vars),
    useDisk = F)
}
\arguments{
  \item{d}{data frame with columns in wide format}

  \item{vars}{columns in wide format by name or index}

  \item{factors}{\code{vars} can be grouped. For each level
  of \code{factor} a new row is created. Implies that
  \code{length(vars)} is a multiple of
  \code{length(levels(factor))}}

  \item{factorColumn}{name of the column to be created for
  the factor}

  \item{valueColumn}{name of the new column of values that
  were in wide format}
}
\description{
  Long format duplicates certain columns and adds rows for
  which one new column hold values coming from a set of
  columns in wide format.
}
\examples{
#reshape variables 2:9 (forming two groups: case/ctr), value of which is named 'group'
	# the shortened columns will get names valueColumn
	d0 = reshape.long(d, vars = 2:9, factors = c('case', 'ctr'), factorColumn = 'group',
		valueColumn = c('AA', 'AG', 'GG', 'tot'));
}

