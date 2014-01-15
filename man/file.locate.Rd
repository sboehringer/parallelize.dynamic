\name{file.locate}
\alias{file.locate}
\title{Return absolute path for name searched in search-pathes}
\usage{
  file.locate(path, prefixes = NULL, normalize = T,
    as.dirs = T, force = F)
}
\arguments{
  \item{as.dirs}{assume that prefixes are pathes, i.e. a
  slash will be put between path and prefix}

  \item{force}{enforces that path and prefix are always
  joined, otherwise if path is absolute no prefixing is
  performed}
}
\description{
  Search for pathes.
}

