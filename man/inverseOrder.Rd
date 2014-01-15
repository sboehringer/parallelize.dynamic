\name{inverseOrder}
\alias{inverseOrder}
\usage{
  inverseOrder(p)
}
\examples{
v = runif(1e2);
print(all(sort(v)[inverseOrder(v)] == v))
}

