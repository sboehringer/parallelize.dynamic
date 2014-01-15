\name{order_align_fromOrder}
\alias{order_align_fromOrder}
\title{Calculates \code{order_align}, assuming that the both arguments are already orders.
sapply(1:40, function(i){v = runif(1e2); v1 = sample(v, length(v)); all(v1[order_align_fromOrder(order(v), order(v1))] == v)})}
\usage{
  order_align_fromOrder(reference, v)
}
\description{
  Calculates \code{order_align}, assuming that the both
  arguments are already orders. sapply(1:40, function(i){v
  = runif(1e2); v1 = sample(v, length(v));
  all(v1[order_align_fromOrder(order(v), order(v1))] ==
  v)})
}

