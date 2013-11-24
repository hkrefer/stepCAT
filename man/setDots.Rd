\name{setDots}
\alias{setDots}
\title{Sets "\dots" as formal arguments of any function}
\usage{
  setDots(fun)
}
\arguments{
  \item{fun}{function: object.}
}
\value{
  {function: object}
}
\description{
  Sets "\dots" (dots method) as formal arguments of any
  function.
}
\examples{
## setDots example 1

require(stepCAT)

fun.a <- function(a, ...) {
  fun.b <- setDots(fun.b)
  fun.c <- setDots(fun.c)
  # if has not formal three-dots on function fun.b or fun.c
  # throws 'Error in fun.b(a, ...) : unused argument'
  print(fun.b(a, ...))
  print(fun.c(a, ...))
}

fun.b <- function(b, b_ = NULL) { paste0(b_, b) }

fun.c <- function(c, c_ = NULL) { paste0(c_, c) }

fun.a(1:5, b_ = "b_", c_ = "c:")
}
\seealso{
  \code{\link[methods]{dotsMethods}}
}
\keyword{internal}

