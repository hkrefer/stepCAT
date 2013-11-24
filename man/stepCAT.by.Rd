\name{stepCAT.by}
\alias{stepCAT.by}
\alias{stepCAT.by.character}
\alias{stepCAT.by.data.frame}
\alias{stepCAT.by.numeric}
\title{Common interface to build an adaptive test by arguments}
\usage{
  stepCAT.by(table, ...)

  \method{stepCAT.by}{data.frame} (table, ...)

  \method{stepCAT.by}{character} (table, ...)

  \method{stepCAT.by}{numeric} (table, ...)
}
\arguments{
  \item{table}{object: to use a method.}

  \item{\dots}{arguments: to pass it on functions.}
}
\value{
  List of class \code{'stepCAT'}:

  \item{start}{object: by elements used in start step.}

  \item{start items}{integer: index of each item.}

  \item{start par}{matrix: parameters of each item.}

  \item{start par "a"}{numeric: discrimination parameter of
  each item.}

  \item{start par "b"}{numeric: difficulty parameter of
  each item.}

  \item{start par "c"}{numeric: pseudo-guessing parameter
  of each item.}

  \item{start par "d"}{numeric: inattention parameter of
  each item.}

  \item{start thStart}{numeric: sequence of ability values
  for first items.}

  \item{start startSelect}{character: criterion for first
  items.}

  \item{test}{object: by elements used in test step.}

  \item{test items}{integer: index of each item.}

  \item{test par}{matrix: parameters of each item.}

  \item{test par "a"}{numeric: discrimination parameter of
  each item.}

  \item{test par "b"}{numeric: difficulty parameter of each
  item.}

  \item{test par "c"}{numeric: pseudo-guessing parameter of
  each item.}

  \item{test par "d"}{numeric: inattention parameter of
  each item.}

  \item{test x}{numeric: response pattern (0 or 1) of each
  item.}

  \item{test theta}{numeric: ability estimate of each
  item.}

  \item{test SE}{numeric: standard error of ability
  estimate of each item.}

  \item{test CI}{numeric: confidence interval of ability
  estimate.}
}
\description{
  The \code{stepCAT.by} provides a main interface to build
  an adaptive test, for a given calibrated item bank in a
  data table and by using some of functions in \pkg{catR}
  package, and to administers that test by one of options
  for Graphical User Interface.
}
\details{
  The \code{stepCAT.by} has been split into five core steps
  to pass arguments on functions: \describe{ \item{Item
  Table step}{ \describe{\item{\code{table} of class
  \code{'data.frame'} or class \code{'numeric'}}{See about
  arguments to pass it on function
  \code{\link{stepCAT.items}}.}}
  \describe{\item{\code{table} of class
  \code{'character'}}{See about arguments to pass it on
  function \code{\link{stepCAT.table}}.}} } \item{Item Bank
  step}{See about arguments to pass it on function
  \code{\link{stepCAT.bank}.}} \item{Start step}{See about
  arguments to pass it on function
  \code{\link{stepCAT.start}}.} \item{Test step}{See about
  arguments to pass it on function
  \code{\link{stepCAT.test}}.} \item{Final step}{See about
  arguments to pass it on function
  \code{\link{stepCAT.final}}.} }
}
\examples{
# stepCAT.by example 1

}
\seealso{
  \code{\link{stepCAT.bank}}, \code{\link{stepCAT.stop}},
  \code{\link{stepCAT.choice}}
}

