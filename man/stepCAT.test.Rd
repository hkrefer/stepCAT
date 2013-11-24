\name{stepCAT.test}
\alias{stepCAT.test}
\title{Administers an adaptive test and computes its estimates}
\usage{
  stepCAT.test(items, itemBank, item = 1, GUI = "console",
    show.id = FALSE, method = "BM", criterion = "MFI",
    rule = "length", thr = 10, alpha = 0.05, out = NULL,
    x = NULL, test = NULL, ...)
}
\arguments{
  \item{items}{object: data frame by columns named like
  \emph{"a"}, \emph{"b"}, \emph{"c"} and \emph{"d"} for
  parameters and \emph{"r"}, \emph{"q"} and
  \emph{"ch[1:n]"} for content;}

  \item{itemBank}{object: item bank of class
  \code{'itBank'} from returned values of function
  \code{\link[catR]{createItemBank}} (usually from object
  \code{items}).}

  \item{item}{integer: index (as vector) of each item to
  start.}

  \item{GUI}{character: user interface (\emph{"console"},
  \emph{"concerto"} or \emph{"tcltk"}) to show the question
  and to choose a response for the item.}

  \item{show.id}{logical: \code{TRUE} to show the index of
  item.}

  \item{method}{character: method for ability estimator.}

  \item{criterion}{character: criterion to select the next
  item.}

  \item{rule}{character: rule (\emph{"length"},
  \emph{"precision"} or \emph{"classification"}) to stop
  the test.}

  \item{thr}{numeric: threshold for the rule to stop the
  test.}

  \item{alpha}{numeric: significance level for the
  confidence interval of ability estimate.}

  \item{out}{integer: index (as vector) of each item once
  previously administered.}

  \item{x}{integer: response pattern (as vector of 0 or 1)
  of each item once previously administered.}

  \item{test}{object: elements \code{items} (\code{out}),
  \code{x}, \code{theta}, \code{SE} and \code{CI} (as
  vectors) of each item once previously administered.}

  \item{\dots}{arguments: to pass it on functions.}
}
\value{
  Object of class \code{'list'}:

  \item{test}{object: by elements used in test.}

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
  The \code{stepCAT.test} administers an adaptive test,
  from an item table and item bank, and computes the
  ability estimate and selects next items by using
  functions \code{\link[catR]{thetaEst}},
  \code{\link[catR]{semTheta}}, \code{\link{stepCAT.stop}}
  and \code{\link[catR]{nextItem}}.
}
\details{
  See about argument \code{items} on function
  \code{\link{stepCAT.items}} in \code{items} of class
  \code{'data.frame'}.\cr See about argument
  \code{itemBank} on function
  \code{\link{stepCAT.bank}}.\cr See about argument
  \code{item} on function \code{\link{stepCAT.start}}.\cr
  See about argument \code{GUI} or arguments to pass it on
  function \code{\link{stepCAT.choice}}; if argument
  \code{show.id} is not \code{TRUE} or \code{FALSE} no
  index is shown.\cr See about argument \code{method} or
  arguments to pass it on function
  \code{\link[catR]{thetaEst}}.\cr See about arguments
  \code{criterion}, \code{out}, \code{x} or other arguments
  to pass it on function \code{\link[catR]{nextItem}}; if
  length of argument \code{out} is equal to length of value
  \code{itemBank$itemPar} the test stops to avoid select
  items out of bound.\cr See about arguments \code{rule}
  and \code{thr} on function \code{\link{stepCAT.stop}}.\cr
  See about argument \code{test} on topic \strong{Value}.
}
\examples{
## stepCAT.test example 1

}
\seealso{
  \code{\link{stepCAT.by}}, \code{\link{stepCAT.table}},
  \code{\link[catR]{semTheta}}
}

