\name{stepCAT.start}
\alias{stepCAT.start}
\title{Selects the first items for an adaptive test}
\usage{
  stepCAT.start(itemBank, nrItems = 1, theta = 0,
    halfRange = 2, startSelect = "MFI", seed = NULL, ...)
}
\arguments{
  \item{itemBank}{object: item bank of class
  \code{'itBank'} from returned value of function
  \code{\link[catR]{createItemBank}}.}

  \item{nrItems}{numeric: number of items to select from
  the \code{itemBank}.}

  \item{theta}{numeric: initial ability level to select
  items from the \code{itemBank}.}

  \item{halfRange}{numeric: half of range of initial
  ability level to select items from the \code{itemBank}.}

  \item{startSelect}{character: criterion to select items
  from the \code{itemBank}.}

  \item{seed}{numeric: fix the random selection by random
  seed number.}

  \item{\dots}{arguments: to pass it on functions.}
}
\value{
  Object of class \code{'list'}.

  See about values \code{items}, \code{par}, \code{thStart}
  and \code{startSelect} on function
  \code{\link[catR]{startItems}}.
}
\description{
  The \code{stepCAT.start} selects the first items for an
  adaptive test from an item bank by using function
  \code{\link[catR]{startItems}}.
}
\details{
  Argument \code{nrItems} must be a positive integer or
  else default value is assumed and random selection is
  fixed by \code{seed} equal to 1.\cr Minimum value between
  argument \code{nrItems} and value \code{itemBank$itemPar}
  is assumed to avoid select items out of bound.\cr See
  about argument \code{startSelect} or arguments to pass it
  on function \code{\link[catR]{startItems}}.
}
\examples{
## stepCAT.start example 1

}
\seealso{
  \code{\link{stepCAT.by}}, \code{\link{stepCAT.items}},
  \code{\link{stepCAT.table}}, \code{\link{stepCAT.bank}}
}

