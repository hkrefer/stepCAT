\name{stepCAT.bank}
\alias{stepCAT.bank}
\title{Creates an item bank by name of parameters}
\usage{
  stepCAT.bank(items, ...)
}
\arguments{
  \item{items}{object: data frame by columns named like
  \emph{"a"}, \emph{"b"}, \emph{"c"} and \emph{"d"} for
  parameters.}

  \item{\dots}{arguments: to pass it on functions.}
}
\value{
  List of class \code{'itBank'}.

  See about values \code{itemPar}, \code{theta},
  \code{infoTab} and \code{cbGroup} on function
  \code{\link[catR]{createItemBank}}.
}
\description{
  The \code{stepCAT.bank} extracts elements from a data
  frame by name of columns (name of parameters) and creates
  an item bank by using function
  \code{\link[catR]{createItemBank}}.
}
\details{
  See about specification for data frame on function
  \code{\link{stepCAT.items}} in \code{items} of class
  \code{'data.frame'}.\cr See about arguments to pass it on
  function \code{\link[catR]{createItemBank}}.
}
\examples{
# stepCAT.bank example 1

}
\seealso{
  \code{\link{stepCAT.by}}, \code{\link{stepCAT.items}},
  \code{\link{stepCAT.table}}
}

