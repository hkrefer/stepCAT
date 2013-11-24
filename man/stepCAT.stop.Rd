\name{stepCAT.stop}
\alias{stepCAT.stop}
\title{Checks the rule to stop an adaptive test}
\usage{
  stepCAT.stop(test = NULL, rule = "length", thr = 10, ...)
}
\arguments{
  \item{test}{object: values \code{items}, \code{param},
  \code{pattern}, \code{theta}, \code{SE} and \code{CI}
  from function \code{\link{stepCAT.test}}.}

  \item{rule}{character: rule (\emph{"length"},
  \emph{"precision"} or \emph{"classification"}).}

  \item{thr}{numeric: threshold for the rule.}

  \item{\dots}{arguments: to pass it on functions.}
}
\value{
  {logical: \code{TRUE} or \code{FALSE}}
}
\description{
  The \code{stepCAT.stop} checks if has been satisfied a
  rule to stop an adaptive test.
}
\details{
  The \code{rule} \emph{"length"} returns \code{TRUE} value
  if length of value \code{test$items} (administered items)
  is greater than or equal to argument \code{thr}
  (threshold) that must be a positive integer or else
  default value is assumed.\cr The \code{rule}
  \emph{"precision"} returns \code{TRUE} value if value
  \code{test$SE} (standard error of ability estimate) is
  less than or equal to argument \code{thr} (threshold).\cr
  The \code{rule} \emph{"classification"} returns
  \code{TRUE} value if value \code{test$CI} (confidence
  interval of ability estimate) is not contains argument
  \code{thr} (threshold).\cr Only if a rule has been
  satisfied \code{TRUE} value is returned.
}
\examples{
## stepCAT.stop example 1

}
\seealso{
  \code{\link{stepCAT.by}}
}

