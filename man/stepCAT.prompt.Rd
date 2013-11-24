\name{stepCAT.prompt}
\alias{stepCAT.prompt}
\title{Enumerates the choices and reads a response}
\usage{
  stepCAT.prompt(choices = 5, choice.random = FALSE, ...)
}
\arguments{
  \item{choices}{integer: number of choices.}

  \item{choice.random}{logical: \code{TRUE} to randomly
  choose a response.}

  \item{\dots}{arguments: to pass it on functions.}
}
\value{
  {numeric: index of choice}
}
\description{
  Enumerates the choices and reads a response from user.
}
\details{
  Argument \code{choices} must be a positive integer or
  else default value is assumed.\cr If non-interactive
  session is in use \code{TRUE} value is assumed for
  \code{choice.random}.
}
\examples{
## stepCAT.prompt example 1

}
\seealso{
  \code{\link{stepCAT.by}}, \code{\link{stepCAT.choice}}
}

