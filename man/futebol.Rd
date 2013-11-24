\name{futebol}
\alias{futebol}
\docType{data}
\title{
Item table to test knowledge about clubs and soccer teams
}
\description{
The \code{futebol} has origin as an activity in the discipline of Item Response Theory provided by professor Adilson dos Anjos during the 1st half of 2013 in course of Statistics at UFPR - Universidade Federal do Parana. The items have been calibrated under the 3PL model respectively holding the discrimination, difficulty, pseudo-guessing and inattention (fixed to one) parameters.
}
\usage{data(futebol)}
\format{
  A data frame with 63 observations on the following 12 variables:
  \describe{
    \item{\code{id}}{numeric vector: index.}
    \item{\code{a}}{numeric vector: discrimination parameter.}
    \item{\code{b}}{numeric vector: difficulty parameter.}
    \item{\code{c}}{numeric vector: pseudo-guessing parameter.}
    \item{\code{d}}{numeric vector: inattention parameter.}
    \item{\code{response}}{numeric vector: response value.}
    \item{\code{question}}{character vector: question text.}
    \item{\code{choice1}}{character vector: choice text 1.}
    \item{\code{choice2}}{character vector: choice text 2.}
    \item{\code{choice3}}{character vector: choice text 3.}
    \item{\code{choice4}}{character vector: choice text 4.}
    \item{\code{choice5}}{character vector: choice text 5.}
  }
}
\examples{
data(futebol)
## maybe str(futebol) ; plot(futebol) ...
}
\keyword{datasets}
