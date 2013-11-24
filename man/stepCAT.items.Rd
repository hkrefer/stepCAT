\name{stepCAT.items}
\alias{stepCAT.items}
\alias{stepCAT.items.data.frame}
\alias{stepCAT.items.numeric}
\title{Sets up an item table by name of parameters and type of values}
\usage{
  stepCAT.items(items, ...)

  \method{stepCAT.items}{numeric} (items = 50, id = FALSE,
    choices = 5, model = "3PL", ...)

  \method{stepCAT.items}{data.frame} (items, id = FALSE,
    choices = 5, ...)
}
\arguments{
  \item{items}{object: to use a method.}

  \item{id}{logical: \code{TRUE} for an index of item in
  first column of object \code{items}.}

  \item{choices}{integer: number of columns of object
  \code{items} for choices in the question of item.}

  \item{\dots}{arguments: to pass it on functions.}

  \item{model}{character: logistic IRT model to create an
  object \code{items}.}
}
\value{
  Object of class \code{'data.frame'}:

  \item{id}{numeric: index (optional by argument
  \code{id}).}

  \item{a}{numeric: discrimination parameter.}

  \item{b}{numeric: difficulty parameter.}

  \item{c}{numeric: pseudo-guessing parameter.}

  \item{d}{numeric: inattention parameter.}

  \item{r}{numeric: response value.}

  \item{q}{character: question text.}

  \item{ch\emph{1}}{character: choice text \emph{1}.}

  \item{ch\emph{:}}{character: choice text \emph{:}
  (variable by argument \code{choices}).}

  \item{ch\emph{n}}{character: choice text \emph{n}.}
}
\description{
  The \code{stepCAT.items} sets up an item table by name of
  parameters and type of values or create it for a given
  number of items by using function
  \code{\link[catR]{createItemBank}}.
}
\details{
  The \code{stepCAT.items} as generic function has two
  methods: \describe{ \item{\code{items} of class
  \code{'numeric'}}{argument \code{items} must be a
  positive integer or else default value is assumed to
  generate an item bank;\cr argument \code{choices} must be
  a positive integer or else default value is assumed to
  generate \code{NA} values;\cr see about argument
  \code{model} or arguments to pass it on function
  \code{\link[catR]{createItemBank}}.} \item{\code{items}
  of class \code{'data.frame'}}{argument \code{items} must
  be a data frame by one row per item and at least 7
  columns in order: discrimination \emph{"a"}, difficulty
  \emph{"b"}, pseudo-guessing \emph{"c"}, inattention
  \emph{"d"}, response \emph{"r"}, question \emph{"q"},
  choices (one per column up to argument \code{choices})
  \emph{"ch[1:n]"};\cr if \code{id} is \code{TRUE} the
  first column must be some index \emph{"id"};\cr in short,
  number of columns in data frame must be equal to
  described columns plus the choices, or else the function
  is halted by an error;\cr argument \code{choices} must be
  a positive integer or else default value is assumed.} }
}
\examples{
## stepCAT.items example 1

}
\seealso{
  \code{\link{stepCAT.by}}, \code{\link{stepCAT.table}},
  \code{\link{stepCAT.bank}}
}

