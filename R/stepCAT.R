
# stepCAT -----------------------------------------------------------------

#' Common interface to build an adaptive test
#' 
#' The \code{stepCAT} provides a common interface to build an adaptive test, for a given calibrated item bank in a data table and by using some of functions in \pkg{catR} package, and to administers that test by one of options of Graphical User Interface.\cr \cr Use \code{library(help = 'stepCAT')} for information about the package.
#'
#' @author Henrique Krefer (UFPR, Brazil), Rhuan Gabriel C. de Lima (UFPR, Brazil)
#' @references David Magis and Gilles Raiche (2012). Random Generation of Response Patterns under Computerized Adaptive Testing with the R Package catR. Journal of Statistical Software, 48 (8), \url{http://www.jstatsoft.org/v48/i08/}.
#' @docType package
#' @name stepCAT
#'  
NULL

# stepCAT.by --------------------------------------------------------------

#' @rdname stepCAT.by
#' @name stepCAT.by
#' @export stepCAT.by
#' @title Common interface to build an adaptive test by arguments
#' @description The \code{stepCAT.by} provides a main interface to build an adaptive test, for a given calibrated item bank in a data table and by using some of functions in \pkg{catR} package, and to administers that test by one of options for Graphical User Interface.
#' 
#' @param table object: to use a method.
#' @param \dots arguments: to pass it on functions.
#'
#' @details The \code{stepCAT.by} has been split into five core steps to pass arguments on functions:
#' \describe{
#' \item{Item Table step}{
#' \describe{\item{\code{table} of class \code{'data.frame'} or class \code{'numeric'}}{See about arguments to pass it on function \code{\link{stepCAT.items}}.}}
#' \describe{\item{\code{table} of class \code{'character'}}{See about arguments to pass it on function \code{\link{stepCAT.table}}.}}
#' }
#' \item{Item Bank step}{See about arguments to pass it on function \code{\link{stepCAT.bank}.}}
#' \item{Start step}{See about arguments to pass it on function \code{\link{stepCAT.start}}.}
#' \item{Test step}{See about arguments to pass it on function \code{\link{stepCAT.test}}.}
#' \item{Final step}{See about arguments to pass it on function \code{\link{stepCAT.final}}.}
#' }
#' 
#' @return List of class \code{'stepCAT'}:
#' @return \item{start}{object: by elements used in start step.}
#' @return \item{start items}{integer: index of each item.}
#' @return \item{start par}{matrix: parameters of each item.}
#' @return \item{start par "a"}{numeric: discrimination parameter of each item.}
#' @return \item{start par "b"}{numeric: difficulty parameter of each item.}
#' @return \item{start par "c"}{numeric: pseudo-guessing parameter of each item.}
#' @return \item{start par "d"}{numeric: inattention parameter of each item.}
#' @return \item{start thStart}{numeric: sequence of ability values for first items.}
#' @return \item{start startSelect}{character: criterion for first items.}
#' @return \item{test}{object: by elements used in test step.}
#' @return \item{test items}{integer: index of each item.}
#' @return \item{test par}{matrix: parameters of each item.}
#' @return \item{test par "a"}{numeric: discrimination parameter of each item.}
#' @return \item{test par "b"}{numeric: difficulty parameter of each item.}
#' @return \item{test par "c"}{numeric: pseudo-guessing parameter of each item.}
#' @return \item{test par "d"}{numeric: inattention parameter of each item.}
#' @return \item{test x}{numeric: response pattern (0 or 1) of each item.}
#' @return \item{test theta}{numeric: ability estimate of each item.}
#' @return \item{test SE}{numeric: standard error of ability estimate of each item.}
#' @return \item{test CI}{numeric: confidence interval of ability estimate.}
#' 
#' @seealso \code{\link{stepCAT.bank}}, \code{\link{stepCAT.stop}}, \code{\link{stepCAT.choice}}
#' @example examples/stepCAT.by_1-Ex.R
#' 
stepCAT.by <- function(table, ...) {
  
  UseMethod("stepCAT.by", table)
}

#' @rdname stepCAT.by
#' @name stepCAT.by.data.frame
#'
#' @method stepCAT.by data.frame
#' @S3method stepCAT.by data.frame
#' 
stepCAT.by.data.frame <- function(table, ...) {
  
  library(catR)
  
  items <- stepCAT.items(items = table, ...)
  
  bank <- stepCAT.bank(items = items, ...)
  
  start <- stepCAT.start(itemBank = bank, ...)
  
  test <- stepCAT.test(items = items, itemBank = bank, item = start$items, ...)
  
  final <- stepCAT.final(test = test, ...)
  
  result <- list(start = start, test = test)
  
  class(result) <- "stepCAT"
  
  return(result)
}

#' @rdname stepCAT.by
#' @name stepCAT.by.character
#'
#' @method stepCAT.by character
#' @S3method stepCAT.by character
#' 
stepCAT.by.character <- function(table, ...) {
  
  items <- stepCAT.table(table = table, ...)
    
  return(stepCAT.by.data.frame(items, ...))
}

#' @rdname stepCAT.by
#' @name stepCAT.by.numeric
#'
#' @method stepCAT.by numeric
#' @S3method stepCAT.by numeric
#'  
stepCAT.by.numeric <- function(table, ...) {
  
  items <- stepCAT.items(items = table, ...)

  return(stepCAT.by.data.frame(items, ...))
}

# stepCAT.items -----------------------------------------------------------

#' @rdname stepCAT.items
#' @name stepCAT.items
#' @export stepCAT.items
#' @title Sets up an item table by name of parameters and type of values
#' @description The \code{stepCAT.items} sets up an item table by name of parameters and type of values or create it for a given number of items by using function \code{\link[catR]{createItemBank}}.
#'
#' @param items object: to use a method.
#' @param id logical: \code{TRUE} for an index of item in first column of object \code{items}.
#' @param choices integer: number of columns of object \code{items} for choices in the question of item.
#' @param \dots arguments: to pass it on functions.
#' @param model character: logistic IRT model to create an object \code{items}.
#'
#' @details The \code{stepCAT.items} as generic function has two methods:
#' \describe{
#' \item{\code{items} of class \code{'numeric'}}{argument \code{items} must be a positive integer or else default value is assumed to generate an item bank;\cr argument \code{choices} must be a positive integer or else default value is assumed to generate \code{NA} values;\cr see about argument \code{model} or arguments to pass it on function \code{\link[catR]{createItemBank}}.}
#' \item{\code{items} of class \code{'data.frame'}}{argument \code{items} must be a data frame by one row per item and at least 7 columns in order: discrimination \emph{"a"}, difficulty \emph{"b"}, pseudo-guessing \emph{"c"}, inattention \emph{"d"}, response \emph{"r"}, question \emph{"q"}, choices (one per column up to argument \code{choices}) \emph{"ch[1:n]"};\cr if \code{id} is \code{TRUE} the first column must be some index \emph{"id"};\cr in short, number of columns in data frame must be equal to described columns plus the choices, or else the function is halted by an error;\cr argument \code{choices} must be a positive integer or else default value is assumed.}
#' }
#'
#' @return Object of class \code{'data.frame'}:
#' @return \item{id}{numeric: index (optional by argument \code{id}).}
#' @return \item{a}{numeric: discrimination parameter.}
#' @return \item{b}{numeric: difficulty parameter.}
#' @return \item{c}{numeric: pseudo-guessing parameter.}
#' @return \item{d}{numeric: inattention parameter.}
#' @return \item{r}{numeric: response value.}
#' @return \item{q}{character: question text.}
#' @return \item{ch\emph{1}}{character: choice text \emph{1}.}
#' @return \item{ch\emph{:}}{character: choice text \emph{:} (variable by argument \code{choices}).}
#' @return \item{ch\emph{n}}{character: choice text \emph{n}.}
#' 
#' @seealso \code{\link{stepCAT.by}}, \code{\link{stepCAT.table}}, \code{\link{stepCAT.bank}}
#' @example examples/stepCAT.items_1-Ex.R
#'
stepCAT.items <- function(items, ...) {
  
  UseMethod("stepCAT.items", items)
}

#' @rdname stepCAT.items
#' @name stepCAT.items.numeric
#' 
#' @method stepCAT.items numeric
#' @S3method stepCAT.items numeric
#' 
stepCAT.items.numeric <- function(items = 50, id = FALSE, choices = 5, model = "3PL", ...) {
  
  createItemBank <- setDots(createItemBank)
  
  if (! isTRUE(all.equal(items, as.integer(items))) ||
        ! items > 0) items = 50
  
  if (! isTRUE(all.equal(choices, as.integer(choices))) || 
        ! choices > 0) choices = 5
  
  if (isTRUE(id))
    items <- data.frame(id = 1:items, 
                        createItemBank(items = items, 
                                       model = model, 
                                       ...)$itemPar, 
                        r = c(sample(1:choices, items, replace = TRUE)), 
                        q = rep(NA, items), 
                        ch = array(NA, c(items, choices)))
  else
    items <- data.frame(createItemBank(items = items, 
                                       model = model, 
                                       ...)$itemPar, 
                        r = c(sample(1:choices, items, replace = TRUE)), 
                        q = rep(NA, items), 
                        ch = array(NA, c(items, choices)))
    
  return(stepCAT.items(items = items, id = id, choices = choices, ...))
}

#' @rdname stepCAT.items
#' @name stepCAT.items.data.frame
#'
#' @method stepCAT.items data.frame
#' @S3method stepCAT.items data.frame
#' 
stepCAT.items.data.frame <- function(items, id = FALSE, choices = 5, ...) {

  columns <- ifelse(isTRUE(id), 7, 6)
  
  if (! isTRUE(all.equal(choices, as.integer(choices))) || 
        ! choices > 0) choices = 5
  
  stopifnot(is.data.frame(items), ncol(items) == (columns + choices))
  
  if (isTRUE(id))
    names(items) <- c("id", "a", "b", "c", "d", "r", "q", 
                      paste0("ch", 1:choices))
  else
    names(items) <- c("a", "b", "c", "d", "r", "q", 
                      paste0("ch", 1:choices))
  
  items[, c(1:(columns - 1))] <- sapply(items[, c(1:(columns - 1))], as.numeric)
  items[, c((columns):(columns + choices))] <- 
    sapply(items[, c((columns):(columns + choices))], as.character)
  
  return(items)
}

# stepCAT.table -----------------------------------------------------------

#' @rdname stepCAT.table
#' @name stepCAT.table
#' @export stepCAT.table
#' @title Reads from a data table and sets it up an item table
#' @description The \code{stepCAT.table} reads data from a table format and sets it up an item table by using function \code{\link{stepCAT.items}}.
#'
#' @param table character: name of data table which to be read from it.
#' @param DB character: to use a method.
#' @param id logical: \code{TRUE} for an index of item in first column of data from the \code{table}.
#' @param choices integer: number of columns of data from the \code{table} for choices in the question of item.
#' @param \dots arguments: to pass it on functions.
#' @param header logical: \code{TRUE} for parameter names in first row of data from the \code{table}.
#' @param sep character: separator for values on each row of data from the \code{table}.
#' @param encoding character: native encoding to handle encoded strings from the \code{table}.
#' @param tableWorkspaceID numeric: \strong{Concerto} Workspace ID where the \code{table} is located.
#' @param file.force logical: \code{TRUE} to force read data from the \code{table} just like from the method \code{file}.
#' 
#' @details The \code{stepCAT.table} as generic function has two methods:
#' \describe{
#' \item{\code{DB} of class \code{'file'}}{argument \code{choices} must be a positive integer or else default value is assumed;\cr see about optional arguments on function \code{\link[utils]{read.table}};\cr see about specification for data table on function \code{\link{stepCAT.items}} in \code{items} of class \code{'data.frame'}.}
#' \item{\code{DB} of class \code{'concerto'}}{argument \code{choices} must be a positive integer or else default value is assumed;\cr argument \code{tableWorkspaceID} must be a positive integer or else \code{NULL} value is assumed;\cr if argument \code{tableWorkspaceID} has \code{NULL} value the \strong{Concerto} session variable \code{concerto$workspaceID} is assumed;\cr see about specification for data table on function \code{\link{stepCAT.items}} in \code{items} of class \code{'data.frame'}.}
#' }
#'
#' @return Object of class \code{'data.frame'}:
#' @return \item{id}{numeric: index (optional value by argument \code{id}).}
#' @return \item{a}{numeric: discrimination parameter.}
#' @return \item{b}{numeric: difficulty parameter.}
#' @return \item{c}{numeric: pseudo-guessing parameter.}
#' @return \item{d}{numeric: inattention parameter.}
#' @return \item{r}{numeric: response value.}
#' @return \item{q}{character: question text.}
#' @return \item{ch\emph{1}}{character: choice text \emph{1}.}
#' @return \item{ch\emph{:}}{character: choice text \emph{:} (variable value by argument \code{choices}).}
#' @return \item{ch\emph{n}}{character: choice text \emph{n}.}
#'
#' @seealso \code{\link{stepCAT.by}}, \href{https://code.google.com/p/concerto-platform/}{Concerto Platform}
#' @example examples/stepCAT.table_1-Ex.R
#' 
stepCAT.table <- function(table, DB = "file", ...) {
  
  class(DB) <- DB

  UseMethod("stepCAT.table", DB)
}

#' @rdname stepCAT.table
#' @name stepCAT.table.file
#' 
#' @method stepCAT.table file
#' 
stepCAT.table.file <- function(table, id = FALSE, choices = 5, header = TRUE, sep = ";", encoding = "UTF-8", ...) {
  
  columns <- ifelse(isTRUE(id), 7, 6)
  
  if (! isTRUE(all.equal(choices, as.integer(choices))) || 
        ! choices > 0) choices = 5
  
  items <- read.table(table, header, sep, nrows = 1)
  
  stopifnot(ncol(items) == (columns + choices))
    
  items <- read.table(table, header = header, sep = sep, encoding = encoding)
  
  return(stepCAT.items(items, id = id, choices = choices, ...))
}

#' @rdname stepCAT.table
#' @name stepCAT.table.concerto
#'
#' @method stepCAT.table concerto
#' 
stepCAT.table.concerto <- function(table, id = FALSE, choices = 5, tableWorkspaceID = NULL, file.force = FALSE, ...) {
    
  stopifnot(isTRUE(file.force) || exists("concerto"))
  
  if(! exists("concerto"))
    concerto <- concerto.table.query <- NULL

  columns <- ifelse(isTRUE(id), 7, 6)
  
  if (! isTRUE(all.equal(choices, as.integer(choices))) || 
        ! choices > 0) choices = 5
  
  if (! isTRUE(all.equal(tableWorkspaceID, as.integer(tableWorkspaceID))) || 
        ! tableWorkspaceID > 0) tableWorkspaceID = NULL
  
  if (is.null(tableWorkspaceID))
    tableWorkspaceID <- ifelse(! is.null(concerto), 
                               concerto$workspaceID, 
                               "NULL")
  
  workspacePrefix <- ifelse(! is.null(concerto), 
                            concerto$workspacePrefix,
                            "concerto4_")

  SQL <- paste0("SELECT * FROM `", 
                paste0(workspacePrefix, tableWorkspaceID), "`.`", table, "`")
  
  if (! is.null(concerto) && ! isTRUE(file.force)) {
    
    items <- concerto.table.query(sql = paste(SQL, "LIMIT 1"))
    
    stopifnot(ncol(items) == (columns + choices))
    
    items <- concerto.table.query(sql = SQL)
  }
  else
    items <- stepCAT.table.file(table, id = id, choices = choices, ...)
  
  return(stepCAT.items(items, id = id, choices = choices, ...))
}

# stepCAT.bank ------------------------------------------------------------

#' @rdname stepCAT.bank
#' @name stepCAT.bank
#' @export stepCAT.bank
#' @title Creates an item bank by name of parameters
#' @description The \code{stepCAT.bank} extracts elements from a data frame by name of columns (name of parameters) and creates an item bank by using function \code{\link[catR]{createItemBank}}.
#' 
#' @param items object: data frame by columns named like \emph{"a"}, \emph{"b"}, \emph{"c"} and \emph{"d"} for parameters.
#' @param \dots arguments: to pass it on functions.
#' 
#' @details See about specification for data frame on function \code{\link{stepCAT.items}} in \code{items} of class \code{'data.frame'}.\cr See about arguments to pass it on function \code{\link[catR]{createItemBank}}.
#' 
#' @return List of class \code{'itBank'}.
#' @return See about values \code{itemPar}, \code{theta}, \code{infoTab} and \code{cbGroup} on function \code{\link[catR]{createItemBank}}.
#' 
#' @seealso \code{\link{stepCAT.by}}, \code{\link{stepCAT.items}}, \code{\link{stepCAT.table}}, \code{\link{stepCAT.start}}, \code{\link{stepCAT.test}}
#' @example examples/stepCAT.bank_1-Ex.R
#' 
stepCAT.bank <- function(items, ...) {
  
  createItemBank <- setDots(createItemBank)
  
  items <- items[, c("a", "b", "c", "d")]
  bank <- createItemBank(items = items, ...)
  
  return(bank)
}

# stepCAT.start -----------------------------------------------------------

#' @rdname stepCAT.start
#' @name stepCAT.start
#' @export stepCAT.start
#' @title Selects the first items for an adaptive test
#' @description The \code{stepCAT.start} selects the first items for an adaptive test from an item bank by using function \code{\link[catR]{startItems}}.
#'
#' @param itemBank object: item bank of class \code{'itBank'} from returned value of function \code{\link[catR]{createItemBank}}.
#' @param nrItems numeric: number of items to select from the \code{itemBank}.
#' @param startTheta numeric: initial ability level to select items from the \code{itemBank}.
#' @param halfRange numeric: half of range of initial ability level to select items from the \code{itemBank}.
#' @param startSelect character: criterion to select items from the \code{itemBank}.
#' @param seed numeric: fix the random selection by random seed number.
#' @param \dots arguments: to pass it on functions.
#'
#' @details Argument \code{nrItems} must be a positive integer or else default value is assumed and random selection is fixed by \code{seed} equal to 1.\cr Minimum value between argument \code{nrItems} and value \code{itemBank$itemPar} is assumed to avoid select items out of bound.\cr See about argument \code{startSelect} or arguments to pass it on function \code{\link[catR]{startItems}}.
#' 
#' @return Object of class \code{'list'}.
#' @return See about values \code{items}, \code{par}, \code{thStart} and \code{startSelect} on function \code{\link[catR]{startItems}}.
#' 
#' @seealso \code{\link{stepCAT.by}}, \code{\link{stepCAT.items}}, \code{\link{stepCAT.table}}, \code{\link{stepCAT.bank}}
#' @example examples/stepCAT.start_1-Ex.R
#' 
stepCAT.start <- function(itemBank, nrItems = 1, startTheta = 0, halfRange = 2, startSelect = "MFI", seed = NULL, ...) {
  
  startItems <- setDots(startItems)

  if (! isTRUE(all.equal(nrItems, as.integer(nrItems))) || 
        ! nrItems > 0) {
    seed = 1; nrItems = 1
  }
  
  nrItems = min(nrItems, length(itemBank$itemPar[,1]))
  
  start <- startItems(itemBank = itemBank, 
                      seed = seed,
                      nrItems = nrItems, theta = startTheta, halfRange = halfRange, 
                      startSelect = startSelect, 
                      ...)
  
  return(start)
}

# stepCAT.test ------------------------------------------------------------

#' @rdname stepCAT.test
#' @name stepCAT.test
#' @export stepCAT.test
#' @title Administers an adaptive test and computes its estimates
#' @description The \code{stepCAT.test} administers an adaptive test, from an item table and item bank, and computes the ability estimate and selects next items by using functions \code{\link[catR]{thetaEst}}, \code{\link[catR]{semTheta}}, \code{\link[catR]{nextItem}} and \code{\link{stepCAT.stop}}.
#'
#' @param items object: data frame by columns named like \emph{"a"}, \emph{"b"}, \emph{"c"} and \emph{"d"} for parameters and \emph{"r"}, \emph{"q"} and \emph{"ch[1:n]"} for content;
#' @param itemBank object: item bank of class \code{'itBank'} from returned values of function \code{\link[catR]{createItemBank}} (usually from object \code{items}).
#' @param item integer: index (as vector) of each item to start.
#' @param GUI character: user interface (\emph{"console"}, \emph{"concerto"} or \emph{"tcltk"}) to show the question and to choose a response for the item.
#' @param show.id logical: \code{TRUE} to show the index of item.
#' @param method character: method for ability estimator.
#' @param criterion character: criterion to select the next item.
#' @param rule character: rule (\emph{"length"}, \emph{"precision"} or \emph{"classification"}) to stop the test.
#' @param thr numeric: threshold for the rule to stop the test.
#' @param alpha numeric: significance level for the confidence interval of ability estimate.
#' @param out integer: index (as vector) of each item once previously administered.
#' @param x integer: response pattern (as vector of 0 or 1) of each item once previously administered.
#' @param test object: elements \code{items} (\code{out}), \code{x}, \code{theta}, \code{SE} and \code{CI} (as vectors) of each item once previously administered.
#' @param \dots arguments: to pass it on functions.
#'
#' @details See about argument \code{items} on function \code{\link{stepCAT.items}} in \code{items} of class \code{'data.frame'}.\cr
#' See about argument \code{itemBank} on function \code{\link{stepCAT.bank}}.\cr 
#' See about argument \code{item} on function \code{\link{stepCAT.start}}.\cr
#' See about argument \code{GUI} or arguments to pass it on function \code{\link{stepCAT.choice}}; if argument \code{show.id} is not \code{TRUE} or \code{FALSE} no index is shown.\cr
#' See about argument \code{method} or arguments to pass it on function \code{\link[catR]{thetaEst}}.\cr 
#' See about arguments \code{criterion}, \code{out}, \code{x} or other arguments to pass it on function \code{\link[catR]{nextItem}}; if length of argument \code{out} is equal to length of value \code{itemBank$itemPar} the test stops to avoid select items out of bound.\cr
#' See about arguments \code{rule} and \code{thr} on function \code{\link{stepCAT.stop}}.\cr
#' See about argument \code{test} on topic \strong{Value}.
#'
#' @return Object of class \code{'list'}:
#' @return \item{test}{object: by elements used in test.}
#' @return \item{test items}{integer: index of each item.}
#' @return \item{test par}{matrix: parameters of each item.}
#' @return \item{test par "a"}{numeric: discrimination parameter of each item.}
#' @return \item{test par "b"}{numeric: difficulty parameter of each item.}
#' @return \item{test par "c"}{numeric: pseudo-guessing parameter of each item.}
#' @return \item{test par "d"}{numeric: inattention parameter of each item.}
#' @return \item{test x}{numeric: response pattern (0 or 1) of each item.}
#' @return \item{test theta}{numeric: ability estimate of each item.}
#' @return \item{test SE}{numeric: standard error of ability estimate of each item.}
#' @return \item{test CI}{numeric: confidence interval of ability estimate.}
#' 
#' @seealso \code{\link{stepCAT.by}}, \code{\link{stepCAT.table}}, \code{\link{stepCAT.prompt}}
#' @example examples/stepCAT.test_1-Ex.R
#' 
stepCAT.test <- function(items, itemBank, item = 1, GUI = "console", show.id = FALSE, method = "BM", criterion = "MFI", rule = "length", thr = 10, alpha = 0.05, out = NULL, x = NULL, test = NULL, ...) {
  
  thetaEst <- setDots(thetaEst)
  semTheta <- setDots(semTheta)
  nextItem <- setDots(nextItem)

  est <- list(theta = NA, SE = NA, CI = c(NA, NA))
  
  i <- item[1]
  out <- c(out, i)
  it <- as.matrix(items[out, c("a", "b", "c", "d")])
  rownames(it) <- NULL
  
  id <- switch(as.character(show.id), "TRUE" = i, "FALSE" = length(out), NULL)
  choice <- stepCAT.choice(item = id, 
                           question =  items[i, "q"], 
                           choices = c(items[i, grepl("ch[0-9]", 
                                                      names(items))]),
                           GUI = GUI,
                           ...)
  x <- c(x, as.numeric(choice == items[i, "r"]))
  
  item <- item[-1]

  if (length(item) == 0) {
        
    est$theta <- thetaEst(it = it, x = x, 
                          method = method, 
                          ...)

    est$SE <- semTheta(est$theta, it = it, x = x, 
                       method = method, 
                       ...)
    
    est$CI <- c(est$theta - qnorm(1 - alpha / 2) * est$SE, 
                est$theta + qnorm(1 - alpha / 2) * est$SE)
  }
 
  test <- list(items = out, par = cbind(it), 
               x = x, 
               theta = c(test$theta, est$theta), 
               SE = c(test$SE, est$SE), CI = est$CI)
  
  if (length(out) == length(itemBank$itemPar[,1]))
    return(test)
  
  if (! is.na(est$theta)) {
    
    if (stepCAT.stop(test = test, 
                     rule = rule, thr = thr))
      return(test)

    item <- nextItem(itemBank, theta = est$theta, 
                     out = out, x = x,
                     criterion = criterion, 
                     method = method, 
                     ...)$item
  }
  
  return(
    stepCAT.test(items = items, itemBank = itemBank, item = item, 
                 GUI = GUI, show.id = show.id, 
                 method = method, 
                 criterion = criterion, 
                 rule = rule, thr = thr, alpha = alpha, 
                 out = out, x = x,
                 test = test, 
                 ...))
}

# stepCAT.stop ------------------------------------------------------------

#' @rdname stepCAT.stop
#' @name stepCAT.stop
#' @export stepCAT.stop
#' @title Checks the rule to stop an adaptive test
#' @description The \code{stepCAT.stop} checks if has been satisfied a rule to stop an adaptive test.
#'
#' @param test object: values \code{items}, \code{param}, \code{pattern}, \code{theta}, \code{SE} and \code{CI} from function \code{\link{stepCAT.test}}.
#' @param rule character: rule (\emph{"length"}, \emph{"precision"} or \emph{"classification"}).
#' @param thr numeric: threshold for the rule.
#' @param \dots arguments: to pass it on functions.
#'
#' @details The \code{rule} \emph{"length"} returns \code{TRUE} value if length of value \code{test$items} (administered items) is greater than or equal to argument \code{thr} (threshold) that must be a positive integer or else default value is assumed.\cr The \code{rule} \emph{"precision"} returns \code{TRUE} value if value \code{test$SE} (standard error of ability estimate) is less than or equal to argument \code{thr} (threshold).\cr The \code{rule} \emph{"classification"} returns \code{TRUE} value if value \code{test$CI} (confidence interval of ability estimate) is not contains argument \code{thr} (threshold).\cr Only if a rule has been satisfied \code{TRUE} value is returned.
#'
#' @return {logical: \code{TRUE} or \code{FALSE}}
#' 
#' @seealso \code{\link{stepCAT.by}}
#' @example examples/stepCAT.stop_1-Ex.R
#' 
stepCAT.stop <- function(test = NULL, rule = "length", thr = 10, ...) {

  rule = switch(rule, "length" = 1, "precision" = 2, "classification" = 3, 1)
  
  if (rule == 1) {
    
    if (! isTRUE(all.equal(thr, as.integer(thr))) ||
          thr < 1)
      thr = 10

    return(length(test$items) >= thr)
  }
    
  if (rule == 2)
    return(tail(test$SE, 1) <= thr)
  
  if (rule == 3)
    return(test$CI[1] >= thr || test$CI[2] <= thr)
}

# stepCAT.final -----------------------------------------------------------

#' @rdname stepCAT.final
#' 
#' @name stepCAT.final
#' @title Computes the final estimates for an adaptive test
#' @description The \code{stepCAT.final} Computes the final estimates for an adaptive test.
#'
#' @param test object: values \code{items}, \code{par}, \code{x}, \code{theta}, \code{SE} and \code{CI} from function \code{\link{stepCAT.test}}.
#' @param \dots arguments: to pass it on functions.
#' 
#' @details This version only returns as-is the object \code{test}.
#'
#' @return Object of class \code{'list'}.
#' 
#' @seealso \code{\link{stepCAT.by}}
#' @example examples/stepCAT.final_1-Ex.R
#' 
stepCAT.final <- function(test, ...) {

  return(test)
}

# stepCAT.choice ----------------------------------------------------------

#' @rdname stepCAT.choice
#' @name stepCAT.choice
#' @export stepCAT.choice
#' @title Shows the choices for the question and reads a response
#' @description The \code{stepCAT.choice} shows the choices for the question and reads a response from user.
#' 
#' @param GUI character: to use a method.
#' @param item numeric: index of item.
#' @param question character: description of the question.
#' @param choices character: description of each choice (indexed as vector).
#' @param \dots arguments: to pass it on functions.
#' @param templateID character: \strong{Concerto} Template ID (or name) to show.
#' @param templateWorkspaceID numeric: \strong{Concerto} Workspace ID where the \code{templateID} is located.
#' @param submit character: text to display on submit button.
#' @param console.force logical: \code{TRUE} to force read a response from console just like from the method \code{console}.
#'
#' @details The \code{stepCAT.choice} as generic function has three methods:
#' \describe{
#' \item{\code{GUI} of class \code{'console'}}{if argument \code{item} has \code{NULL} value it did not join with argument \code{question} to show;\cr argument \code{choices} must be a vector at least one element or else default value is assumed.}
#' \item{\code{GUI} of class \code{'concerto'}}{argument \code{choices} must be a positive integer or else default value is assumed;\cr if argument \code{templateID} has \code{NULL} value the \emph{HTML} code is generated and argument \code{templateWorkspaceID} is ignored;\cr argument \code{templateWorkspaceID} must be a positive integer or else \code{NULL} value is assumed;\cr if argument \code{templateWorkspaceID} has \code{NULL} value the \strong{Concerto} session variable \code{concerto$workspaceID} is assumed.}
#' \item{\code{GUI} of class \code{'tcltk'}}{if argument \code{item} has \code{NULL} value it is did not join with argument \code{question} to show; argument \code{choices} must be a vector at least one element or else default value is assumed.}
#' }
#'
#' @return {numeric: index of choice by response}
#'  
#' @seealso \code{\link{stepCAT.by}}, \href{https://code.google.com/p/concerto-platform/}{Concerto Platform}
#' @example examples/stepCAT.choice_1-Ex.R
#' 
stepCAT.choice <- function(GUI = "console", item = NULL, question = NA, choices = rep(NA, 5), ...) {

  class(GUI) <- GUI

  UseMethod("stepCAT.choice", GUI)
}

#' @rdname stepCAT.choice
#' @name stepCAT.choice.console
#'                                  
#' @method stepCAT.choice console
#' 
stepCAT.choice.console <- function(item = NULL, question = NA, choices = rep(NA, 5), ...) {
  
  if (! length(choices) > 0)
    choices = rep(NA, 5)
  
  cat(ifelse(is.null(item), 
             paste(question),
             paste(item, "-", question)), 
      fill = TRUE)
  
  for(i in 1:length(choices))
    cat(paste("(", i, ")", choices[i]), fill = TRUE)
  
  choice <- stepCAT.prompt(length(choices),
                           ...)

  return(as.numeric(choice))
}

#' @rdname stepCAT.choice
#' @name stepCAT.choice.concerto
#' 
#' @method stepCAT.choice concerto
#' 
stepCAT.choice.concerto <- function(item = NULL, question = NA, choices = rep(NA, 5), templateID = NULL, templateWorkspaceID = NULL, submit = "Submit", console.force = FALSE, ...) {
       
  stopifnot(isTRUE(console.force) || exists("concerto"))

  if(! exists("concerto"))
    concerto <- concerto.template.show <- NULL
  
  if (! length(choices) > 0)
    choices = rep(NA, 5)
  
  if (! isTRUE(all.equal(templateWorkspaceID, as.integer(templateWorkspaceID))) || 
        ! templateWorkspaceID > 0) templateWorkspaceID = NULL
  
  if (is.null(templateWorkspaceID))
    templateWorkspaceID <- ifelse(! is.null(concerto), 
                                  concerto$workspaceID, 
                                  "concerto$workspaceID")
  
  HTML <- ""
  if (is.null(templateID) || templateID == -1) {
    
    HTML <- paste0("<p>{{q}}</p>")
    
    for(i in 1:length(choices))
      HTML <- paste0(HTML, 
                     "<p><input name='choice' type='radio' value='", i, "' />", 
                     "{{", names(choices[i]), "}}</p>")
    
    HTML <- paste0(HTML, 
                   "<p><input name='submit' type='submit' value='", 
                   submit, "' /></p>")
  }
  
  params <- c(q = ifelse(is.null(item), 
                         paste0(question), 
                         paste(item, "-", question)), choices)

  if (! is.null(concerto) && ! isTRUE(console.force))
    choice <- concerto.template.show(HTML = HTML, 
                                     params = params, 
                                     workspaceID = templateWorkspaceID,
                                     templateID = templateID)$choice
  else {
    
    print(c(HTML = HTML, 
            params, 
            templateWorkspaceID = templateWorkspaceID, 
            templateID = templateID))
    
    choice <- stepCAT.prompt(length(choices), ...)
  } 
  
  if (length(choice) == 0)
    return(stepCAT.choice.concerto(item = item, 
                                   question = question,
                                   choices = choices,
                                   templateWorkspaceID = templateWorkspaceID,
                                   templateID = templateID,
                                   submit = submit,
                                   console.force = console.force,
                                   ...))
  
  return(as.numeric(choice))
}

#' @rdname stepCAT.choice
#' @name stepCAT.choice.tcltk
#'          
#' @method stepCAT.choice tcltk
#' 
stepCAT.choice.tcltk <- function(item = NULL, question = NA, choices = rep(NA, 5), ...) {

  library(tcltk)
  
  header <- ifelse(is.null(item), paste0(question), paste(item, "-", question))
  
  done <- tclVar(0)
  rbValue <- tclVar(0)
  
  tt <- tktoplevel()
  
  tkgrid(tklabel(tt, text=header), sticky="w")

  tkgrid(tklabel(tt,text=""))
    
  for (i in 1:length(choices)) {
    choice.label <- tklabel(tt, text=choices[[i]])
    choice.value <- tkradiobutton(tt)
    
    tkconfigure(choice.value, variable=rbValue, value=i)
    
    tkgrid(choice.label, choice.value)
    
    tkgrid.configure(choice.label, sticky="w")
  }
  tkgrid(tklabel(tt,text=""))
  tkgrid(tklabel(tt,text=""))
  
  OnOK <- function() {
    if (tclvalue(rbValue) > 0 ){
      tkdestroy(tt)
      tclvalue(done) <- 1
    }
  }
  
  OK.but <- tkbutton(tt, text="Submit", command=OnOK)
  
  tkgrid(OK.but)
  
  tkbind(tt, "<Destroy>", function() tclvalue(done) <- 1)
  tkfocus(tt)
  
  tkwait.variable(done)
  
  choice <- tclvalue(rbValue)

  return(as.numeric(choice))
}

# stepCAT.prompt ----------------------------------------------------------

#' @rdname stepCAT.prompt
#' @name stepCAT.prompt
#' @export stepCAT.prompt
#' @title Enumerates the choices and reads a response
#' @description Enumerates the choices and reads a response from user.
#'
#' @param choices integer: number of choices.
#' @param choice.random logical: \code{TRUE} to randomly choose a response.
#' @param \dots arguments: to pass it on functions.
#'
#' @details Argument \code{choices} must be a positive integer or else default value is assumed.\cr If non-interactive session is in use \code{TRUE} value is assumed for \code{choice.random}.
#'
#' @return {numeric: index of choice}
#' 
#' @seealso \code{\link{stepCAT.by}}, \code{\link{stepCAT.choice}}
#' @example examples/stepCAT.prompt_1-Ex.R
#' 
stepCAT.prompt <- function(choices = 5, choice.random = FALSE, ...) {
  
  if (! is.numeric(choices) || choices < 1)
    choices = 5

  if (! isTRUE(interactive())) choice.random = TRUE
  
  prompt <- paste0("(", paste0(1:choices, collapse = "|"), ") > ")
  regex <- paste0("^[1-", choices, "]$")
  
  if (isTRUE(choice.random)) {
    
    choice <- sample(1:choices, 1, replace = TRUE)
    writeLines(paste0(prompt, choice))
  } 
  else
    choice <- readline(prompt = prompt)
    
  ifelse(! grepl(regex, choice), 
         return(stepCAT.prompt(choices)), 
         return(as.numeric(choice)))
}

# setDots -----------------------------------------------------------------

#' @rdname setDots 
#' @name setDots
#' @export setDots
#' @title Sets "\dots" as formal arguments of any function
#' @description Sets "\dots" (dots method) as formal arguments of any function.
#'
#' @param fun function: object.
#'
#' @return {function: object}
#' 
#' @keywords internal
#' @seealso \code{\link[methods]{dotsMethods}}
#' @example examples/setDots_1-Ex.R
#' 
setDots <- function(fun) {
  
  if (is.null(formals(fun)$...))
    formals(fun) <- c(formals(fun), alist(... =))
  
  return(fun)
}
