##
## Global variable for package wundr
##
wundr.options <- list(conds = list, repository = NULL, API.count = numeric)

#' Set the wundr package conds option
#'
#' @param value to assign
#' @return the changed value of the package global \code{wundr.options}
#' @export
#' @examples
#' \dontrun{
#'   setConds(data.frame)
#' }
#'
setConds <- function(conds) {
  wundr.options <- wundr.options
  wundr.options$conds <- conds
  assignInMyNamespace("wundr.options", wundr.options)
  wundr.options
}

#' Set the wundr maximum number of iterations option
#'
#' @param maxIter, the value to assign
#' @return the changed value of the package global \code{wundr.options}
#' @export
#' @examples
#' \dontrun{
#'   setRepository(NULL)
#' }
#'
setRepository <- function(repository) {
  wundr.options <- wundr.options
  wundr.options$repository <- repository
  assignInMyNamespace("wundr.options", wundr.options)
  wundr.options
}

#' Reset the global package variable \code{wundr.options}
#'
#' @return the default value value of wundr package global \code{wundr.options}
#' @export
#' @examples
#' \dontrun{
#'   setCount(numeric)
#' }
#'
setCount <- function(API.count) {
  wundr.options <- wundr.options
  wundr.options$API.count <- API.count
  assignInMyNamespace("wundr.options", wundr.options)
  wundr.options
}

#' Reset the global package variable \code{wundr.options}
#'
#' @return the default value value of wundr package global \code{wundr.options}
#' @export
#' @examples
#' \dontrun{
#'   resetOptions()
#' }
#'
resetOptions <- function() {
  assignInMyNamespace("wundr.options", list(conds = data.frame, repository = NULL, API.count = numeric))
  wundr.options
}
