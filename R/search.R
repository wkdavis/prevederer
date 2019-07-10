#' Search Prevedere
#'
#' Send an arbitrary query to the Prevedere API.
#'
#' @inheritParams prevedere_fetch
#' @param query A query to send to Prevedere. This should be a named list.
#'
#' @return The result of the search.
#' @seealso [prevedere_fetch]
#' @export
prevedere_search <- function(key,query) {
  path <- "/search"
  payload <- query

  prevedere_fetch(key,path,payload)
}