#' Search Prevedere
#'
#' Send an arbitrary query to the Prevedere API.
#'
#' @param query A query to send to Prevedere. This should be a named list.
#'
#' @return The result of the search.
#' @seealso [prevedere.api.fetch]
#' @export
prevedere.api.search <- function(query) {
  path <- "/search"
  payload <- query

  prevedere.api.fetch(path,payload)
}