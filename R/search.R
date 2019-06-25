#' Search Prevedere
#'
#' Send an arbitrary query to the Prevedere API.
#'
#' @param query A query to send to Prevedere. This should be a named list.
#'
#' @return The result of the search.
#' @seealso [Prevedere.api.fetch]
#' @export
Prevedere.api.search <- function(query) {
  path <- "/search"
  payload <- query

  Prevedere.api.fetch(path,payload)
}