#' Aggregation methods
#'
#' Retrieve a list of the aggregation methods currently supported by the
#' Prevedere API.
#'
#' @inheritParams prevedere_fetch
#'
#' @return A character vector of aggregation methods.
#'
#' @family enumeration functions
#' @export
#'
#' @examples
#' \dontrun{
#' key <- "1235467abcdefg"
#' prevedere_aggregations(key)
#' }
prevedere_aggregations <- function(key) {
  path <- "aggregationmethods"
  result <- prevedere_fetch(key, path)
  unlist(result)
}

#' Calculations
#'
#' Retrieve a list of the calculations currently supported by the
#' Prevedere API.
#'
#' @inheritParams prevedere_fetch
#'
#' @return A character vector of calculations.
#'
#' @family enumeration functions
#' @export
#'
#' @examples
#' \dontrun{
#' key <- "1235467abcdefg"
#' prevedere_calculations(key)
#' }
prevedere_calculations <- function(key) {
  path <- "calculations"
  result <- prevedere_fetch(key, path)
  unlist(result)
}

#' Frequencies
#'
#' Retrieve a list of the time frequencies currently supported by the
#' Prevedere API.
#'
#' @inheritParams prevedere_fetch
#'
#' @return A character vector of frequencies.
#'
#' @family enumeration functions
#' @export
#'
#' @examples
#' \dontrun{
#' key <- "1235467abcdefg"
#' prevedere_frequencies(key)
#' }
prevedere_frequencies <- function(key) {
  path <- "frequencies"
  result <- prevedere_fetch(key, path)
  unlist(result)
}

#' Seasonalities
#'
#' Retrieve a list of the seasonalities currently supported by the
#' Prevedere API.
#'
#' @inheritParams prevedere_fetch
#'
#' @return A character vector of seasonalities.
#'
#' @family enumeration functions
#' @export
#'
#' @examples
#' \dontrun{
#' key <- "1235467abcdefg"
#' prevedere_seasonalities(key)
#' }
prevedere_seasonalities <- function(key) {
  path <- "seasonalities"
  result <- prevedere_fetch(key, path)
  unlist(result)
}
