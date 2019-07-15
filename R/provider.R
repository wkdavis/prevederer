#' Providers
#'
#' Retrieve a list of entities whose data is available in Prevedere
#'
#' @inheritParams prevedere_indicator_series
#'
#' @return A dataframe (or list if \code{raw = TRUE}) of provider metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' k <- "1235467abcdefg"
#'
#' prevedere_providers(key = k)
#' }
prevedere_providers <- function(key, raw = FALSE) {
  path <- "provider"
  p <- prevedere_fetch(key, path)

  if (raw) {
    return(p)
  }

  # store names of list elements
  p.names <- names(p[[1]])

  # recursively fill NULL values in the list so that the columns still appear
  # in the new dataframe
  p <- lapply(p, function(x) lapply(x, function(y) ifelse(is.null(y), "", y)))

  # convert nested list into a dataframe
  p <- data.frame(matrix(unlist(p),
    nrow = length(p),
    byrow = T
  ),
  stringsAsFactors = FALSE
  )

  # give dataframe columns the same names as the original list elements
  colnames(p) <- p.names

  p
}
