#' Set Prevedere API key
#'
#' Set the Prevedere API key to be used. This key can be obtained by contacting
#' Prevedere. The key is stored in an \R option named "prevedere.api.key".
#'
#' @param key A Prevedere API key.
#' @param overwrite If the API key is already set, should it be overwritten?
#'
#' @return The current API key as set in [options]. In the case of
#' [prevedere.api.set_key], the API key is returned invisibly.
#'
#' @name key
#' @examples
#' prevedere.api.set_key("abcdefghijkl")
#' prevedere.api.get_key()
#'
#' ## the API key can also be accessed directly via options
#' options(prevedere.api.key = "mnopqrstuv")
#' getOption("prevedere.api.key")
#'
#' \dontrun{
#' ## attempting to get the prevedere.api.key when it hasn't been set will
#' ## result in a warning.
#' options(prevedere.api.key = NULL)
#' prevedere.api.get_key()
#'
#' ## setting the API key when the option is already set will also result in
#' ## a warning
#' prevedere.api.set_key("abcdefghijkl")
#' prevedere.api.set_key("mnopqrstuv")
#' }
NULL

#' @rdname key
#' @export
prevedere.api.set_key <- function(key,overwrite = TRUE) {

  if (missing(key))
    stop("'key' must not be missing.")

  if (!is.null(getOption("prevedere.api.key")))
    if(overwrite) {
      warning(paste0("Overwriting current API key: ",getOption("prevedere.api.key")))
    } else {
      stop("prevedere.api.key is already set.")
    }

  options(prevedere.api.key = as.character(key))

  invisible(getOption("prevedere.api.key"))

}

#' @rdname key
#' @export
prevedere.api.get_key <- function() {

  if (is.null(getOption("prevedere.api.key")))
    warning("'prevedere.api.key' is not set.")

  getOption("prevedere.api.key")

}