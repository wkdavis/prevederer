#' Set Prevedere API key
#'
#' Set the Prevedere API key to be used. This key can be obtained by contacting
#' Prevedere. The key is stored in an \R option named "Prevedere.api.key".
#'
#' @param key A Prevedere API key.
#' @param overwrite If the API key is already set, should it be overwritten?
#'
#' @return The current API key as set in [options]. In the case of
#' [Prevedere.api.set_key], the API key is returned invisibly.
#'
#' @name key
#' @examples
#' Prevedere.api.set_key("abcdefghijkl")
#' Prevedere.api.get_key()
#'
#' ## the API key can also be accessed directly via options
#' options(Prevedere.api.key = "mnopqrstuv")
#' getOption("Prevedere.api.key")
#'
#' \dontrun{
#' ## attempting to get the Prevedere.api.key when it hasn't been set will
#' ## result in a warning.
#' options(Prevedere.api.key = NULL)
#' Prevedere.api.get_key()
#'
#' ## setting the API key when the option is already set will also result in
#' ## a warning
#' Prevedere.api.set_key("abcdefghijkl")
#' Prevedere.api.set_key("mnopqrstuv")
#' }
NULL

#' @rdname key
#' @export
Prevedere.api.set_key <- function(key,overwrite = TRUE) {

  if (missing(key))
    stop("'key' must not be missing.")

  if (!is.null(getOption("Prevedere.api.key")))
    if(overwrite) {
      warning(paste0("Overwriting current API key: ",getOption("Prevedere.api.key")))
    } else {
      stop("Prevedere.api.key is already set.")
    }

  options(Prevedere.api.key = as.character(key))

  invisible(getOption("Prevedere.api.key"))

}

#' @rdname key
#' @export
Prevedere.api.get_key <- function() {

  if (is.null(getOption("Prevedere.api.key")))
    warning("'Prevedere.api.key' is not set.")

  getOption("Prevedere.api.key")

}