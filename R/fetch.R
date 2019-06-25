#' Query the Prevedere API
#'
#' Send a GET request to the Prevedere API. Most users should calling this
#' function directly and instead use the appropriate wrapper for accessing each
#' part of the API.
#'
#' @param path The path (within the API) to which the request will be sent.
#' @param payload The payload for the request. This should be a named list.
#'
#' @return The result of the API request.
#' @seealso [Prevedere.api.search], [GET][httr::GET], [content][httr:content], [response][httr::response]
#' @importFrom httr parse_url build_url GET stop_for_status content
#' @export
#'
#' @examples
#' \dontrun{
#' Prevedere.api.fetch(path = "/indicator/BLS/CES3133231058",
#'                     payload = list(Frequency = "Annual",
#'                                    Calculation = "None",
#'                                    Offset = 0))
#' }
Prevedere.api.fetch <- function(path,payload = NULL) {

  if(is.null(Prevedere.api.get_key()))
    stop("No API key has been set. Please obtain an API key and set it using `Prevedere.api.set_key()`.")

  url <- httr::parse_url("https://api.prevedere.com")

  url$path <- path

  if(is.null(payload)) payload <- list()

  payload["ApiKey"] <- Prevedere.api.get_key()

  url$query <- I(payload)

  url <- httr::build_url(url)

  r <- httr::GET(url)

  httr::stop_for_status(r)

  if(!grepl("application/json",r$headers$`content-type`))
    stop(paste0("Expected response type 'application/json', got '",r$headers$`content-type`,"'"))

  httr::content(r)
}

