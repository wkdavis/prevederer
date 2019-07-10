#' Query the Prevedere API
#'
#' Send a GET request to the Prevedere API. Most users should calling this
#' function directly and instead use the appropriate wrapper for accessing each
#' part of the API.
#'
#' @param key A Prevedere API key.
#' @param path The path (within the API) to which the request will be sent.
#' @param payload The payload for the request. This should be a named list.
#'
#' @return The result of the API request.
#' @seealso [prevedere_search], [GET][httr::GET], [content][httr:content], [response][httr::response]
#' @importFrom httr parse_url build_url GET stop_for_status content
#' @export
#'
#' @examples
#' \dontrun{
#' prevedere_fetch(key = "1235467abcdefg",
#'                 path = "/indicator/BLS/CES3133231058",
#'                 payload = list(Frequency = "Annual",
#'                                Calculation = "None",
#'                                Offset = 0)
#'                 )
#'
#' k = "1235467abcdefg"
#' prevedere_fetch(key = k,
#'                 path = "/indicator/BLS/CES3133231058",
#'                 payload = list(Frequency = "Annual",
#'                                Calculation = "None",
#'                                Offset = 0)
#'                 )
#' }
prevedere_fetch <- function(key,path,payload = NULL) {
  url <- httr::parse_url("https://api.prevedere.com")

  url$path <- path

  if(is.null(payload)) payload <- list()

  payload["ApiKey"] <- key

  url$query <- I(payload)

  url <- httr::build_url(url)

  r <- httr::GET(url)

  httr::stop_for_status(r)

  if(!grepl("application/json",r$headers$`content-type`))
    stop(paste0("Expected response type 'application/json', got '",r$headers$`content-type`,"'"))

  httr::content(r)
}

