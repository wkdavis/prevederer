#' Prevederer
#'
#' prevederer provides an R wrapper around the
#' [Prevedere Software](https://www.prevedere.com/) API. The package facilitates
#' access to the main API components, including:
#'
#' \itemize{
#' \item Indicators
#' \item Models (correlations, raw models, and forecasts)
#' \item Workbenches
#' }
#'
#' Direct calls can also be made to the API using [Prevedere.api.fetch].
#'
#' **Note that access to the API requires an API key**. The API key is set in
#' [options] using the convenience function [Prevedere.api.set_key].
#'
#' @docType package
"_PACKAGE"