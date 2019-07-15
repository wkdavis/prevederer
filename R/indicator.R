#' Indicators
#'
#' Access indicator data and metadata.
#'
#' @param key A Prevedere API key.
#' @param provider Code for a data provider, can be hexidecimal or abbreviated
#' name.
#' @param provider_id Specific ProviderID for the indicator.
#' @param freq Frequency of indicator to retrieve. For a list of supported
#' frequencies, see [prevedere_frequencies()].
#' @param calculation Calculation to transform the indicator. For a list of
#' supported calculations, see [prevedere_calculations()].
#' @param start_date,end_date Start and end dates for the indicator. Each should be
#' either a date or a character string capable of being coerced to a date.
#' Setting a date to \code{NULL} will result in the historical data being unbounded
#' in that direction.
#' @param offset_periods Number of periods to offset.
#' @param raw Logical value indicating if data should be returned in its raw form
#' (typically nested lists) or formatted as appropriate, usually a dataframe.
#'
#' @return A list. prevedere_indicator returns metadata for
#' the target indicator, while prevedere_indicator_series returns the actual
#' data for the indicator (in addition to the metadata).
#'
#' @examples
#' \dontrun{
#'
#' k <- "1235467abcdefg"
#'
#' ## Return indicator metadata
#' prevedere_indicator(key = k, provider = "BLS", provider_id = "CES3133231058")
#'
#' ## Return indicator data
#' prevedere_indicator_series(
#'   key = k, provider = "BLS", provider_id = "CES3133231058",
#'   freq = "Monthly", calculation = "None", start_date = "2010-01-01",
#'   offset_periods = 0
#' )
#'
#' ## Return indicator data unformatted
#' prevedere_indicator_series(
#'   key = k, provider = "BLS", provider_id = "CES3133231058",
#'   freq = "Monthly", calculation = "None", start_date = "2010-01-01",
#'   offset_periods = 0, raw = TRUE
#' )
#' }
#'
#' @family indicator functions
#' @export
prevedere_indicator <- function(key, provider, provider_id) {
  path <- paste("indicator", provider, provider_id, sep = "/")
  prevedere_fetch(key, path)
}

#' @rdname prevedere_indicator
#' @export
prevedere_indicator_series <- function(key,
                                       provider,
                                       provider_id,
                                       freq = prevedere_frequencies(key),
                                       calculation = prevedere_calculations(key),
                                       start_date = NULL,
                                       end_date = NULL,
                                       offset_periods = 0,
                                       raw = FALSE) {
  freq <- match.arg(freq)
  if (length(freq) != 1) {
    stop(paste0("freq should be length 1, not ", length(freq)))
  }

  calculation <- match.arg(calculation)
  if (length(calculation) != 1) {
    stop(paste0("calculation should be length 1, not ", length(calculation)))
  }

  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    if (is.na(start_date)) {
      stop("'start_date' must be a date or an object coercible to a date.")
    }
  }

  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
    if (is.na(end_date)) {
      stop("'end_date' must be a date or an object coercible to a date.")
    }
  }

  offset_periods <- as.numeric(offset_periods)
  if (is.na(offset_periods)) {
    stop("'offset' must be numeric.")
  }

  path <- paste("indicator", "series", provider, provider_id, sep = "/")

  d <- prevedere_fetch(key, path, payload = list(
    Frequency = freq,
    Offset = offset_periods,
    Calculation = calculation,
    StartDate = start_date,
    EndDate = end_date
  ))

  if (raw) {
    return(d)
  }

  # store names of list elements
  d.names <- names(d[[1]])

  # recursively fill NULL values in the list so that the columns still appear
  # in the new dataframe
  d <- lapply(d, function(x) lapply(x, function(y) ifelse(is.null(y), "", y)))

  # convert nested list into a dataframe
  df <- data.frame(
    matrix(unlist(d), nrow = length(d), byrow = T),
    stringsAsFactors = FALSE
  )

  # give dataframe columns the same names as the original list elements
  colnames(df) <- d.names

  df[["Value"]] <- as.numeric(df[["Value"]])
  df[["Date"]] <- as.Date(df[["Date"]])
  df[["ManuallyAdjusted"]] <- as.logical(df[["ManuallyAdjusted"]])

  df
}

#' Correlation
#'
#' Calculates Pearson's r and other statistics at different offsets between an
#' endogenous and exogenous indicator.
#'
#' @param endog_provider Code for the data provider of the endogenous indicator,
#' can be hexidecimal or abbreviated name.
#' @param endog_provider_id Specific ProviderID for the endogenous indicator.
#' @param exog_provider Code for the data provider of the exogneous indicator,
#' can be hexidecimal or abbreviated name.
#' @param exog_provider_id Specific ProviderID for the exogenous indicator.
#' @inheritParams prevedere_indicator_series
#'
#' @return Model results and metadata, as a list.
#' @export
#'
#' @family indicator functions
#' @examples
#' \dontrun{
#' k <- "1235467abcdefg"
#'
#' prevedere_correlation(
#'   key = k, endog_provider = "BLS", endog_provider_id = "CES3133231058",
#'   exog_provider = "FRED", exog_provider_id = "PCU332313332313", freq = "Monthly",
#'   calculation = "ThreePeriodMoving"
#' )
#' }
prevedere_correlation <- function(key, endog_provider,
                                  endog_provider_id,
                                  exog_provider,
                                  exog_provider_id,
                                  freq = prevedere_frequencies(key),
                                  calculation = prevedere_calculations(key),
                                  raw = FALSE) {
  freq <- match.arg(freq)
  if (length(freq) != 1) {
    stop(paste0("freq should be length 1, not ", length(freq)))
  }

  calculation <- match.arg(calculation)
  if (length(calculation) != 1) {
    stop(paste0("calculation should be length 1, not ", length(calculation)))
  }


  path <- paste("correlation",
                endog_provider,
                endog_provider_id,
                exog_provider,
                exog_provider_id,
                freq,
                calculation,
                sep = "/"
  )

  fit <- prevedere_fetch(key, path)

  if (raw) {
    return(fit)
  }

  # store names of list elements
  d.names <- names(fit$Data[[1]])

  # recursively fill NULL values in the list so that the columns still appear
  # in the new dataframe
  fit$Data <- lapply(
    fit$Data,
    function(x) lapply(
      x,
      function(y) ifelse(is.null(y), "", y)
    )
  )

  # convert nested list into a dataframe
  fit$Data <- data.frame(matrix(unlist(fit$Data),
                                nrow = length(fit$Data),
                                byrow = T
  ),
  stringsAsFactors = FALSE
  )

  # give dataframe columns the same names as the original list elements
  colnames(fit$Data) <- d.names

  # convert to proper type - all non-Notes fields to numeric
  notes_col <- which(colnames(fit$Data) == "Notes")
  fit$Data[-notes_col] <- apply(fit$Data[-notes_col], 2, as.numeric)

  fit
}