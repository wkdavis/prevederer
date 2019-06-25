#' Indicators
#'
#' Access indicator data and metadata.
#'
#' @param provider Code for a data provider, can be hexidecimal or abbreviated
#' name.
#' @param provider_id Specific ProviderID for the indicator.
#' @param freq Frequency of indicator to retrieve. Must be one of "Annual",
#' "SemiAnnual", "Quarterly", "Monthly", "BiWeekly", "Weekly", or "Daily".
#' @param calculation Calculation to transform the indicator. Must be one of
#' "None", "PeriodOverPeriod", "YearOverYear", "ThreePeriodMoving",
#' "FivePeriodMoving", or "ThreePeriodYearOverYear".
#' @param start_date,end_date Start and end dates for the indicator. Each should be
#' either a date or a character string capable of being coerced to a date.
#' Setting a date to \code{NULL} will result in the historical data being unbounded
#' in that direction.
#' @param offset_periods Number of periods to offset.
#' @param raw Logical value indicating if data should be returned in its raw form
#' (typically nested lists) or formatted as appropriate, usually a dataframe.
#'
#' @return A list. Prevedere.api.indicator returns metadata for
#' the target indicator, while Prevedere.api.indicator_series returns the actual
#' data for the indicator (in addition to the metadata).
#'
#' @examples
#' \dontrun{
#'
#' ## Return indicator metadata
#' Prevedere.api.indicator(provider = "BLS", provider_id = "CES3133231058")
#'
#' ## Return indicator data
#' Prevedere.api.indicator_series(provider = "BLS", provider_id = "CES3133231058",
#' freq = "Monthly", calculation = "None", start_date = "2010-01-01",
#' offset_periods = 0)
#'
#' ## Return indicator data unformatted
#' Prevedere.api.indicator_series(provider = "BLS", provider_id = "CES3133231058",
#' freq = "Monthly", calculation = "None", start_date = "2010-01-01",
#' offset_periods = 0,raw = TRUE)
#' }
#'
#' @name indicator
NULL

#' @rdname indicator
#' @export
Prevedere.api.indicator <- function(provider,provider_id) {
  path <- paste("/indicator",provider,provider_id,sep = "/")
  Prevedere.api.fetch(path)
}

#' @rdname indicator
#' @export
Prevedere.api.indicator_series <- function(provider,
                                           provider_id,
                                           freq = c("Annual","SemiAnnual","Quarterly","Monthly","BiWeekly","Weekly","Daily"),
                                           calculation = c("None","PeriodOverPeriod","YearOverYear","ThreePeriodMoving","FivePeriodMoving","ThreePeriodYearOverYear"),
                                           start_date = NULL,
                                           end_date = NULL,
                                           offset_periods = 0,
                                           raw = FALSE) {

  freq <- match.arg(freq)
  if(length(freq) != 1) stop(paste0("freq should be length 1, not ",length(freq)))

  calculation <- match.arg(calculation)
  if(length(calculation) != 1) stop(paste0("calculation should be length 1, not ",length(calculation)))

  start_date <- as.Date(start_date)
  if(is.na(start_date)) stop("'start_date' must be a date or an object coercible to a date.")

  end_date <- as.Date(end_date)
  if(is.na(end_date)) stop("'end_date' must be a date or an object coercible to a date.")

  offset_periods <- as.numeric(offset_periods)
  if(is.na(offset_periods)) stop("'offset' must be numeric.")

  path <- paste("/indicator","series",provider,provider_id,sep = "/")

  d <- Prevedere.api.fetch(path,payload = list(Frequency = freq,
                                               Offset = offset_periods,
                                               Calculation = calculation,
                                               StartDate = start_date,
                                               EndDate = end_date))

  if(raw) return(d)

  # store names of list elements
  d.names <- names(d[[1]])

  # recursively fill NULL values in the list so that the columns still appear
  # in the new dataframe
  d <- lapply(d, function(x) lapply(x,function(y) ifelse(is.null(y),"",y)))

  # convert nested list into a dataframe
  df <- data.frame(matrix(unlist(d), nrow=length(d), byrow=T),stringsAsFactors=FALSE)

  # give dataframe columns the same names as the original list elements
  colnames(df) <- d.names

  df[["Value"]] <- as.numeric(df[["Value"]])
  df[["Date"]] <- as.Date(df[["Date"]])
  df[["ManuallyAdjusted"]] <- as.logical(df[["ManuallyAdjusted"]])

  df
}

