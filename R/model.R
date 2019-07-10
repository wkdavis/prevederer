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
#' @examples
#' \dontrun{
#' k = "1235467abcdefg"
#'
#' prevedere_correlation(key = k,endog_provider = "BLS", endog_provider_id = "CES3133231058",
#' exog_provider = "FRED", exog_provider_id = "PCU332313332313", freq = "Monthly",
#' calculation = "ThreePeriodMoving")
#' }
prevedere_correlation <- function(key,endog_provider,
                                      endog_provider_id,
                                      exog_provider,
                                      exog_provider_id,
                                      freq = c("Annual","SemiAnnual","Quarterly","Monthly","BiWeekly","Weekly","Daily"),
                                      calculation = c("None","PeriodOverPeriod","YearOverYear","ThreePeriodMoving","FivePeriodMoving","ThreePeriodYearOverYear"),
                                      raw = FALSE) {

  freq <- match.arg(freq)
  if(length(freq) != 1) stop(paste0("freq should be length 1, not ",length(freq)))

  calculation <- match.arg(calculation)
  if(length(calculation) != 1) stop(paste0("calculation should be length 1, not ",length(calculation)))


  path <- paste("/correlation",endog_provider,endog_provider_id,exog_provider,exog_provider_id,freq,calculation,sep = "/")

  fit <- prevedere_fetch(key,path)

  if(raw) return(fit)

  # store names of list elements
  d.names <- names(fit$Data[[1]])

  # recursively fill NULL values in the list so that the columns still appear
  # in the new dataframe
  fit$Data <- lapply(fit$Data, function(x) lapply(x,function(y) ifelse(is.null(y),"",y)))

  # convert nested list into a dataframe
  fit$Data <- data.frame(matrix(unlist(fit$Data ), nrow=length(fit$Data), byrow=T),stringsAsFactors=FALSE)

  # give dataframe columns the same names as the original list elements
  colnames(fit$Data) <- d.names

  # convert to proper type - all non-Notes fields to numeric
  fit$Data[-which(colnames(fit$Data)=="Notes")] <- apply(fit$Data[-which(colnames(fit$Data)=="Notes")],2,as.numeric)

  fit
}

#' Raw model
#'
#' Returns all information about a forecast model.
#'
#' @param model_id UUID for the forecast model.
#' @param exclude_indicators Whether to return only indicators used in model, or all associated indicators.
#' @param as_of_date Get the model only using data up to the specified date (YYYY-MM-DD). Used for backtesting.
#' @inheritParams prevedere_indicator_series
#'
#' @return A list of model components and metadata, including indicators,
#' coefficients, and the model start date.
#' @export
#'
#' @examples
#' \dontrun{
#' k = "1235467abcdefg"
#'
#' prevedere_raw_model(key = k, model_id = "1b1878399833c7f38b094e54dd43d374")
#'
#' ## Backtest
#' prevedere_raw_model(key = k, model_id = "1b1878399833c7f38b094e54dd43d374",as_of_data = "2019-05-01")
#' }
prevedere_raw_model <- function(key,model_id,exclude_indicators = TRUE,as_of_date = NULL,raw = FALSE) {

  if(!is.logical(exclude_indicators)) stop("'exclude_indicators' must be logical.")

  if(!is.null(as_of_date)) {
    as_of_date <- as.Date(as_of_date)
    if(is.na(as_of_date)) stop("'as_of_date' must be a date or an object coercible to a date.")
  }

  path <- paste("/rawmodel",model_id,sep = "/")

  payload <- list(ExcludeIndicators = exclude_indicators,
                  AsOfDate = as_of_date)

  fit <- prevedere_fetch(key,path,payload)

  if(raw) return(fit)

  fit$StartDate <- as.Date(fit$StartDate)

  fit$Indicators[[1]]$Values

  fit$Indicators <- lapply(fit$Indicators,function(i) {

    # store names of list elements
    d.names <- names(i$Values[[1]])

    # recursively fill NULL values in the list so that the columns still appear
    # in the new dataframe
    i$Values <- lapply(i$Values, function(x) lapply(x,function(y) ifelse(is.null(y),"",y)))

    # convert nested list into a dataframe
    i$Values <- data.frame(matrix(unlist(i$Values), nrow=length(i$Values), byrow=T),stringsAsFactors=FALSE)

    # give dataframe columns the same names as the original list elements
    colnames(i$Values) <- d.names

    # convert to proper type
    i$Values[["Value"]] <- as.numeric(i$Values[["Value"]])
    i$Values[["Date"]] <- as.Date(i$Values[["Date"]])
    i$Values[["ManuallyAdjusted"]] <- as.logical(i$Values[["ManuallyAdjusted"]])

    i

  })

  fit

}

#' Forecast
#'
#' Returns historical fit and forecasted values of a forecast model.
#'
#' @inheritParams prevedere_raw_model
#'
#' @return A dataframe of forecasted values and metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' k = "1235467abcdefg"
#'
#' prevedere_forecast(key = k, model_id = "1b1878399833c7f38b094e54dd43d374")
#' }
prevedere_forecast <- function(key,model_id,as_of_date = NULL, raw = FALSE) {

  if(!is.null(as_of_date)) {
    as_of_date <- as.Date(as_of_date)
    if(is.na(as_of_date)) stop("'as_of_date' must be a date or an object coercible to a date.")
  }

  path <- paste("/forecast",model_id,sep = "/")

  payload <- list(AsOfDate = as_of_date)

  f <- prevedere_fetch(key,path,payload)

  if(raw) return(f)

  f

  # store names of list elements
  f.names <- names(f[[1]])

  # recursively fill NULL values in the list so that the columns still appear
  # in the new dataframe
  f <- lapply(f, function(x) lapply(x,function(y) ifelse(is.null(y),"",y)))

  # convert nested list into a dataframe
  df <- data.frame(matrix(unlist(f), nrow=length(f), byrow=T),stringsAsFactors=FALSE)

  # give dataframe columns the same names as the original list elements
  colnames(df) <- f.names

  df[["Value"]] <- as.numeric(df[["Value"]])
  df[["Date"]] <- as.Date(df[["Date"]])
  df[["ManuallyAdjusted"]] <- as.logical(df[["ManuallyAdjusted"]])

  df

}
