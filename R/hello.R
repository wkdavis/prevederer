# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

Prevedere.api.set_key <- function(api_key) {

  if (missing(api_key))
    stop("")

  if (!is.null(getOption("Prevedere.api.key")))
    warning(paste0("Overwriting current api key: ",getOption("Prevedere.api.key")))

  options(Prevedere.api.key = as.character(api_key))

  invisible(getOption("Prevedere.api.key"))

}

Prevedere.api.set_key(apikey)

Prevedere.api.get_key <- function() {

  if (is.null(getOption("Prevedere.api.key")))
    warning("'Prevedere.api.key' is not set.")

  getOption("Prevedere.api.key")

}

Prevedere.api.fetch <- function(path,payload = NULL) {

  if(is.null(Prevedere.api.get_key()))
    stop("No API key has been set. Please obtain an API key and set it using `Prevedere.api.set_key()`.")

  url <- httr::parse_url("https://api.prevedere.com")

  url$path <- path

  if(is.null(payload)) payload <- list()

  payload["ApiKey"] <- Prevedere.api.get_key()

  url$query <- payload

  url <- httr::build_url(url)

  r <- httr::GET(url)

  httr::stop_for_status(r)

  if(!grepl("application/json",r$headers$`content-type`))
    stop(paste0("Expected response type 'application/json', got '",r$headers$`content-type`,"'"))

  httr::content(r)
}

Prevedere.api.indicator <- function(provider,provider_id) {
  path <- paste("/indicator",provider,provider_id,sep = "/")
  Prevedere.api.fetch(path)
}

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

Prevedere.api.correlation <- function(endog_provider,
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

  fit <- Prevedere.api.fetch(path)

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

Prevedere.api.search <- function(query) {
  path <- "/search"
  payload <- query

  Prevedere.api.fetch(path,payload)
}

Prevedere.api.raw_model <- function(model_id,exclude_indicators = TRUE,as_of_date = NULL,raw = FALSE) {

  if(!is.logical(exclude_indicators)) stop("'exclude_indicators' must be logical.")

  if(!is.null(as_of_date)) {
    as_of_date <- as.Date(as_of_date)
    if(is.na(as_of_date)) stop("'as_of_date' must be a date or an object coercible to a date.")
  }

  path <- paste("/rawmodel",model_id,sep = "/")

  payload <- list(ExcludeIndicators = exclude_indicators,
                  AsOfDate = as_of_date)

  fit <- Prevedere.api.fetch(path,payload)

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

Prevedere.api.forecast <- function(model_id,as_of_date = NULL, raw = FALSE) {

  if(!is.null(as_of_date)) {
    as_of_date <- as.Date(as_of_date)
    if(is.na(as_of_date)) stop("'as_of_date' must be a date or an object coercible to a date.")
  }

  path <- paste("/forecast",model_id,sep = "/")

  payload <- list(AsOfDate = as_of_date)

  f <- Prevedere.api.fetch(path,payload)

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

Prevedere.api.providers <- function() {
  path <- "/provider"
  Prevedere.api.fetch(path)
}

Prevedere.api.workbench <- function(workbench_id) {
  path <- paste("/workbench",workbench_id,sep = "/")
  Prevedere.api.fetch(path)
}
