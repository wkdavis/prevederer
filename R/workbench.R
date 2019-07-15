#' Workbench
#'
#' Returns the indicators used in a workbench. Typically includes workbench
#' metadata and associated indicator metadata.
#'
#' @inheritParams prevedere_fetch
#' @param workbench_id UUID for workbench.
#'
#' @return A list of workbench metadata.
#'
#' @family workbench functions
#' @examples
#' \dontrun{
#' k <- "1235467abcdefg"
#'
#' prevedere_workbench(key = k, workbench_id = "b8da829f7a1d4509ca5125e4699d6f0e")
#' }
#' @export
prevedere_workbench <- function(key, workbench_id) {
  path <- paste("workbench", workbench_id, sep = "/")
  prevedere_fetch(key, path)
}

