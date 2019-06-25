#' Workbench
#'
#' Returns the indicators used in a workbench. Typically includes workbench
#' metadata and associated indicator metadata.
#'
#' @param workbench_id UUID for workbench.
#'
#' @return A list of workbench metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' Prevedere.api.workbench("b8da829f7a1d4509ca5125e4699d6f0e")
#' }
Prevedere.api.workbench <- function(workbench_id) {
  path <- paste("/workbench",workbench_id,sep = "/")
  Prevedere.api.fetch(path)
}
