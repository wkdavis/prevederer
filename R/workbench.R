Prevedere.api.workbench <- function(workbench_id) {
  path <- paste("/workbench",workbench_id,sep = "/")
  Prevedere.api.fetch(path)
}
