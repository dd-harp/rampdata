#' Read a workflow description file.
#' @param filename The path to the config file to read.
#' @return A list of config information from `configr`.
read_workflow_config <- function(filename) {
  first_pass <- configr::read.config(filename)
  if ("versions" %in% names(first_pass)) {
    replaced <- configr::read.config(filename, extra.list = first_pass$versions)
  } else {
    replaced <- first_pass
  }
  replaced
}


#' Read a workflow file in order to set up versions of inputs.
#' @param filename The filename with the workflow in it.
#' This reads a key-value file with names of versions and files.
#' @export
initialize_workflow <- function(filename) {
  config <- read_workflow_config(filename)
  key <- "workflow"
  package_name <- packageName()
  if (!is.null(package_name)) {
    .rampdata.config[[key]] <- config
    assignInNamespace(".rampdata.config", .rampdata.config, ns = package_name)
  } else {
    .rampdata.config[[key]] <<- config
  }
}


#' Get a ramp_path from a workflow configuration file.
#' @param role The key used to identify the path in the configuration file.
#' @return a ramp_path made from that key.
#' @seealso \code{\link{initialize_workflow}}
#' @export
workflow_path <- function(role) {
  config <- cached_config("workflow", function() {
    cat(paste("Set the workflow with initialize_workflow before using it for ", role))
    stop()
  })
  if (role %in% names(config[["roles"]])) {
    rpath <- ramp_path(config[["roles"]][[role]])
  } else {
    stop(paste("workflow configuration doesn't have role: ", role))
  }
  rpath
}
