#' The fully-rooted local path to a file.
#' @param file_id A list identifying the file.
#' @param config A configuration from \link{data_configuration}.
#'     If this is NULL, then a new one is read from your dotfiles.
#' @return A fully-rooted path.
#' @export
local_path <- function(file_id, config = NULL) {
  if (is.null(config)) {
    config <- data_configuration()
  }
  fs::path(config$LOCALDATA, project_path(file_id))
}


#' Save information about a directory or data file.
#'
#' If you give it a directory, it saves a file in the directory.
#' If you give it a filename, it saves a file next to that one.
#'
#' The format is TOML. The file ends in .rampmd (?).
#' The document structure follows W3C-Prov ontology, as best we
#' can, which means the TOML has subheadings for things that are
#' agents, activities, and entities.
#'
#' I would like to fill out some properties automatically, when
#' possible.
#'
#' Some well-known properties:
#'
#' \itemize{
#'   \item title Something you would call this data.
#'   \item download_date When the file was
#'   \item creator Person or organization that made the file.
#'   \item creator_email Email of person or organization that made the file.
#'   \item obtainer Person who got the file.
#'   \item obtainer_email Email of person who got the file.
#'   \item format Describe the data format.
#'   \item source_repository The git repository of code that made the data.
#'   \item source_version Version number of the source code.
#'   \item source_hash Hash of the git checkout.
#'   \item source_branch Branch of the code that made the data.
#'   \item description A free text description of what's in the file.
#'   \item creation_date When the file was created.
#' }
#'
#' @param path The path to the directory or file.
#' @param properties A list of information about that path.
#' @export
save_source <- function(path, properties) {
  rampmd_extension <- "rampmd"
  if (fs::is_file(path)) {
    save_path <- fs::path(fs::path_ext_remove(path), ext = rampmd_extension)
  } else if (fs::is_dir(path)) {
    save_path <- fs::path(path, fs::path(fs::path_file(path), ext = rampmd_extension))
  }
  sink(save_path)

  check_print <- function(prop, key) {
    if (prop %in% names(properties)) {
      cat(paste(key, ": ", properties[[prop]], "\n", sep = ""))
    }
  }

  cat(paste("[dataset]\n"))
  check_print("title", "title")
  check_print("format", "format")

  if ("description" %in% names(properties)) {
    cat(paste("description = \"\"\"\n"))
    cat(paste(properties[["description"]]))
    cat(paste("\"\"\""))
  }

  cat(paste("[creator]\n"))
  check_print("creator_email", "email")
  check_print("creator", "name")

  cat(paste("[obtainer]\n"))
  check_print("obtainer_email", "email")
  check_print("obtainer", "name")

  cat(paste("[generation]\n"))
  check_print("creation_date", "date")

  cat(paste("[code]\n"))
  check_print("source_repository", "repository")
  check_print("source_version", "version")
  check_print("source_hash", "hash")
  check_print("source_branch", "branch")

  sink()
}


#' Ensure files are available locally.
#'
#' @param ramp_identifiers A list of RAMP IDs.
#' @export
ensure_present <- function(ramp_identifiers) {
  missing_identifiers <- list()
  for (find_entry in ramp_identifiers) {
    if (!is.null(find_entry)) {
      actual <- fs::path(project_base_path(), project_path(find_entry), find_entry$path)
      if (!file.exists(actual)) {
        missing_identifiers[length(missing_identifiers) + 1] <- find_entry
      }  # If it's there, the job is done.
    }  # Null entries happen if you assign to a later integer.
  }

  if (length(missing) > 0) {
    ssh_session <- ssh::ssh_connect(host)
    for (transfer_entry in missing_identifiers) {

    }
    ssh::ssh_disconnect(ssh_session)
  }
}


#' Ensure a set of local files are on the server.
#'
#' Not implemented.
#'
#' @param ramp_identifiers A list of RAMP IDs.
#' @export
ensure_on_server <- function(ramp_identifiers) {

}
