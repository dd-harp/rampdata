#' This variable contains configuration for the file locations.
.rampdata.config <- list()


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


#' Get the configuration file and store as package global.
cached_config <- function(key, builder) {
  config <- .rampdata.config[[key]]
  if (is.null(config)) {
    config <- builder()
    package_name <- packageName()
    if (!is.null(package_name)) {
      .rampdata.config[[key]] <- config
      assignInNamespace(".rampdata.config", .rampdata.config, ns = package_name)
    } else {
      .rampdata.config[[key]] <<- config
    }
  }
  config
}


#' Convert a single string into a ramp path.
#' @param rooted_path Must start with a "/". If it starts with "inputs",
#'     then it's in the global area. Otherwise, it's in a project.
#' @return a list describing a ramp path.
#'
#' The order is `/project/stage/dataset/version/path`.
ramp_path_from_rooted <- function(rooted_path) {
  if (substr(rooted_path, 1, 1) != "/") {
    cat(paste("The path in", rooted_path, "should start with a /."))
    stop()
  }
  rpath <- vector(mode = "list", length = 5)
  parts <- unlist(strsplit(rooted_path, "/"))
  # Add a blank project for global inputs.
  if (length(parts) > 1 & parts[2] == "inputs") parts <- c("", parts)
  for (part_idx in 2:5) {
    if (length(parts) >= part_idx) {
      rpath[[part_idx - 1]] <- parts[part_idx]
    } else {
      rpath[[part_idx - 1]] <- ""
    }
  }
  if (length(parts) >= 6) {
    rpath[[5]] <- do.call(file.path, as.list(parts[6:length(parts)]))
  } else {
    rpath[[5]] <- ""
  }
  names(rpath) <- c("project", "stage", "dataset", "version", "file")
  rpath
}


#' Make a RAMP path.
#' @param rooted_path Optional. Specify a path with a single string that looks like
#'     a rooted path. This could be "/" for an empty path that you augment later.
#'     It could be "/project/stage/dataset/version/filename".
#' @param project Optional project name.
#' @param stage Optional stage.
#' @param dataset Optional dataset.
#' @param version Optional version.
#' @param file Optional filename, which is relative path, including subdirectory to
#'     a file.
#' @return a ramp path.
#' @examples
#' ramp_path("/uganda2020/mic/population/201015_final/population.csv")
#' ramp_path(project = "uganda2020", dataset = "population")
#' @export
ramp_path <- function(
  rooted_path = NULL, project = NULL, stage = NULL, dataset = NULL,
  version = NULL, file = NULL)
  {
  if (!is.null(rooted_path)) {
    rpath <- ramp_path_from_rooted(rooted_path)
  } else {
    rpath <- ramp_path_from_rooted("/")
  }
  add_path(rpath, project, stage, dataset, version, file)
}


#' Modify parts of the ramp path by name.
#' @param rpath The original ramp path.
#' @param project The project name as a string.
#' @param stage The stage name.
#' @param dataset The dataset name.
#' @param version The version name.
#' @param file The filename, which can be a relative path to a file within the
#'     directory.
#' @return A ramp path with the specified parts modified.
#' @export
add_path <- function(
  rpath, project = NULL, stage = NULL, dataset = NULL, version = NULL, file = NULL
) {
  if (!is.null(project)) {
    rpath[["project"]] <- project
  }
  if (!is.null(stage)) {
    rpath[["stage"]] <- stage
  }
  if (!is.null(dataset)) {
    rpath[["dataset"]] <- dataset
  }
  if (!is.null(version)) {
    rpath[["version"]] <- version
  }
  if (!is.null(file)) {
    rpath[["file"]] <- file
  }
  rpath
}


#' Convert a ramp path into a simple string filename for reading or writing.
#' @param rpath A ramp path.
#' @param config If you want to set a configuration, this is it.
#' @return A character string.
#'
#' This applies the configuration file information to find the root of
#' the data directory and place the filename in that root.
#' @export
as.path <- function(rpath, config = NULL) {
  if (is.null(config)) {
    config <- cached_config("config", data_configuration)
  }
  if (nchar(rpath$project) > 0) {
    base <- list(config$LOCALDATA, "projects", rpath$project, rpath$stage, rpath$dataset)
  } else {
    base <- list(config$LOCALDATA, rpath$stage, rpath$dataset)
  }
  if (nchar(rpath$version) > 0) {
    base[[length(base) + 1]] <- rpath$version
  }
  if (nchar(rpath$file) > 0) {
    base[[length(base) + 1]] <- rpath$file
  }
  do.call(file.path, base)
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
