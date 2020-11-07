utils::globalVariables("data_config", package = "rampdata")

#' Reads configuration file on where to download data.
#'
#' If you want to make the config file, then create
#' a file in one of three places:
#'
#' - \code{${XDG_CONFIG_HOME}/RAMP/data.ini}
#' - \code{$HOME/.config/RAMP/data.ini}
#' - \code{$HOME/.ramp.ini}
#'
#' Put the following in it.
#'
#' @param section Which section of the initialization file to read.
#'   This defaults to \code{Default}. This parameter exists in order
#'   to create a \code{Test} section to use for non-destructive testing.
#' @return A list of configuration parameters.
#'
#' @examples
#' \dontrun{
#' [Default]
#' SCPHOST = computer-name.ihme.uw.edu
#' SCPHOSTBASE = /path/to/data/directory
#' LOCALDATA = /home/username/data
#' }
#'
#' @export
data_configuration <- function(section = "Default") {
  home <- list(
    xdg = fs::path(Sys.getenv("XDG_CONFIG_HOME"), "RAMP", "data.ini"),
    env = fs::path(Sys.getenv("HOME"), ".config", "RAMP", "data.ini"),
    default = fs::path("", "home", Sys.info()[["effective_user"]], ".ramp.ini")
  )

  found_ini <- character(0)
  for (ini_path in home) {
    if (file.exists(ini_path)) {
      cfg <- configr::read.config(ini_path)
      if (is.list(cfg)) {
        return(cfg[[section]])
      }
      found_ini <- c(found_ini, ini_path)
    }
  }
  if (length(found_ini) > 0) {
    logerror(paste("Found data.ini but could not load", paste(found_ini, collapse = ", ")))
  } else {
    logerror(paste("Could not find data directory because could not",
        "find data.ini in", paste(home, collapse = ", ")))
  }
  NULL
}


#' Use scp to retrieve data.
#'
#' Only if you have ssh credentials set up.
#' This is used for data that isn't yet public.
#' Equivalent to: ssh ihme.uw.edu:/path/to/file local_file.dat
#'
#' You have to call `data_configuration()` before you call this.
#'
#' @param session An ssh session.
#' @param filename The path of the file within the repository.
#' @param local_directory Where to put that file on the local machine.
#' @export
get_from_ihme <- function(session, filename, data_configuration = NULL) {
  if (is.null(data_configuration)) {
    config <- rampdata::data_configuration()
  } else {
    config <- data_configuration
  }
  source <- fs::path(config$SCPHOSTBASE, filename)
  target <- fs::path(config$LOCALDATA, fs::path_dir(filename))
  dir.create(target, showWarnings = FALSE, recursive = FALSE)
  ssh::scp_download(session, source, to = target)
}




#' Use scp to send data.
#'
#' Responsible for moving files within a certain set of remote
#' and local directory structures, as specified by the data configuration.
#' This will create directories and ensure that files aren't
#' overwritten by the copy command.
#'
#' @param session An ssh session.
#' @param filename The path of the file within the repository.
#' @param overwrite Whether it is OK to overwrite the destination file.
#' @param local_directory Where to put that file on the local machine.
#' @export
send_to_ihme <- function(session, filename, overwrite = TRUE, data_configuration = NULL) {
  if (is.null(data_configuration)) {
    config <- rampdata::data_configuration()
  } else {
    config <- data_configuration
  }
  source <- fs::path(config$LOCALDATA, filename)
  if (!file.exists(source)) {
    warning(paste("file", source, "does not exist\n"))
    return()
  }
  target_file <- fs::path(config$SCPHOSTBASE, filename)
  target_directory <- fs::path_dir(target_file)
  # fs::path makes paths for the local system, but the remote system
  # is always Unix.
  ssh::ssh_exec_internal(session, paste("mkdir -p", target_directory))
  stat_finds_file <- ssh::ssh_exec_internal(
    session,
    command = paste("stat", target_file),  # The result of the stat command will be the status.
    error = FALSE
    )
  if (overwrite | stat_finds_file$status == 1) {
    ssh::scp_upload(session, source, to = target_directory)
  } else {
    stop(paste("Did not transfer because this would overwrite:", filename))
  }
}




#' Retrieve worldpop data.
#'
#' This is an example of a dataset that can be retrieved with Curl
#' if we record its location.
#' Worldpop returns several GeoTIFFs in WGS 84.
#' GNQ = Equatorial Guinea
#' 10 or 15 is 2010 or 2015 data.
#' adjv2 or v2 is whether it was adjusted to match WHO.
#' So use GNQ15v2.tif.
#' @param local directory Where to put that file on the local machine.
#' @param overwrite Whether to overwrite an existing file by the same name.
#' @export
download_worldpop <- function(local_directory = "inst/extdata", overwrite = FALSE) {
  local_file <- fs::path(local_directory, "Equatorial_Guinea_100m_Population.7z")
  worldpop_directory <- fs::path_ext_remove(local_file)
  remote_url <- "ftp://ftp.worldpop.org.uk/GIS/Population/Individual_countries/GNQ/Equatorial_Guinea_100m_Population.7z"

  if (!file.exists(local_file)) {
    curl::curl_download(remote_url, local_file, mode = "wb")
  }
  if (!dir.exists(worldpop_directory)) {
    dir.create(worldpop_directory)
    un7zip(local_file, worldpop_directory)
  }
  dir(worldpop_directory)
}


#' Retrieve Bioko grid data.
#'
#' This is an example of what we have to do in order to transfer
#' zipped data.
#' Bioko grids are two shapefiles in UTM zone 32N projection.
#' The 100m grids are secs.shp and the 1km are mapareas.shp.
#' The two grids align in this projection.
#' @param session An ssh session object.
#' @param local directory Where to put that file on the local machine.
#' @export
download_bioko_grids <- function(session, local_directory = "inst/extdata") {
  filename <- "Bioko_grids.zip"
  local_path <- fs::path(local_directory, filename)
  destination_directory <- fs::path_ext_remove(local_path)
  if (!file.exists(local_path)) {
    get_from_ihme(session, filename)
  }
  if (!dir.exists(destination_directory)) {
    dir.create(destination_directory)
  }
  unzip(local_path, exdir = destination_directory)
}
