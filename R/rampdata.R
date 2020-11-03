#' rampdata: put and get data for RAMP calculations
#'
#' This package is responsible for transfer of data for
#' processing by RAMP projects. Data is either on the institute's
#' repository or on cloud storate. This transfers that data to
#' your machine on request, or it transfers that data from your
#' machine to the appropriate place on the repository.
#'
#' There is a vignette with an example notebook using rampdata:
#' \code{vignette("ramp_example", package = "rampdata")}
#'
#' @section User Interface Methods:
#'
#' \itemize{
#'   \item \link{local_path} - The fully-rooted local path to a file.
#'   \item \link{ensure_present} - Ensure files are available locally.
#'   \item \link{ensure_on_server} - Ensure a set of local files are on the server.
#'   \item \link{data_configuration} - Reads configuration file on where to download data.
#'   \item \link{save_source} - Save information about a directory or data file.
#' }
#'
#'
#' @section Local Path Implementation:
#'
#' \itemize{
#'   \item \link{ramp_path} - Formats a base path for a file given all keys.
#'   \item \link{inverse_ramp_path} - Turns a relative path into a ramp path.
#'   \item \link{project_path} - Creates a path relative to the project root.
#' }
#'
#'
#' @section Transfer with Cluster:
#'
#' \itemize{
#'   \item \link{get_from_ihme} - Use scp to retrieve data.
#'   \item \link{send_to_ihme} - Use scp to send data.
#'   \item \link{un7zip} - Unzip a 7zip file.
#'   \item \link{download_worldpop} - Retrieve worldpop data.
#'   \item \link{download_bioko_grids} - Retrieve Bioko grid data.
#' }
#'
#' @docType package
#' @name rampdata
#' @import futile.logger
NULL
