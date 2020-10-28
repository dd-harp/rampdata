#' Formats a base path for a file given all keys.
#'
#' This function reflects how we want to organize files.
#' It returns a path relative to whatever is the
#' data root.
#'
#' @param stage One of (input, working, output). Must not be NULL.
#' @param location If this is associated with a location. Can be NULL.
#' @param project The Github project or R project name, without a slash.
#'   Can be NULL.
#' @param user The username for the person's working directory.
#' @param rproject The Rstudio project name.
#' @return a path to use as a base path for this dataset.
#' @export
ramp_path1 <- function(stage, location, project, user, rproject, path = NULL) {
  stopifnot(stage %in% c("input", "output", "working"))

  used <- list()
  # Keep track of which tokens were used and which weren't.
  using <- function(key) {
    if (!is.null(key)) {
      used[key] <<- TRUE
      return(TRUE)
    }
    FALSE
  }

  using(stage)
  if (using(project)) {
    if (stage == "input") {
      base <- fs::path("projects", project, "inputs")
    } else if (stage == "output") {
      base <- fs::path("projects", project, "outputs")
    } else {
      # stage == working
      if (using(user)) {
        base <- fs::path("projects", project, "users", user)
      } else {
        stop("Must have username to put working data under a project.")
      }
    }
  } else if (using(rproject)) {
    base <- fs::path("libraries", rproject, "inst", "extdata")
    if (using(user)) {
      base <- fs::path(base, "users", user)
    }
  } else {
    if (stage == "input") {
      base <- fs::path("inputs")
    } else {
      if (using(user)) {
        base <- fs::path("users", user)
      } else {
        stop("Nowhere to put a file that has no project, rproject, or user
             and is an output or working.")
      }
    }
  }

  # Location is tacked onto the end if it's used.
  if (using(location)) {
    base <- fs::path(base, "locations", location)
  } # else don't tack a location on the end.
  if (using(path)) {
    base <- fs::path(base, path)
  }
  list(path = base, used = used)
}


#' Turns a relative path into a ramp path.
#'
#' @param path A path relative to the base data directory.
#' @return A list that is a ramp path.
#' @examples
#' inverse_ramp_path("users/ad") == list(stage = "working", user = "ad")
#' @export
inverse_ramp_path1 <- function(path) {
  splitted <- fs::path_split(path)[[1]]
  ramp_path <- list()
  if (splitted[1] == "inputs") {
    ramp_path["stage"] <- "input"
    rest_idx <- 2
  } else if (splitted[1] == "users") {
    ramp_path["user"] <- splitted[2]
    ramp_path["stage"] <- "working"
    rest_idx <- 3
  } else if (splitted[1] == "projects") {
    ramp_path["project"] <- splitted[2]
    if (splitted[3] == "inputs") {
      ramp_path["stage"] <- "input"
      rest_idx <- 4
    } else if (splitted[3] == "outputs") {
      ramp_path["stage"] <- "output"
      rest_idx <- 4
    } else if (splitted[3] == "users") {
      ramp_path["stage"] <- "working"
      ramp_path["user"] <- splitted[4]
      rest_idx <- 5
    } else {
      stop("project directory must be inputs, outputs, or users")
    }
  } else if (splitted[1] == "libraries") {
    ramp_path["rproject"] <- splitted[2]
    ramp_path["stage"] <- "working"
    if (length(splitted) > 4 & splitted[5] == "users") {
      ramp_path["user"] <- splitted[6]
      rest_idx <- 7
    } else {
      rest_idx <- 3
    }
  }

  # add location
  if (length(ramp_path) >= rest_idx & ramp_path[rest_idx] == "location") {
    ramp_path["location"] <- ramp_path[rest_idx + 1]
    rest_idx <- rest_idx + 2
  }
  # add path
  if (length(ramp_path) >= rest_idx) {
    ramp_path["path"] <- ramp_path[rest_idx:length(ramp_path)]
  }
  ramp_path
}


#' Creates a path relative to the project root
#'
#' @param file_id A list with keys (stage, location, project, user, rproject).
#'   If user is missing, the effective user is supplied.
#'   Only stage is required.
#' @return a path to use as a base path for this dataset.
#' @seealso \link{ramp_path}
#' @export
project_path <- function(file_id) {
  keys <- names(file_id)
  if (!"user" %in% keys | is.null(file_id$user)) {
    user <- Sys.info()[["effective_user"]]
  } else {
    user <- file_id$user
  }
  complete <- list(
    user = user, stage = NULL, location = NULL, project = NULL, rproject = NULL, path = NULL
    )
  for (name in names(file_id)) {
    complete[[name]] <- file_id[[name]]
  }
  do.call(ramp_path1, complete)$path
}
