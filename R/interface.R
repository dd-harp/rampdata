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