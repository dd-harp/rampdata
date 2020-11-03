testing_config <- list(LOCALDATA = "/rooted")
test_that("ramp path recognizes parts", {
  expect_equal(as.path(ramp_path("/proj/stage/dataset/version/sub/file"), config = testing_config),
               "/rooted/projects/proj/stage/dataset/version/sub/file")
  expect_equal(as.path(ramp_path("/proj/stage/dataset/version"), config = testing_config),
               "/rooted/projects/proj/stage/dataset/version")
  expect_equal(as.path(ramp_path("/proj/stage/dataset"), config = testing_config),
               "/rooted/projects/proj/stage/dataset")
})


test_that("ramp path can modify an rpath", {
  rp <- ramp_path()
  rp2 <- add_path(rp, dataset = "violet")
  rp2 <- add_path(rp2, project = "uga", stage = "mic")
  expect_equal(as.path(rp2, testing_config), "/rooted/projects/uga/mic/violet")
  rp3 <- add_path(rp2, file = "blah.csv", version = "23")
  expect_equal(as.path(rp3, testing_config), "/rooted/projects/uga/mic/violet/23/blah.csv")
})


test_list <- function(trials) {
  for (idx in 1:length(trials)) {
    trial <- trials[[idx]]
    trial_name <- paste("ramp_path obeys list", names(trials)[idx])

    test_that(trial_name, {
      had_error <- FALSE
      pp <- tryCatch({
        ramp_path1(
          stage = trial[["stage"]], location = NULL, project = trial[["project"]],
          user = trial[["user"]], rproject = trial[["rproject"]]
          )
      }, error = function(e) had_error <<- TRUE
      )
      if (!is.null(trial[["result"]])) {
        expect_equal(pp$path, trial[["result"]])
        expect_equal(had_error, FALSE)
      } else {
        expect_equal(had_error, TRUE)
      }
    })
  }
}


test_inverse_list <- function(trials) {
  # Select those paths which don't cause exceptions.
  trials <- trials[sapply(trials, function(x) !is.null(x$result))]

  for (idx in 1:length(trials)) {
    trial <- trials[[idx]]
    trial_name <- paste("inverse_ramp_path parses path", names(trials)[idx])
    expected <- trial[unlist(lapply(trial, function(x) !is.null(x)))]
    expected <- expected[!names(expected) %in% "result"]
    test_that(trial_name, {
      had_error <- FALSE
      pp <- tryCatch({
        inverse_ramp_path1(trial[["result"]])
      }, error = function(e) had_error <<- TRUE
      )
      expect_false(had_error)
      # Compare lists by comparing names and then values.
      expect_equal(sort(names(pp)), sort(names(expected)))
      expect_equal(pp[sort(names(pp))], expected[sort(names(expected))])
    })
  }
}


# List an output for specified inputs.
# Ignore location and path arguments b/c we know their handling is simple.
# A NULL means we expect a stop condition.
# Trials are (3 x stage, 2 x project, 2 x user, 2 x rproject) = 24.
trials <- list(
  missing_stage = list(
    stage = NULL, project = "hi", user = "me", rproject = "always", result = NULL
  ),
  stage_in = list(
    stage = "input", project = NULL, user = NULL, rproject = NULL, result = "inputs"),
  stage_out = list(
    stage = "output", project = NULL, user = NULL, rproject = NULL, result = NULL),
  stage_working = list(
    stage = "working", project = NULL, user = NULL, rproject = NULL, result = NULL),
  stage_working_user = list(
    stage = "working", project = NULL, user = "ad", rproject = NULL, result = "users/ad"),
  stage_in_p = list(
    stage = "input", project = "ugp", user = NULL, rproject = NULL, result = "projects/ugp/inputs"),
  stage_out_p = list(
    stage = "output", project = "ugp", user = NULL, rproject = NULL, result = "projects/ugp/outputs"),
  stage_working_p = list(
    stage = "working", project = "ugp", user = NULL, rproject = NULL, result = NULL),
  stage_working_p_u = list(
    stage = "working", project = "ugp", user = "ad", rproject = NULL, result = "projects/ugp/users/ad"),
  stage_working_r = list(
    stage = "working", project = NULL, user = NULL, rproject = "rp", result = "libraries/rp/inst/extdata"),
  stage_working_r_user = list(
    stage = "working", project = NULL, user = "ad", rproject = "rp", result = "libraries/rp/inst/extdata/users/ad")
)
# These trials don't work in reverse because the stage is inferred as "working."
forward_only_trials <- list(
  stage_out_user = list(
    stage = "output", project = NULL, user = "ad", rproject = NULL, result = "users/ad"),
  stage_in_r = list(
    stage = "input", project = NULL, user = NULL, rproject = "rp", result = "libraries/rp/inst/extdata"),
  stage_out_r = list(
    stage = "output", project = NULL, user = NULL, rproject = "rp", result = "libraries/rp/inst/extdata")
)
test_list(trials)
test_inverse_list(trials)
test_list(forward_only_trials)
