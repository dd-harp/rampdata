test_that("transfer finds initialization", {
  skip("not using ssh now")
  xdg_home <- fs::path(base::tempdir(), "xdg")
  dir.create(fs::path(xdg_home, "RAMP"), recursive = TRUE, showWarnings = FALSE)
  cat(
    "[Default]\nSCPHOST = testing.ihme.uw.edu\nSCPHOSTBASE = /home/borlaug/data\n",
    file = fs::path(xdg_home, "RAMP", "data.ini")
  )
  xdg_previous <- Sys.getenv("XDG")
  Sys.setenv(XDG_CONFIG_HOME = xdg_home)

  config <- data_configuration()
  expect_equal(config$SCPHOST, "testing.ihme.uw.edu")
  expect_equal(config$SCPHOSTBASE, "/home/borlaug/data")

  if (xdg_previous != "") {
    Sys.setenv(XDG_CONFIG_HOME = xdg_previous)
  } else {
    Sys.unsetenv("XDG_CONFIG_HOME")
  }
})


test_that("local file goes there and back", {
  skip("not using ssh now")
  # Set up the test file.
  test_config <- rampdata::data_configuration("Test")
  test_file_relative <- fs::path("working", "goes.txt")
  test_file_rooted <- fs::path(test_config$LOCALDATA, test_file_relative)
  play_dir <- fs::path_dir(test_file_rooted)
  dir.create(
    play_dir,
    recursive = TRUE,
    showWarnings = FALSE
    )
  cat("some odd stuff\n", file = test_file_rooted)
  expect_true(file.exists(test_file_rooted))

  # Send it there.
  capture.output({
      ssh_session <- ssh::ssh_connect(test_config$SCPHOST, verbose = 0)
    },
    type = "output"
  )
  send_to_ihme(ssh_session, test_file_relative, data_configuration = test_config)
  file.remove(test_file_rooted)
  expect_true(!file.exists(test_file_rooted))
  get_from_ihme(ssh_session, test_file_relative, data_configuration = test_config)
  expect_true(file.exists(test_file_rooted))
  ssh::ssh_disconnect(ssh_session)
  file.remove(test_file_rooted)
  unlink(play_dir)
})




test_that("overwrite raises an error", {
  skip("not using ssh now")
  # Set up the test file.
  test_config <- rampdata::data_configuration("Test")
  test_file_relative <- fs::path("overwrite", "overwrite.txt")
  test_file_rooted <- fs::path(test_config$LOCALDATA, test_file_relative)
  play_dir <- fs::path_dir(test_file_rooted)
  dir.create(
    play_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  cat("some odd stuff\n", file = test_file_rooted)
  expect_true(file.exists(test_file_rooted))

  # Send it there.
  capture.output({
    ssh_session <- ssh::ssh_connect(test_config$SCPHOST, verbose = 0)
  },
  type = "output"
  )
  send_to_ihme(ssh_session, test_file_relative, data_configuration = test_config)
  had_error <- FALSE
  tryCatch(
    send_to_ihme(
      ssh_session,
      test_file_relative,
      data_configuration = test_config,
      overwrite = FALSE
      ),
    error = function(c) {had_error <<- TRUE}
  )

  ssh::ssh_disconnect(ssh_session)
  file.remove(test_file_rooted)
  unlink(play_dir)
  expect_true(had_error)
})
