testing_config <- list(LOCALDATA = "/rooted")

test_that("workflow can read", {
  fn <- tempfile()
  text <- "[versions]\npopver=\"27\"\n[roles]\nadmin1pop=\"/uga/mic/admin1/{popver}/pop.csv\"\n"
  value <- tryCatch({
    cat(text, file = fn)
    initialize_workflow(fn)
    workflow_path("admin1pop")
  }, finally = {
    file.remove(fn)
  })
  rpath <- as.path(value, config = testing_config)
  expect_equal(rpath, "/rooted/projects/uga/mic/admin1/27/pop.csv")
})
