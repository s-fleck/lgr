context("logger")


test_that("logger works as expected", {

  cs <- 93L

  log <- collector_dt$new(
    level = NA_integer_,
    timestamp = Sys.time,
    msg = NA_character_,
    .cache_size = cs
  )



  expect_identical(
    nrow(log$.__enclos_env__$private$data),
    cs
  )

})
