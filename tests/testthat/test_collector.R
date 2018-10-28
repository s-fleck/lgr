context("logger")


test_that("logger works as expected", {

  cs <- 93L

  log <- CollectorDefault$new(
    level = NA_integer_,
    timestamp = Sys.time,
    msg = NA_character_,
    .cache_size = cs
  )

  expect_identical(
    nrow(log$data),
    cs
  )

})
