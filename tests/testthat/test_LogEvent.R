context("LogEvent")


test_that("custom LogEvents", {
  l  <- Logger$new("l")

  expect_output(l$log(100, "blubb", user_agent = "007"))
  expect_identical(l$last_event$values$user_agent, "007")

  expect_output(
    l$info("blubb %s -", "blah", user_agent = "008"),
    "blubb blah"
  )

  expect_identical(l$last_event$values$user_agent, "008")
  expect_identical(l$last_event$values$msg, "blubb blah -")
})




test_that("as.data.table and as.data.frame work with list columns", {
  l  <- Logger$new("l", propagate = FALSE)
  l$fatal("test", df = iris)
  dte <- data.table::as.data.table(l$last_event)
  dfe <- as.data.frame(l$last_event)
  dtb <- tibble::as_tibble(l$last_event)
  expect_true(is.data.frame(dte$df[[1]]))
  expect_true(is.data.frame(dfe$df[[1]]))
  expect_true(is.data.frame(dtb$df[[1]]))
})
