context("LogEvent")


test_that("custom LogEvents", {
  l  <- Logger$new("l")

  expect_output(l$log(100, "blubb", user_agent = "007"))
  expect_identical(l$last_event$values$user_agent, "007")

  expect_warning(
    l$log(100, "blubb", timestamp = Sys.time(), caller = "none",  "007"),
    "must be named"
  )
})
