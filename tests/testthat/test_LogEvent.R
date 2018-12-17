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
