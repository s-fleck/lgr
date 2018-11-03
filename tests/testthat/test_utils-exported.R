context("utils-exported")


test_that("utils-exported works as expected", {
  ml <- Logger$new(string_formatter = sprintf_safely)
  ml$info("blub %s")
})
