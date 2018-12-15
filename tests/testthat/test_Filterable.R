context("Filterable")


test_that("Filterable works as expected", {
  fil <- function(event, obj) { blubb }
  expect_true(is_filter(fil))
  expect_false(is_filter(mean))
  expect_false(is_filter("blubb"))
})
