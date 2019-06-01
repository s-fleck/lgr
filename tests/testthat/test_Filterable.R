context("Filterable")


test_that("is_filter() works", {
  fil <- function(event) { blubb }
  expect_true(is_filter(fil))
  expect_false(is_filter(mean))
  expect_false(is_filter("blubb"))

  assert_filter(fil)
  expect_error(
    assert_filter(mean),
    "mean"
  )
})




test_that("Filterable: $add_filter(), $remove_filter(), $filters", {
  app <- Appender$new()

  fil <- function(event) { FALSE }

  expect_error(app$add_filter(mean))

  app$add_filter(fil)
  expect_identical(app$filters[[1]], fil)
  expect_false(app$filter())

  app$remove_filter(1)
  expect_length(app$filters, 0)
  expect_true(app$filter())

  app$add_filter(fil, "foo")
  expect_identical(app$filters[[1]], fil)
  app$remove_filter("foo")
  expect_length(app$filters, 0)


  fil <- function(event) { NA }
  app$add_filter(fil, "foo")
  app$add_filter(fil, "foo")
  expect_warning(expect_true(app$filter()))
})
