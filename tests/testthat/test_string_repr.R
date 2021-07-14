context("string_repr")


test_that("string_repr works as expected", {

  string_repr(mean)
  string_repr(function(x) print(x))
  string_repr(iris)
  string_repr(iris, 16)
  string_repr(as.matrix(iris), 32)


  string_repr(data.table::as.data.table(iris), 36)
  string_repr(data.table::as.data.table(iris), 22)
  expect_equal(nchar(string_repr(data.table::as.data.table(iris), 22)), 22)
  expect_equal(nchar(string_repr(data.table::as.data.table(iris), 8)), 8)

  expect_equal(nchar(string_repr(as.matrix(iris), 8)), 8)

  expect_lte(nchar(string_repr(letters, 16)), 16)

  string_repr(letters, 16)
  string_repr(1:100, 16)


  expect_lte(nchar(string_repr(iris, 16)), 16)
})
