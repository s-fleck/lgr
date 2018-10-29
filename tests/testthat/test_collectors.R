context("collectors")


test_that("collectors works as expected", {

  col <- CollectorDefault$new(
    level = NA_integer_,
    timestamp = Sys.time,
    msg = NA_character_,
    caller = get_caller
  )

  expect_identical(col$last_value, NULL)


  col$log(level = 3, msg = "blubb")

  expect_identical(col$last_value$level, 3)
  expect_identical(col$last_value$msg, "blubb")


  col$log(level = 4, msg = "blubb")

  expect_equal(
    as.list(col$data[2, !".id"]),
    as.list(col$last_value)
  )

})
