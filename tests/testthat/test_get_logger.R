context("get_logger")


test_that("get_logger works as expected", {

  lg <- get_logger("blubb")

  expect_identical(lg$name, "blubb")
  expect_identical(lg$name, "blubb")
  expect_identical(lg$parent, lgr)

  lg1 <- get_logger("fizz")
  lg3 <- get_logger("fizz/buzz/wuzz")

  expect_true(exists_logger("fizz/buzz"))

  lg2 <- get_logger("fizz/buzz")

  expect_identical(lg3$parent, lg2)
  expect_identical(lg2$parent, lg1)
  expect_identical(lg1$parent, lgr::lgr)

  lg2$set_propagate(FALSE)

  expect_equal(
    unname(unclass(lg3$ancestry)),
    c(TRUE, FALSE, TRUE)
  )

  expect_equal(
    names(lg3$ancestry),
    c("fizz", "buzz", "wuzz")
  )
})
