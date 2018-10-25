context("memlog")


test_that("basic logging", {
  ml <- memlog$new()
  ts <- structure(1540486764.41946, class = c("POSIXct", "POSIXt"))

  testfun <- function(){
    ml$log(level = 4L, timestamp = ts, msg = "test", user = "test_user", pid = 001, caller = "test_function")
  }

  expect_output({
    testfun()
    testfun()
  })

  expect_true(
    data.table::is.data.table(ml$showdt())
  )

  expect_true(nrow(ml$showdt()) == 2L)

  fatalfun <- function() ml$fatal("test ftl")
  expect_output(fatalfun(), "FATAL")
  expect_true(ml$showdt()[3]$caller == "fatalfun")
})



test_that("test memory allocation", {
  ml <- memlog$new()

  expect_output({
    for (i in 1:100) ml$fatal("blubb")
  })

  expect_identical(nrow(ml$showdt(99, 1000)), 100L)
  expect_output(ml$fatal("blubb"))
  expect_identical(nrow(ml$showdt(99, 1000)), 101L)
  expect_identical(nrow(ml$.__enclos_env__$private$data), 200L)
})
