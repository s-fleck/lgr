context("LogEvent")


test_that("LogEvent can have custom fields", {
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




test_that("LogEvents preserves field order", {
  l  <- Logger$new("l", propagate = FALSE)

  l$fatal("test", c = "1", a = "2", b = "3")

  # Order depends on the internal implementation of environments I guess...
  # let's see if this will break one day.
  expect_identical(names(l$last_event)[1:3], c("c", "a", "b"))
})






# as.data.frame/table/tibble ---------------------------------------------

as_funs <- list(
  as.data.frame = as.data.frame.LogEvent,
  as.data.table = as.data.table.LogEvent,
  as_tibble = as_tibble.LogEvent
)

# nm <- "as.data.table"

for (nm in names(as_funs)){

  l  <- Logger$new("l", propagate = FALSE)

  test_that(paste0(nm, "() works as expected"), {
    l$fatal("test", df = iris, root_logger = lgr)
    res <- as_funs[[nm]](l$last_event)

    expect_identical(nrow(res), 1L)
    expect_true(is.data.frame(res$df[[1]]))
    expect_identical(res$root_logger[[1]], lgr)
    expect_identical(nrow(res$df[[1]]), 150L)
  })


  test_that(paste0(nm, "() vectorizes over msg"), {
    l  <- Logger$new("l", propagate = FALSE)
    l$fatal(c("test", "test2"), letters = letters, df = iris, root_logger = lgr)

    res <- as_funs[[nm]](l$last_event)

    expect_identical(nrow(res), 2L)
    expect_true(is.data.frame(res$df[[1]]))
    expect_true(is.data.frame(res$df[[2]]))
    expect_true(is.character(res$letters[[1]]))
    expect_true(is.character(res$letters[[2]]))
    expect_identical(nrow(res$df[[1]]), 150L)
    expect_identical(nrow(res$df[[2]]), 150L)
  })
}
