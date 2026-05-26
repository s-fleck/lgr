get_local_console_logger <- function(env = parent.frame()) {
  withr::defer(get_logger("test", reset = TRUE), envir = env)

  lg <- get_logger("test", reset = TRUE)
  lg$add_appender(AppenderConsole$new(threshold = NA), "console")
  lg
}


test_that("LogEvent can have custom fields", {

  l <- get_local_console_logger()


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
  l <- get_local_console_logger()

  capture.output(
    l$fatal("test", c = "1", a = "2", b = "3")
  )

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

  test_that(paste0(nm, "() works as expected"), {
    l <- get_local_console_logger()

    capture.output(
      l$fatal("test", df = iris, root_logger = lgr)
    )

    res <- as_funs[[nm]](l$last_event)

    expect_identical(nrow(res), 1L)
    expect_true(is.data.frame(res$df[[1]]))
    expect_identical(res$root_logger[[1]], lgr)
    expect_identical(nrow(res$df[[1]]), 150L)
  })


  test_that(paste0(nm, "() vectorizes over msg"), {
    l <- get_local_console_logger()

    capture.output(
      l$fatal(c("test", "test2"), letters = letters, df = iris, root_logger = lgr)
    )

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





test_that("as_LogEvent.list and as_LogEvent.data.frame work", {

  ts <- Sys.time()

  l <- list(
    level = 100,
    msg = "blah",
    timestamp = ts,
    caller = NA,
    foo = "bar"
  )

  Sys.sleep(1)
  expect_identical(as_LogEvent(l)$timestamp, l$timestamp)
  expect_s3_class(as_LogEvent(l), "LogEvent")
  expect_s3_class(as_LogEvent(as.data.frame(l)), "LogEvent")


  l <- list(
    level = 100,
    msg = "blah",
    `@timestamp` = ts,
    caller = NA,
    foo = "bar"
  )

  expect_identical(as_LogEvent(l)[["timestamp"]], l[["@timestamp"]])
  expect_s3_class(as_LogEvent(l), "LogEvent")
  expect_s3_class(as_LogEvent(as.data.frame(l)), "LogEvent")
})
