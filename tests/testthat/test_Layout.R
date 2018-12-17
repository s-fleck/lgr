context("Layout")


tevent <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
  caller = NA_character_,
  msg = "foo bar"
)




test_that("layouts works as expected", {
  lo <- Layout$new()
  expect_match(lo$format_event(x), "19:33")
  expect_true(is_scalar_character(lo$format_event(x)))
})



test_that("LayoutFormat works as expected", {
  lo <- LayoutFormat$new()
  expect_match(lo$format_event(x), "ERROR .*2018-11-02 .*:19:33.*\\.* foo bar")

  lo$set_timestamp_fmt("%Y-%m-%d")
  lo$set_fmt("[%t]")
  expect_identical(lo$format_event(x), "[2018-11-02]")
})



test_that("LayoutDbi works as expected", {
  col_types <-  c(
    timestamp = "timestamp",
    level = "smallint",
    msg = "varchar(1024)",
    user = "varchar(256)",
    foo = "varchar(255)"
  )

  expect_error(
    lo <- LayoutDbi$new(
      event_vals = c("level", "timestamp", "msg"),
      logger_vals = "user",
      col_types = col_types
    ), "foo"
  )

  expect_error(
    lo <- LayoutDbi$new(
      event_vals = c("level", "timestamp", "msg", "foo", "caller"),
      logger_vals = "user",
      col_types = col_types
    ), "caller"
  )

  lo <- LayoutDbi$new(
    event_vals = c("level", "timestamp", "msg", "foo"),
    logger_vals = "user",
    col_types = col_types
  )

  x <- tevent$clone()
  x$foo <- "bar"

  eres <- c(x$values, user = x$logger$user)
  tres <- lo$format_event(x)

  tres[sapply(tres, is.null)] <- NA_character_
  expect_setequal(names(eres), c(names(tres), "caller"))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["user"]], eres[["user"]])
  expect_true(is.null(tres[["caller"]]))
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")
})


test_that("LayoutJson works as expected", {
  lo <- LayoutJson$new(
    logger_vals = "user"
  )

  x <- tevent$clone()
  x$foo <- "bar"

  eres <- c(x$values, user = x$logger$user, foo = "bar")
  json <- lo$format_event(x)
  tres <- jsonlite::fromJSON(json)


  tres[sapply(tres, is.null)] <- NA_character_
  expect_setequal(c(names(eres), "foo"), names(tres))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["user"]], eres[["user"]])
  expect_identical(tres[["caller"]], eres[["caller"]])
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")
})
