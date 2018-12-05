context("layouts")


x <- LogEvent$new(
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

  lo$timestamp_fmt <- "%Y-%m-%d"
  lo$fmt <- "[%t]"
  expect_identical(lo$format_event(x), "[2018-11-02]")
})




test_that("LayoutGlue works as expected", {
  skip("layoutGlue not yet operational")
  lo <- LayoutGlue$new()
  expect_equal(lo$format_event(x), "ERROR .*2018-11-02 .*:19:33.* foo bar")
})




test_that("LayoutJson works as expected", {
  lo <- LayoutJson$new(
    logger_vals = "user",
    other_vals = list(pid = Sys.getpid, teststring = "blah")
  )

  eres <- c(x$values, user = x$logger$user)
  json <- lo$format_event(x)
  tres <- jsonlite::fromJSON(json)


  tres[sapply(tres, is.null)] <- NA_character_

  expect_setequal(c(names(eres), "pid", "teststring"), names(tres))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["user"]], eres[["user"]])
  expect_identical(tres[["caller"]], eres[["caller"]])
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["pid"]], Sys.getpid())
  expect_equal(tres[["teststring"]], "blah")

})
