context("print_LogEvent")



test_that("format.LogEvent works as expected", {
  x <- LogEvent$new(
    logger = lgr::lgr,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd.",
    blah = "blubb",
    numbers = 1:100,
    large_number = c(23.525325235213525235525, 930.824687923409867298367293406),
    iris = iris,
    logg = lgr::lgr,
    letters = letters
  )

  expect_output(print(x))
  expect_true(!crayon::has_style(format(x, colors = NULL)))
  expect_output(expect_identical(x, print(x)))
})




test_that("format.LogEvent works with colored %k and %K paramters", {
  old <- Sys.getenv("R_CLI_NUM_COLORS")
  Sys.setenv(R_CLI_NUM_COLORS = "256")
  on.exit(Sys.setenv(R_CLI_NUM_COLORS = as.character(old)))

  x <- LogEvent$new(
    level = 200,
    logger = lgr::lgr,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd.",
    blah = "blubb",
    numbers = 1:100,
    large_number = c(23.525325235213525235525, 930.824687923409867298367293406),
    iris = iris,
    logg = lgr::lgr,
    letters = letters
  )

  expect_silent(
    expect_true(
      crayon::has_style(format(x, fmt = "%k %m", colors = list(error = crayon::bgRed)))
    )
  )

  expect_silent(
    expect_true(
      crayon::has_style(format(x, fmt = "%K %m", colors = list(error = crayon::bgRed)))
    )
  )
})




test_that("toString.LogEvent works as expected", {
  x <- LogEvent$new(
    logger = lgr::lgr,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd.",
    blah = "blubb",
    numbers = 1:100,
    large_number = c(23.525325235213525235525, 930.824687923409867298367293406),
    iris = iris,
    logg = lgr::lgr,
    letters = letters
  )

  expect_length(toString(x), 1L)
  # just a rudimentary check because exact representation depends on console width
  expect_match(toString(x), "level.*letters")
})




test_that("toString.LogEvent without custom fields does not end in whitespace", {
  event_without_field <- LogEvent$new(
    logger = lgr::lgr,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd."
  )
  res <- format(event_without_field, fmt = "%m  %f", colors = NULL)
  expect_identical(
    substr(res, nchar(res), nchar(res)),
    "."
  )

  event_with_field <- LogEvent$new(
    logger = lgr::lgr,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd.",
    foo = "bar"
  )
  res <- format(event_with_field, fmt = "msg  %f", colors = NULL)
  expect_identical(
    substr(res, nchar(res), nchar(res)),
    "}"
  )
})
