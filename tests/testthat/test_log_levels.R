context("simple_logging")


test_that("add_log_levels(), get_log_levels() works as expected", {
  ml <- Logger$new("dummy")

  expect_error(ml$set_threshold("blubb"))
  expect_silent(add_log_levels(c(blubb = 250, schwupp = 341)))
  expect_silent(ml$set_threshold("blubb"))
  expect_identical(ml$threshold, 250L)
  expect_setequal(
    names(get_log_levels()),
    c("fatal", "error", "warn", "info", "debug", "trace", "blubb", "schwupp")
  )

  expect_error(
    add_log_levels(c("blubb", "schwupp"))
  )

  expect_message(add_log_levels(c(blubb = 250, schwupp = 341)))
  expect_true(all_are_distinct(get_log_levels()))
  expect_true(all_are_distinct(names(get_log_levels())))

  remove_log_levels(c("blubb", "schwupp"))  # cleanup
  expect_error(ml$set_threshold("blubb"))
})




test_that("label_levels() & unlabel_levels() are symetirc", {
  tdat <- sample(c(seq(0, 600, by = 100), NA))

  expect_silent({
    x <- label_levels(tdat)
    expect_equal(names(x), as.character(tdat))
    y <- unlabel_levels(x)
    expect_equal(unname(y), tdat)
    expect_equal(names(y), unname(x))
  })

  expect_silent(label_levels(NA_integer_))
  expect_warning(label_levels(700))
  expect_warning(unlabel_levels(NA_character_))
  expect_warning(unlabel_levels("foo"))

  num <- c(93L, 200L)
  expect_warning(chr <- label_levels(num))
  expect_warning(
    expect_identical(unname(unlabel_levels(chr)), num)
  )
})




test_that("colorize_levels() works as expected", {
  if (!crayon::has_color()) skip("Terminal does not support colors")
  # set colors manualy so that we also check on systems without real color support
  colors <- list(
    "fatal" = crayon::red,
    "error" = crayon::blue,
    "warn"  = crayon::yellow,
    "debug" = crayon::silver,
    "trace" = crayon::white
  )

  tdat <- setdiff(sample(seq(100, 600, by = 100)), 400)  # 400/info is not colored
  tr1  <- colorize_levels(tdat, colors = colors)
  walk(tr1, function(.x) expect_true(crayon::has_style(.x), info = .x))
  tr2 <- colorize_levels(label_levels(tdat), colors = colors)
  walk(tr2, function(.x) expect_true(crayon::has_style(.x), info = .x))

  expect_true(is.null(colorize_levels(NULL)))
  expect_identical(colorize_levels(integer()), integer())
  expect_identical(colorize_levels(character()), character())
  expect_identical(colorize_levels(tdat, NULL), tdat)
})




test_that("named_union() works as expected", {
  x <- c("blah" = "blubb", "fizz" = "buzz")
  y <- c("foo" = "bar", "blah2" = "blubb")

  r <- named_union(x, y)

  expect_identical(
    r,
    setNames(c("blubb", "buzz", "bar"), c("foo", "fizz", "blah2"))
  )
})




test_that("standardize_* doc examples", {

  expect_identical(standardize_threshold("info"), 400L)
  expect_true(is.na(standardize_threshold("all")))

  expect_identical(standardize_log_level("info"), 400L)
  expect_error(is.na(standardize_log_level("all")))

  expect_error(standardize_log_level(c("info", "fatal")))
  expect_identical(standardize_log_levels(c("info", "fatal")), c(400L, 100L))




})
