context("simple_logging")


test_that("simple_logging works as expected", {
  ml <- Logger$new("dummy")

  expect_error(ml$set_threshold("blubb"))
  add_log_levels(c(blubb = 250, schwupp = 341))
  expect_silent(ml$set_threshold("blubb"))
  expect_identical(ml$threshold, 250L)
  expect_setequal(
    names(get_log_levels()),
    c("fatal", "error", "warn", "info", "debug", "trace", "blubb", "schwupp")
  )
  remove_log_levels(c("blubb", "schwupp"))  # cleanup
  expect_error(ml$set_threshold("blubb"))
})




test_that("labelling/unlabelling log levels is symetirc", {
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




test_that("colorize log levels works", {
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
