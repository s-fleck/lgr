context("logger_tree")


testthat::setup(remove_all_loggers())
testthat::teardown(remove_all_loggers())


test_that("logger_tree() works as expected", {
  on.exit(remove_all_loggers())

  get_logger("blah/blubb/foo")
  get_logger("blah/blubb/bar")
  expect_setequal(logger_tree()$parent, c("root", "blah", "blubb", "foo", "bar"))
  remove_all_loggers()
  expect_identical(logger_tree()$parent, "root")
})




test_that("logger_tree() detects logger properties", {
  on.exit(remove_all_loggers())
  get_logger("blah/blubb/foo")
  get_logger("blah/blubb/bar")

  get_logger("blah/blubb")$set_propagate(FALSE)
  get_logger("blah/blubb")$set_appenders(
    list(AppenderConsole$new(), AppenderConsole$new())
  )

  res <- logger_tree()

  expect_false(res$propagate[res$parent == "blubb"])
  expect_identical(res$n_appenders[res$parent == "blubb"], 2L)

  expect_true(all(res$propagate[res$parent != "blubb"]))
  expect_true(all(res$n_appenders[!res$parent %in% c("blubb", "root")] == 0))
})




test_that("logger_tree() doc examples", {
  on.exit(remove_all_loggers())

  get_logger("fancymodel")
  get_logger("fancymodel/shiny")$
    set_propagate(FALSE)

  get_logger("fancymodel/shiny/ui")$
    set_appenders(AppenderConsole$new())

  get_logger("fancymodel/shiny/server")$
    set_appenders(list(AppenderConsole$new(), AppenderConsole$new()))$
    set_threshold("trace")

  get_logger("fancymodel/plumber")

  out <- utils::capture.output(logger_tree())
  expect_identical(length(out), 6L)
})
