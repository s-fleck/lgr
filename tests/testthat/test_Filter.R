context("Filter")


test_that("FilterForceLevel and FilterInject work as expected", {

  l <- Logger$new("foo")

  l$add_filter(FilterForceLevel$new("fatal"))
  l$add_filter(FilterInject$new(iris = iris, .list = list(cars = "foo")))
  expect_output(l$info("blubb"), "FATAL.*cars.*data\\.frame")
  l$remove_filter(1)
  expect_output(l$info("blubb"), "INFO.*cars.*data\\.frame")
  l$remove_filter(1)
  expect_output(l$info("test"), "test\\s*$")
})



test_that(".obj() works as expected", {
  l <- Logger$new("foo")

  f <- function(event) {
    cat(class_fmt(.obj()))
    TRUE
  }

  l$add_filter(f)

  expect_output(l$fatal("test"), "Logger/Filterable/R6")
  l$remove_filter(1)
  l$add_filter(EventFilter$new(f))
  expect_output(l$fatal("test"), "Logger/Filterable/R6")
})



test_that("FilterForceLevel and FilterInject work inside function", {
  lg <- get_logger("test")
  lg$set_threshold(NA)
  lg$set_propagate(FALSE)
  lg$add_appender(AppenderConsole$new(threshold = NA))

  analyse <- function(){
    lg$add_filter(FilterForceLevel$new("info"), "force")
    lg$add_filter(FilterInject$new(type = "analysis"), "inject")
    on.exit(lg$remove_filter(c("force", "inject")))
    lg$debug("a debug message")
    lg$error("an error")
  }
  expect_output(analyse(), "INFO.*debug.*type:.*INFO.*error.*type:.*")
  expect_length(lgr$filters, 0)
  lg$config(logger_config())
})
