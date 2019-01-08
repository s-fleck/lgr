context("Filter")


test_that("Filter works as expected", {

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



test_that("preset filters work", {
  analyse <- function(){
    lgr$add_filter(FilterForceLevel$new("info"), "force")
    lgr$add_filter(FilterInject$new(type = "analysis"), "inject")
    on.exit(lgr$remove_filter(c("force", "inject")))
    lgr$debug("a debug message")
    lgr$error("an error")
  }
  expect_output(analyse(), "INFO.*debug.*type:.*INFO.*error.*type:.*")
  expect_length(lgr$filters, 0)
})



