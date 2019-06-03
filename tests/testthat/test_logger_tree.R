context("logger_tree")


test_that("logger_tree works as expected", {
  skip("skip")
  get_logger("blah/blubb/swhupp")
  get_logger("blah/blubb/woop")

  logger_tree()

  cli::tree(res, root = "root")
})
