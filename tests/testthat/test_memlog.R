context("memlog")


test_that("basic logging", {
  ml <- memlog$new()
  ts <- structure(1540486764.41946, class = c("POSIXct", "POSIXt"))

  testfun <- function(){
    ml$fatal("blubb")
  }

  expect_output({
    testfun()
    testfun()
  })

  expect_true(
    data.table::is.data.table(ml$showdt())
  )

  expect_true(nrow(ml$showdt()) == 2L)

  fatalfun <- function() ml$fatal("test ftl")
  expect_output(fatalfun(), "FATAL")
  expect_true(grepl("fatalfun", ml$showdt()[3]$caller))
})




test_that("memory cycling works", {
  ml <- memlog$new(collector = collector_dt$new(
    level = NA_integer_,
    msg = NA_character_,
    .cache_size = 10)
  )

  expect_output(replicate(12, ml$info("blubb info")))
  expect_equal(ml$showdt()$id, 3:12)
})
