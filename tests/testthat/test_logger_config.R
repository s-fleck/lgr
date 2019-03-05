context("logger_config")


test_that("logger_config works as expected", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_full.yaml")
  cfg <- as_logger_config(ty)
})




test_that("resolve_r6_ctors", {

  x <- list(
    "Logger" = list(
      name = "test",
      appenders = list(
        "AppenderFile" = list(
          file = tempfile()
        )
      )
    )
  )

  resolve_r6_ctors(x)


  x <- list(
    "Logger" = list(
      name = "test2",
      appenders = list(
        "AppenderBuffer" = list(
          threshold = NA,
          appenders = list(
            "AppenderJson" = list(threshold = 100, file = tempfile()),
            "AppenderFile" = list(file = tempfile()),
            "Appender" = list()
          )
        )
      )
    )
  )

  resolve_r6_ctors(x)

})
