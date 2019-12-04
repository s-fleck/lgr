context("print_Appender")




test_that("all Appenders print() without failure", {
  if (!requireNamespace("rotor")){
    skip("Required packages not installed")
  }

  tf <- tempfile()
  on.exit(unlink(tf))

  expect_output({
    print(Appender$new())
    print(AppenderConsole$new())
    print(AppenderConsole$new(layout = LayoutGlue$new()))
    print(AppenderBuffer$new(appenders = list(
      AppenderConsole$new(),
      blah = AppenderBuffer$new()
    )))
    print(AppenderFileRotating$new(tf))
    print(AppenderFileRotating$new(tf, size = "1kb"))
    app <- AppenderFileRotatingDate$new(tf, layout = LayoutJson$new())
    app$rotate(force = TRUE)
    print(app)
    app$prune(0)

    print(AppenderFileRotatingTime$new(
      tf,
      layout = LayoutJson$new(),
      age = "2 years",
      size = 90000)
    )
    print(AppenderFile$new(tf))
  })
})
