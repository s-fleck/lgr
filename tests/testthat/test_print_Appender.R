context("print_Appender")




test_that("print_Appender works as expected", {
  tf <- tempfile()
  on.exit(unlink(tf))

  Appender$new()
  AppenderConsole$new()
  AppenderConsole$new(layout = LayoutGlue$new())
  AppenderDbi$new(RSQLite::dbConnect(RSQLite::SQLite()), "blubb")
  AppenderMemory$new()
  AppenderMail$new()
  AppenderSendmail$new("test@blah.com", control = list())
  AppenderTable$new()
  AppenderBuffer$new(appenders = list(
    AppenderConsole$new(),
    blah = AppenderBuffer$new()
  ))
  AppenderPushbullet$new(recipients = "blubb")
  AppenderFileRotating$new(tf)
  AppenderFileRotating$new(tf, size = "1kb")
  app <- AppenderFileRotatingDate$new(tf, layout = LayoutJson$new())
  app$rotate(force = TRUE)
  app


  AppenderFileRotatingTime$new(tf, layout = LayoutJson$new(), age = "2 years", size = 90000)
  AppenderFile$new(tf)

})
