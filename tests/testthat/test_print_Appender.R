context("print_Appender")


test_that("print_Appender works as expected", {
  tf <- tempfile()
  on.exit(unlink(tf))


  c <- AppenderConsole$new()
  c

  d <- AppenderDbi$new(RSQLite::dbConnect(RSQLite::SQLite()), "blubb")
  d

  b <- AppenderBuffer$new(appenders = list(
    AppenderConsole$new(),
    blah = AppenderBuffer$new()
  ))
  b


  r <- AppenderFileRotating$new(tf)
  r

  r <- AppenderFileRotating$new(tf, size = "1kb")
  r

  r <- AppenderFileRotatingDate$new(tf, layout = LayoutJson$new())
  r

  r <- AppenderFileRotatingTime$new(tf, layout = LayoutJson$new(), age = "2 years", size = 9000)
  r

  fl <- AppenderFile$new(tf)
  fl

})



<AppenderConsole> [all]
  Destination:  console
  LayoutFormat:  %L [%t] %m %f



<AppenderFile> [all]
  Destination:  "/tmp/RtmpWYA891/file5ac1dc5db5d"
  LayoutFormat:  %L [%t] %m %f


<AppenderJson> [all]
  Destination:  "/tmp/RtmpWYA891/file5ac1dc5db5d"
  LayoutJson:  jsonlite::toJSON



<AppenderFileRotating> [all]
  Destination:  "/tmp/RtmpWYA891/file5ac1dc5db5d"
  LayoutJson:  jsonlite::toJSON
  Age: "2 Years"
