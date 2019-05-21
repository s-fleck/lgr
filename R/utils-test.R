expect_equal_timestamp = function(x, y){
  assert(is_POSIXct(x))
  assert(is_POSIXct(y))
  attr(x, "tzone") <- NULL
  attr(y, "tzone") <- NULL
  expect_true(all(
    format(x, usetz = FALSE) == format(y, usetz = FALSE)
  ))
}



expect_setequal_timestamp = function(x, y){
  assert(is_POSIXct(x))
  assert(is_POSIXct(y))
  attr(x, "tzone") <- NULL
  attr(y, "tzone") <- NULL
  expect_setequal(
    format(x, usetz = FALSE), format(y, usetz = FALSE)
  )
}
