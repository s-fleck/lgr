#' @export
LogRecord <- R6::R6Class(
  public = list(
    initialize = function(logger){
      assert(inherits(logger, "Logger"))
      self$logger <- logger
    },
    level = NULL,
    timestamp = NULL,
    caller = NULL,
    msg = NULL,
    logger = NULL,
    value_names = c("level", "timestamp", "caller", "msg")
  ),

  active = list(
    values = function(){
      mget(c("level", "timestamp", "caller", "msg"), envir = self)
    }
  ),

  class = FALSE
)
