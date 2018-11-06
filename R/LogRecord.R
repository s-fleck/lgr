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
    logger = NULL
  ),

  active = list(
    values = function(){
      mget(c("level", "timestamp", "caller", "msg"), envir = self)
    },
    level_name = function(){
      label_levels(self$level, log_levels = self$logger$log_levels)
    }
  )
)
