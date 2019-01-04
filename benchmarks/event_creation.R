library(bench)
library(lgr)
library(R6)
library(sfmisc)

AdvancedEvent <- R6::R6Class(
  "LogEvent",
  lock_objects = FALSE,
  public = list(
    initialize = function(
      logger,
      level = 400,
      timestamp = Sys.time(),
      caller = NA,
      msg = NA,
      ...
    ){
      assert(inherits(logger, "Logger"))

      # assign has less overhead than [[ and event creation needs to be as fast
      # as possible
      assign("logger", logger, self)
      assign("level", level, self)
      assign("timestamp", timestamp, self)
      assign("caller", caller, self)
      assign("msg", msg, self)

      # custom values
      if (!missing(...)){
        dots <- list(...)
        assert(identical(length(names(dots)), length(dots)))
        for (nm in names(dots)){
          assign(nm, dots[[nm]], self)
        }
      }
    },
    logger = NULL,
    level = NULL,
    timestamp = NULL,
    caller = NULL,
    msg = NULL
  ),

  active = list(
    json1 = function() {
      jsonlite::toJSON(self$values)
    },

    json2 = function() {
      jsonlite::toJSON(self$values)
    },

    json3 = function() {
      jsonlite::toJSON(self$values)
    },

    json4 = function() {
      jsonlite::toJSON(self$values)
    },

    json5 = function() {
      jsonlite::toJSON(self$values)
    },

    values = function(){
      fixed_vals   <- c("level", "timestamp", "caller", "msg")
      custom_vals <- setdiff(
        names(get(".__enclos_env__", self)[["self"]]),
        c(".__enclos_env__", "level_name", "initialize", "clone", "values",
          "logger", "logger_name", "logger_user", paste0("json", 0:9))
      )
      valnames <- union(fixed_vals, custom_vals) # to enforce order of fixed_vals
      mget(valnames, envir = self)
    },

    level_name = function(){
      label_levels(get("level", envir = self))
    },

    logger_name = function(){
      get("name", envir = get("logger", envir = self))
    },

    logger_user = function(){
      get("user", envir = get("logger", envir = self))
    }
  )
)


event <- LogEvent$new(logger = lgr)
avent <- AdvancedEvent$new(logger = lgr)


bench::mark(
  for (i in 1:100) LogEvent$new(logger = lgr),
  for (i in 1:100) AdvancedEvent$new(logger = lgr),
  for (i in 1:100) {
    assign("level", 400,  envir = event)
    assign("timestamp", Sys.time(), envir = event)
    assign("caller", get_caller(), envir = event)
    assign("msg", NA_character_, envir = event)
  },
  for (i in 1:100) {
    assign("level", 400,  envir = avent)
    assign("timestamp", Sys.time(), envir = avent)
    assign("caller", get_caller(), envir = avent)
    assign("msg", NA_character_, envir = avent)
  },
  min_iterations = 20
)
