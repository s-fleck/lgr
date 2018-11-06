#' @include filterable.R

#' Title
#'
#' @param appenders
#' @param user
#' @param pid
#' @param log_levels
#'
#' @return
#' @export
#'
#' @examples
Logger <- R6::R6Class(
  "Logger",
  inherit = Filterable,

  # public methods --------------------------------------------------------
  public = list(
    initialize = function(
      name,
      appenders = list(),
      threshold = NA,
      user = get_user(),
      log_levels = getOption("yog.log_levels"),
      parent = yog::yog,
      string_formatter = sprintf,
      handle_exception = function(e){
        warning(
          "[", format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), "] ",
          "An error occured in the logging sub system: ", e
        )
      }
    ){
      # fields ------------------------------------------------------------
      # active
      self$log_levels <- log_levels
      self$threshold <- threshold
      self$appenders <- appenders
      self$user  <- user
      self$string_formatter <- string_formatter
      self$handle_exception <- handle_exception
      self$parent <- parent
      self$name <- name
      self$last_event <- LogRecord$new(self)

      # init log functions ----------------------------------------------
      make_logger <- function(
        level,
        ...
      ){
        force(level)
        function(msg, ...){
          self$log(
            msg = private$.string_formatter(msg, ...),
            level = level
          )
        }
      }

      for (i in seq_along(private$.log_levels)){
        nm  <- names(private$.log_levels)[[i]]
        lvl <- private$.log_levels[[i]]

        if (nm %in% names(self)){
          stop(
            "The following names are not allowed for log levels: ",
            paste(sort(names(self)), collapse= ", ")
          )
        }

        self[[nm]] <- make_logger(lvl)
      }

      invisible(self)
    },


    log = function(
      level,
      timestamp = Sys.time(),
      caller = get_caller(),
      msg
    ){
      tryCatch({
        assign("level", level, envir = self$last_event)
        assign("timestamp", timestamp,  envir = self$last_event)
        assign("caller", caller, envir = self$last_event)
        assign("msg", msg, envir = self$last_event)
        #assign(".Logger", self, envir = self$last_event)


        if (self$filter(self$last_event)){
          for (app in c(self$appenders, self$ancestral_appenders)) {
            if (app$filter(self$last_event)){
              app$append(self$last_event)
            }
          }
        }
      },
      error = self$handle_exception
      )
    },


    handle_exception = NULL,


    add_appender = function(appender, name = NULL){
      assert(inherits(appender, "Appender"))
      appender <- appender$clone()
      appender$logger <- self
      private$.appenders[length(private$.appenders) + 1L] <- list(appender)

      if (!is.null(name))
        names(private$.appenders)[length(private$.appenders)] <- name

      invisible(self)
    },

    remove_appender = function(pos){
      if (is.numeric(pos)){
        assert(
          all(pos %in% seq_along(private$.appenders)),
          "'pos' is out of range of the length of appenders (1:",
          length(appenders), ")"
        )

        pos <- as.integer(pos)
      } else if (is.character(pos)) {
        assert(
          all(pos %in% names(private$.appenders)),
          "'pos' is not names of appenders (",
          paste(names(private$.appenders), collapse = ", "),
          ")"
        )
      }

      private$.appenders[pos] <- NULL
      invisible(self)
    },

    suspend = function(){
      if (length(private$suspended_loggers) > 0){
        warning("Logger is already suspended")
      } else {
        for (i in seq_along(self$log_levels)){
          nm  <- names(self$log_levels)[[i]]
          lvl <- self$log_levels[[i]]
          private$suspended_loggers[[nm]] <- self[[nm]]
          self[[nm]] <- function(...) NULL
        }
      }
    },


    unsuspend = function(){
      if (length(private$suspended_loggers) < 1){
        warning("Logger is not suspended")
      }

      for (i in seq_along(private$.log_levels)){
        nm  <- names(private$.log_levels)[[i]]
        lvl <- private$.log_levels[[i]]
        self[[nm]] <- private$suspended_loggers[[nm]]
      }
      private$suspended_loggers <- list()
    },


    show = function(
      n = 20,
      threshold = NA
    ){
      sel <- vapply(self$appenders, inherits, TRUE, "AppenderMemoryDt")

      if (!any(sel)){
        message("This logger has no memory appender (see AppenderMempryDt)")
        return(invisible)

      } else {
        self$appenders[sel][[1]]$show(n = n, threshold = threshold)
      }
    },


    format = function(
      ...,
      colors = TRUE
    ){

      header <- paste(
        paste0("<", class(self)[[1]], ">"),
        style_subtle(format(self$ancestry))
      )

      ind <- "  "

      appenders <- do.call(rbind, lapply(self$appenders, srs))

      if (length(appenders) > 0){
        appenders$name <- pad_right(appenders$name)

        appenders <- paste0(
          pad_right(appenders$name), ": ",
          pad_right(
            paste0(label_levels(appenders$threshold), style_subtle(paste0(" (", appenders$threshold, ")")))
          ),
          vapply(appenders$comment, ptrunc, character(1), width = 128)
        )
      } else {
        appenders <- style_subtle("none")
      }

      anc_appenders <- do.call(
        rbind,
        lapply(self$ancestral_appenders, function(.x){
          cbind(data.frame(logger = .x$logger$name, srs(.x)))
        }
        ))



      if (length(anc_appenders) > 0){
        anc_appenders$logger <- pad_right(anc_appenders$logger)
        anc_appenders$name   <- pad_right(anc_appenders$name)

        anc_appenders <- paste0(
          style_subtle(paste0(anc_appenders$logger, " -> ")),
          anc_appenders$name, ": ",
          pad_right(
            paste0(label_levels(anc_appenders$threshold), style_subtle(paste0(" (", anc_appenders$threshold, ")")))
          ),
          vapply(anc_appenders$comment, ptrunc, character(1), width = 128)
        )
      } else {
        anc_appenders <- NULL
      }




      obsums <- object_summaries(self)

      act <- obsums[obsums == "active binding"]
      act <- paste0(names(act), ": ", act)

      funs <- obsums[grep("function", obsums)]
      funs <- paste0(names(funs), ": ", funs)

      logger_pat <-
        paste0("(^", c(names(self$log_levels), "log"), ":)", collapse = "|")

      sel <- grepl(logger_pat, funs)
      methods <- funs[!sel]
      loggers <- c(
        paste0(names(self$log_levels), ": function (msg, ...) "),
        grep("^log:", funs, value = TRUE)
      )

      c(
        header,
        paste0(ind, "Fields / Active Bindings:"),
        paste0(ind, ind, "threshold: ",  fmt_threshold(self$threshold)),
        paste0(ind, ind, "log_levels: ", sls(self$log_levels)),
        paste0(ind, ind, "string_formatter: ", sls(self$string_formatter)),
        paste0(ind, ind, "user: ", self$user),
        paste0(ind, ind, "appenders:"),
        paste0(ind, ind, ind, appenders),
        paste0(ind, ind, ind, anc_appenders)[!is.null(anc_appenders)],
        paste0(ind, "Methods:"),
        paste0(ind, ind, "Loggers:"),
        paste0(ind, ind, ind, loggers),
        paste0(ind, ind, methods)
      )
    },


    # public fields -----------------------------------------------------------
    last_event = NULL

  ),




  # active bindings ---------------------------------------------------------
  active = list(
    name = function(value){
      if (missing(value)) return(private$.name)
      assert(is_scalar_character(value))
      private$.name <- value
    },

    ancestry = function(){
      structure(
        c(self$name, private$.parent$ancestry),
        class = c("ancestry", "character")
      )
    },

    parent = function(value){
      if (missing(value)) return(private$.parent)
      assert(is.null(value) || inherits(value, "Logger"))
      private$.parent <- value
    },

    log_levels = function(value){
      if (missing(value)) return(private$.log_levels)

      assert(
        is.null(private$.log_levels),
        stop("'log_levels' cannot be modified once they have been initialized")
      )

      stopifnot(
        all_are_distinct(unname(value)),
        all_are_distinct(names(value)),
        !any(value == 0),
        is_integerish(value),
        identical(length(names(value)) , length(value))
      )

      value <- setNames(as.integer(value), names(value))
      class(value) <- c("log_levels", class(value))

      private$.log_levels <- value
    },

    threshold = function(value){
      if (missing(value))
        return(private$.threshold)

      if (is.na(value)){
        value <- NA_integer_

      } else if (is_scalar_character(value)) {
        assert_valid_log_levels(value)
        value <- unlabel_levels(value, log_levels = self$log_levels)
      }

      assert_valid_log_levels(value)
      private$.threshold <- as.integer(value)
    },

    ancestral_appenders = function(){
      c(
        private$.parent$appenders,
        private$.parent$ancestral_appenders
      )
    },

    appenders = function(value){
      if (missing(value)) return(c(private$.appenders))

      if (is.null(value)){
        private$.appenders <- NULL
        return(invisible())
      }

      if (inherits(value, "Appender"))
        value <- list(value)

      value <- lapply(value, function(app){
        res <- app$clone()
        res$logger <- self
        res
      })

      assert(
        is.list(value) && all(vapply(value, inherits, TRUE, "Appender")),
        "'appenders' must either be a single Appender, a list thereof, or NULL for no appenders."
      )

      private$.appenders <- value
      invisible()
    },

    user = function(value){
      if (missing(value)) return(private$.user)
      assert(
        is_scalar_character(value),
        "'user' must be a scalar character"
      )
      private$.user <- value
    },

    string_formatter = function(value){
      if (missing(value)) return(private$.string_formatter)
      assert(is.function(value))
      private$.string_formatter <- value
    }
  ),


  # private -----------------------------------------------------------------
  private = list(
    .filters = list(check_threshold),
    .name = NULL,
    .parrent = NULL,
    .appenders = NULL,
    .user = NA_character_,
    .log_levels = NULL,
    .threshold = 4L,
    .string_formatter = NULL,
    suspended_loggers = list()
  ),

  lock_objects = FALSE
)
