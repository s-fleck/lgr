#' @include utils.R utils-sfmisc.R

# AppenderRotating ----------------------------------------------------
AppenderRotating <- R6::R6Class(
  "AppenderRotating",
  inherit = AppenderFile,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      compress = FALSE,
      compress_args = list(flags = paste0(formals(utils::zip)$flags, "q"))
    ){
      self$file <- file
      self$threshold <- threshold
      self$layout <- layout
      self$compress <- compress
      self$compress_args <- compress_args
    },

    append = function(x){
      if (self$should_rotate()){
        self$do_rollover()
      }
      cat(
        private$.layout$format_event(x),
        sep = "\n", file = private$.file, append = TRUE
      )
      return(invisible())
    },

    do_rollover = function(){
      assert(file.exists(self$file))
      backups <- private$list_backups()
      idx_pad <- 1L

      if (length(backups) > 0){
        idx <- names(backups)
        ext <- ifelse(tools::file_ext(backups) == "zip", ".zip", "")
        sorder <- order(sort(as.integer(idx)), decreasing = TRUE)
        idx <- idx[sorder]
        ext <- ext[sorder]

        assert(!anyNA(idx))
        idx_pad <- max(nchar(as.character(as.integer(idx) + 1L)))

        # walk instead of vectorized file.rename to prevent accidental overwrites
        walk(
          seq_along(idx),
          function(i){
            file.rename(
              paste0(self$file, ".", idx[[i]], ext[[i]]),
              paste0(self$file, ".", as.integer(idx[[i]]) + 1L, ext[[i]])
            )
          }
        )
      }

      if (self$compress) {
        utils::capture.output(do.call(
          utils::zip,
          c(list(zipfile = paste0(self$file, ".1.zip"), files  = self$file), self$compress_args)
        ))
        file.remove(self$file)

      } else {
        file.rename(self$file, paste0(self$file, ".1"))
      }

      backups <- private$list_backups()
      file.rename(backups, autopad_backup_index(backups))
      invisible(self)
    },


    prune_backups = function(max_backups){
      backups <- private$list_backups()
      if (length(backups) >= max_backups){
        backups <- backups[order(as.integer(names(backups)))]
        to_remove <- backups[-c(seq_len(max_backups))]
        file.remove(to_remove)
        backups <- private$list_backups()
        file.rename(backups, autopad_backup_index(backups))
      }
      invisible(self)
    }
  ),


  active = list(
    file = function(value){
      if (missing(value)) return(private$.file)
      private$.file <- value
    },


    compress = function(value){
      if (missing(value)) return(private$.compress)
      assert(is_scalar_bool(value))
      private$.compress <- value
    },


    compress_args = function(value){
      if (missing(value)) return(private$.compress_args)
      assert(
        identical(length(names(value)), length(value)) &&
          all(names(value) %in% names(formals(utils::zip))),
        "'compress_args' must be a named list, all names be valid arguments",
        "for tools::zip()."
      )
      private$.compress_args <- value
    }
  ),


  private = list(
    .file = NULL,
    .compress = FALSE,
    .compress_args = NULL,
    list_backups = function(){
      backups <- list.files(
        dirname(self$file),
        pattern = paste0(basename(self$file), "\\.\\d*(\\.zip){0,1}$"),
        full.names = TRUE
      )
      idx <- get_backup_index(backups)
      setNames(backups, idx)
    }
  )
)





# AppenderRotatingDate ----------------------------------------------------
AppenderRotatingDate <- R6::R6Class(
  "AppenderRotatingDate",
  inherit = AppenderRotating,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      period = "1 month",
      timestamp_fmt = "%Y-M%m",
      layout = LayoutFormat$new(),
      compress = FALSE,
      compress_args = list(flags = paste0(formals(utils::zip)$flags, "q"))
    ){
      self$file <- file
      self$threshold <- threshold
      self$timestamp_fmt <- timestamp_fmt
      self$layout <- layout
      self$compress <- compress
      self$compress_args <- compress_args
      self$period <- period


      # get last rollover
      backups <- list.files(
        dirname(self$file),
        pattern = paste0(basename(self$file), "\\..*(\\.zip){0,1}$"),
        full.names = TRUE
      )

      if (length(backups) > 0){
        backups <- gsub("\\.zip$", "", backups)
        backups <- strsplit(backups, ".", fixed = TRUE)
        backups <- vapply(backups, function(.x) .x[[length(.x)]], character(1))

        if (grepl("\\d{4}-M\\d{2}$", backups)){
          y <- as.integer(substr(backups, 1, 4))
          m <- as.integer(substr(backups, 7, 8))
          d <- ISOdate(y, m , 1)
        }

        self$last_rollover <- d

      } else {

        if (file.exists(self$file)){
          self$last_rollover <- file.info(self$file)$ctime

        } else {
          self$last_rollover <- Sys.time()
        }
      }
    },


    do_rollover = function(){
      new_ts <- format(Sys.time(), format = self$timestamp_fmt)
      assert(file.exists(self$file))
      backups <- private$list_backups()
      outname <- paste0(self$file, ".", new_ts)

      if (any(grepl(paste0(basename(outname), "(\\.zip){0,1}"), backups))){
        stop(sprintf("Log rollover aborted: A backup for '%s' already exists. ", outname))
      }

      if (self$compress) {
        outname <- paste0(outname, ".zip")
        do.call(
          utils::zip,
          c(list(zipfile = outname, files  = self$file), self$compress_args)
        )
        file.remove(self$file)

      } else {
        file.rename(self$file, outname)
      }

      self$last_rollover <- Sys.time()
      invisible(self)
    },


    prune_backups = function(max_backups){
      backups <- private$list_backups()
      if (length(backups) >= max_backups){
        backups <- backups[order(as.integer(names(backups)))]
        to_remove <- backups[-c(seq_len(max_backups))]
        file.remove(to_remove)
        backups <- private$list_backups()
        file.rename(backups, autopad_backup_index(backups))
      }
      invisible(self)
    },


    last_rollover = NULL,


    should_rotate = function(){
      as.POSIXlt(Sys.Date())$mon >= as.POSIXlt(self$last_rollover)$mon + 1L
    }
  ),


  active = list(
    period = function(value){
      if (missing(value)) return(private$.period)
      private$.period <- value
    },

    timestamp_fmt = function(value){
      if (missing(value)) return(private$.timestamp_fmt)
      assert(is_scalar_character(value))
      private$.timestamp_fmt <- value
    }
  ),


  private = list(
    .period = NULL,
    .timestamp_fmt = NULL,
    .last_rollover = NULL,
    list_backups = function(){
      backups <- list.files(
        dirname(self$file),
        pattern = paste0(basename(self$file), "\\..*(\\.zip){0,1}$"),
        full.names = TRUE
      )
      idx <- get_backup_timestamp(backups)
      setNames(backups, idx)
    }
  )
)







# appender crash ----------------------------------------------------------


# appender email ----------------------------------------------------------


