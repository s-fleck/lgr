#' @include utils.R utils-sfmisc.R
#' @export
CollectorDefault <- R6::R6Class(
  "CollectorDefault",
  public = list(
    initialize = function(
      ...,
      .cache_size = 1e5
    ){
      assert(is_scalar_integerish(.cache_size))
      args <- list(...)
      assert(
        all(vapply(args, is_scalar, logical(1))),
        "All arguments must scalars"
      )

      res <- function(){
        private$current_row <- private$current_row + 1L
        private$id <- private$id  + 1L
        .args <- formals()

        vals <- lapply(
          names(.args),
          function(nm) {
            .x <- sys.frame(-2)[[nm]]
            if (is.function(.x)) .x() else .x
          }
        )

        data.table::set(
          private$.data,
          private$current_row,
          j = c(names(.args), "id"),
          value = c(vals, list(private$id))
        )

        private$.last_value <- setNames(vals, names(.args))

        if (identical(private$current_row, nrow(private$.data))){
          private$current_row <- 0L
        }

        invisible(msg)
      }

      formals(res) <- args
      self$log <- res

      # initialize empty dt
        vals <- lapply(args, function(.x) {if (is.function(.x)) .x() else .x})
        private$.data <- structure(
          data.table::as.data.table(vals),
          class = c("memlog_data", "data.table", "data.frame")
        )
        for (j in seq_along(private$.data)){
          data.table::set(private$.data, i = 1L, j = j, value = NA)
        }
        dd <- list(
          private$.data,
          list(rep(private$.data[[1]], .cache_size - 1L))
        )
        names(dd[[2]]) <- names(private$.data)[[1]]
        private$.data <- data.table::rbindlist(
          dd,
          fill = TRUE
        )
        data.table::setattr(
          private$.data,
          "class",
          c("memlog_data", "data.table", "data.frame")
        )

      invisible()
    },
    log = NULL
  ),

  active = list(
    last_value = function(value) {
      if (missing(value)) return(private$.last_value)
      private$.last_value <- value
    },

    data = function(){
      private$.data
    }
  ),

  private = list(
    id = 0L,
    current_row = 0L,
    .last_value = NULL,
    .data = NULL
  )
)
