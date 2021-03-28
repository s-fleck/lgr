#' Logger Tree
#'
#' Displays a tree structure of all registered Loggers.
#'
#' @section Symbology:
#'
#' * unconfigured Loggers are displayed in gray (if your terminal supports
#'   colors and you have the package \pkg{crayon} installed).
#' * If a logger's `threshold` is set, it is displayed in square brackets next
#'   to its name (reminder: if the threshold is not set, it is inherited from
#'   next logger up the logger tree).
#' * If a logger's `propagate` field is set to `FALSE` an red hash (`#`) sign
#'   is displayed in front of the logger name, to imply that it does not pass
#'   LogEvents up the tree.
#'
#' @return `data.frame` with subclass `"logger_tree"`
#'
#' @seealso [logger_index()] for a tidy `data.frame` representation of
#'   all registered loggers
#'
#' @export
#'
#' @examples
#' get_logger("fancymodel")
#' get_logger("fancymodel/shiny")$
#'   set_propagate(FALSE)
#'
#' get_logger("fancymodel/shiny/ui")$
#'   set_appenders(AppenderConsole$new())
#'
#' get_logger("fancymodel/shiny/server")$
#'   set_appenders(list(AppenderConsole$new(), AppenderConsole$new()))$
#'   set_threshold("trace")
#'
#' get_logger("fancymodel/plumber")
#'
#' if (requireNamespace("cli")){
#'   print(logger_tree())
#' }
#'
logger_tree <- function(
){
  names <- ls(envir = loggers)
  nodes <- lapply(names, function(.x) unlist(strsplit(.x, "/")))
  max_depth <- seq_len(max(vapply(nodes, length, integer(1))))
  second_tier <- setdiff(vapply(nodes, `[[`, character(1), 1L), "root")

  res <- data.frame(
    parent = "root",
    children = I(list(unique(second_tier))),
    configured = TRUE,
    threshold = get_logger()$threshold,
    threshold_inherited = FALSE,
    propagate = TRUE,
    n_appenders = length(get_logger()$appenders),
    stringsAsFactors = FALSE
  )


  for (i in seq_along(nodes)){
    for (j in seq_along(nodes[[i]])){
      parent_cur <- nodes[[i]][[j]]

      if (j < length(nodes[[i]])){
        child_cur  <- nodes[[i]][[j + 1]]
      } else {
        child_cur <- NULL
      }

      if (parent_cur %in% res$parent){
        sel <- res$parent == parent_cur
        res$children[sel] <- I(list(unique(unlist(compact(c(res$children[sel], child_cur))))))

      } else {
        logger_name <- paste(nodes[[i]][seq_len(j)], collapse = "/")
        cur_logger <- get_logger(logger_name)

        res <- rbind(
          res,
          data.frame(
            parent = parent_cur,
            children = I(list(child_cur)),
            configured = !is_virgin_Logger(logger_name),
            threshold = get_logger(logger_name)$threshold,
            threshold_inherited = is_threshold_inherited(cur_logger),
            propagate = cur_logger$propagate,
            n_appenders = length(cur_logger$appenders)
          )
        )
      }
    }
  }

  assert(all_are_distinct(res$parent))
  structure(
    res,
    class = union("logger_tree", class(res))
  )
}




#' Print Logger Trees
#'
#' @param x a [logger_tree][logger_tree()]
#' @param color `logical` scalar. If `TRUE` terminal output is colorized via
#'   the package \pkg{crayon}?
#' @param ... passed on to [cli::tree()]
#' @return `x` (invisibly)
#' @export
print.logger_tree <- function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  if (requireNamespace("cli", quietly = TRUE)){
    cat(format(x, color = color, ...), sep = "\n")
  } else {
    warning(
      "Console output of logger trees requires the package 'cli'. You can",
      "install it with `install.packages(\"cli\")`."
    )
    print(as.data.frame(x))
  }
  invisible(x)
}




#' @rdname print.logger_tree
#' @export
format.logger_tree <- function(
  x,
  color = FALSE,
  ...
){
  assert_namespace("cli")

  if (!color){
    style_fatal <- identity
    style_color <- identity
  }

  label <- ifelse(x$configured, x$parent, style_subtle(x$parent))
  label <- ifelse(
    x$threshold_inherited,
    label,
    paste0(label, " [", label_levels(x$threshold), "]")
  )
  label <- ifelse(
    x$n_appenders == 0,
    label,
    paste0(label, " -> ", x$n_appenders, " appender", ifelse(x$n_appenders > 1, "s", ""))
  )
  label <- ifelse(
    x$propagate,
    label,
    paste0(style_fatal("#"), label)
  )

  x_print <- data.frame(
    parent = x$parent,
    children = x$children,
    label = label
  )

  format(cli::tree(x_print, root = "root", ...))
}
