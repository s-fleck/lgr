#' Return a data.frame of all registered loggers
#'
#' @return a `logger_index` `data.frame`
#' @export
#' @seealso [logger_tree()] for a more visual representation of registered
#'   loggers
#'
#' @examples
#' get_logger("tree/leaf")
#' get_logger("shrub/leaf")
#' get_logger("plant/shrub/leaf")
#' logger_index()
logger_index <- function(){

    initialize_implicit_loggers()
    names <- sort(ls(envir = loggers))
    names <- union("root", names)  # ensure root logger is sorted first

    res <- lapply(names, function(logger_name){
        cur_logger <- get_logger(logger_name)

        data.frame(
            name = logger_name,
            configured = !is_virgin_Logger(logger_name),
            threshold = cur_logger$threshold,
            threshold_inherited = is_threshold_inherited(cur_logger),
            propagate = cur_logger$propagate,
            n_appenders = length(cur_logger$appenders)
        )
    })

    res <- do.call(rbind, res)
    structure(
        res,
        class = union("logger_index", class(res))
    )
}



#' Initialize all loggers along all logger paths
#'
#' This is usually not necessary because all loggers along the path are
#' initialized the first time they are needed (even when printing a
#' logger!); however, it is sometimes necessary to do this explicitely
#' when calling `logger_index()`.
#' @noRd
initialize_implicit_loggers <- function(){
    names <- sort(ls(envir = loggers))
    for (n in names){
        x <- unlist(strsplit(n, "/", fixed = TRUE))
        for (i in seq_along(x)){
            get_logger(paste(x[1:i], collapse = "/"))
        }
    }
}
