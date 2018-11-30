if (requireNamespace("crayon", quietly = TRUE)){
  style_error   <- crayon::make_style("#BB3333", colors = 256)
  style_fatal   <- function(x) style_error(crayon::bold(x))
  style_warning <- crayon::make_style("#EEBB50", colors = 256)
  style_subtle  <- crayon::make_style(grDevices::grey(0.5), grey = TRUE)
  style_accent  <- crayon::silver
  col_nchar     <- crayon::col_nchar

} else {
  style_fatal   <- function(...) paste(...)
  style_error   <- style_fatal
  style_warning <- style_fatal
  style_subtle  <- style_fatal
  style_accent  <- style_fatal
  col_nchar     <- function(...) nchar(...)
}
