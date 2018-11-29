
if (requireNamespace("colt", quietly = TRUE)){

  style_fatal   <- function(x) colt::clt_emph2(colt::clt_error(x))
  style_error   <- colt::clt_error
  style_warning <- colt::clt_warning
  style_subtle  <- colt::clt_chr_subtle
  style_accent <- colt::clt_chr_accent

} else if (requireNamespace("crayon", quietly = TRUE)){

  style_error   <- crayon::make_style("#BB3333", colors = 256)
  style_fatal   <- function(x) style_error(crayon::bold(x))
  style_warning <- crayon::make_style("#EEBB50", colors = 256)
  style_subtle  <- crayon::make_style(grDevices::grey(0.5), grey = TRUE)
  style_accent  <- crayon::blue

} else {
  style_error   <- function(...) paste(...)
  style_fatal   <- function(...) paste(...)
  style_error   <- function(...) paste(...)
  style_warning <- function(...) paste(...)
  style_subtle  <- function(...) paste(...)
  style_accent  <- function(...) paste(...)

}
