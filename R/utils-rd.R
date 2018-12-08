r6_usage <- function(
  x,
  name = "x"
){
  ind <- "    "

  public_methods <- vapply(
    names(x$public_methods),
    function(nm) fmt_function(nm, formals(x$public_methods[[nm]])),
    character(1)
  )
  ctor <- public_methods[["initialize"]]
  ctor <- gsub("^initialize", paste0(deparse(substitute(x)), "$new"), ctor)
  public_methods <-
    public_methods[!names(public_methods) %in% c("initialize", "finalize")]


  c(
    "@section Usage:",
    "```",
    strwrap(ctor, width = 80, exdent = 4),
    "# public methods",
    paste0(ind, name, "$",  public_methods),
    "# public fields",
    paste0(ind, name, "$", names(x$public_fields)),
    "# active bindings",
    paste0(ind, name, "$", names(x$active)),
    "```"
  )
}



fmt_function <- function(name, arglist){
  paste0(name, "(", args_string(usage_args(arglist)), ")")
}



quote_if_needed <- function(x) {
  needs_quotes <- !has.quotes(x) & !is.syntactic(x)
  x[needs_quotes] <- paste0('"', str_replace_all(x[needs_quotes], '(["\\\\])', "\\\\\\1"), '"')
  x
}
is.syntactic <- function(x) make.names(x) == x



usage_args <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- enc2utf8(deparse(arg, backtick = TRUE, width.cutoff = 500L))
    text <- paste0(text, collapse = "\n")
    Encoding(text) <- "UTF-8"

    text
  }
  vapply(args, arg_to_text, character(1))
}

args_string <- function(x) {
  missing_arg <- x == ""
  sep <- ifelse(!missing_arg, "\u{A0}=\u{A0}", "")

  arg_names <- names(x)
  needs_backtick <- !is.syntactic(arg_names)
  arg_names[needs_backtick] <- paste0("`", arg_names[needs_backtick], "`")

  paste0(arg_names, sep, x, collapse = ", ")
}
