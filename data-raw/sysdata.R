DEFAULT_CONFIGS <- list(
  minimal = as_logger_config(system.file("configs", "minimal.yaml", package = "lgr")),
  interactive = as_logger_config(system.file("configs", "recommended.yaml", package = "lgr"))
)


usethis::use_data(DEFAULT_CONFIGS, internal = TRUE, overwrite = TRUE)
