
cfg_recommended <- as_logger_config(system.file("configs", "recommended.yaml", package = "lgr"))

usethis::use_data(cfg_recommended, internal = TRUE)
