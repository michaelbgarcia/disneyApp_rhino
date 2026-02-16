#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

set.seed(1971)

base_path <- "app/static/parks_data.rds"
output_path <- "shinylive/data/rides.csv"

if (!file.exists(base_path)) {
  stop("Can't find app/static/parks_data.rds. Run from the project root.")
}

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

parks_data <- readr::read_rds(base_path)
now_ts <- Sys.time()

rides <- parks_data |>
  transmute(parks_name, id, name, latitude, longitude) |>
  mutate(
    status = sample(c("OPERATING", "DOWN"), dplyr::n(), replace = TRUE, prob = c(0.88, 0.12)),
    wait_time = sample(seq(5, 95, 5), dplyr::n(), replace = TRUE),
    ll_time = format(now_ts + (sample(seq(20, 140, 5), dplyr::n(), replace = TRUE) * 60), "%H:%M"),
    last_updated = format(now_ts - sample(0:1800, dplyr::n(), replace = TRUE), "%Y-%m-%d %H:%M:%S")
  )

readr::write_csv(rides, output_path)
