library(tidyverse)
library(readxl)
library(scales)
library(patchwork)
library(knitr)

find_project_root <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(cur, "_quarto.yml"))) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) stop("Could not locate project root containing _quarto.yml")
    cur <- parent
  }
}

project_root <- find_project_root()
project_path <- function(...) file.path(project_root, ...)

th1_zip_url <- "https://data.mendeley.com/public-api/zip/sw4jmdb2sm/download/1"
th1_data_dir <- project_path("data", "th_ex1", "raw")
th1_csv_path <- file.path(th1_data_dir, "Dataset of motor insurance portfolio.csv")
th1_dict_path <- file.path(th1_data_dir, "Descriptive of variables.xlsx")

ensure_th1_data <- function() {
  if (file.exists(th1_csv_path) && file.exists(th1_dict_path)) {
    return(invisible(TRUE))
  }
  dir.create(th1_data_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- project_path("data", "th_ex1", "sw4jmdb2sm-1.zip")
  if (!file.exists(zip_path)) {
    dir.create(dirname(zip_path), recursive = TRUE, showWarnings = FALSE)
    download.file(th1_zip_url, zip_path, mode = "wb", quiet = TRUE)
  }
  temp_dir <- tempfile("th_ex1_unzip_")
  dir.create(temp_dir)
  utils::unzip(zip_path, exdir = temp_dir)
  nested <- list.files(temp_dir, recursive = TRUE, full.names = TRUE)
  for (p in nested) {
    if (basename(p) %in% c("Dataset of motor insurance portfolio.csv", "Descriptive of variables.xlsx")) {
      file.copy(p, file.path(th1_data_dir, basename(p)), overwrite = TRUE)
    }
  }
  unlink(temp_dir, recursive = TRUE)
  invisible(TRUE)
}

load_th1_motor <- function() {
  ensure_th1_data()
  read_delim(th1_csv_path, delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8")) |>
    mutate(
      year = factor(year),
      claim_flag = total_claims > 0,
      profit = total_premium - total_incurred,
      loss_ratio = if_else(total_premium > 0, total_incurred / total_premium, NA_real_),
      loss_ratio_cap = if_else(
        is.na(loss_ratio),
        NA_real_,
        pmin(loss_ratio, quantile(loss_ratio, 0.99, na.rm = TRUE))
      ),
      claim_frequency = if_else(total_exposure > 0, total_claims / total_exposure, NA_real_),
      severity = if_else(total_claims > 0, total_incurred / total_claims, NA_real_),
      driver_age_band = cut(driver_age, breaks = c(17, 25, 35, 45, 55, 65, Inf),
                            labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66+"), right = TRUE),
      vehicle_age_band = cut(vehicle_age, breaks = c(-Inf, 3, 7, 12, 20, Inf),
                             labels = c("0-3", "4-7", "8-12", "13-20", "21+"), right = TRUE)
    )
}

portfolio_summary <- function(df, group_var) {
  df |>
    group_by({{ group_var }}) |>
    summarise(
      records = n(),
      exposure = sum(total_exposure, na.rm = TRUE),
      premium = sum(total_premium, na.rm = TRUE),
      incurred = sum(total_incurred, na.rm = TRUE),
      claims = sum(total_claims, na.rm = TRUE),
      profit = sum(profit, na.rm = TRUE),
      high_loss_rate = mean(loss_ratio > 1, na.rm = TRUE),
      p95_loss_ratio = quantile(loss_ratio, 0.95, na.rm = TRUE),
      loss_ratio = incurred / premium,
      claim_frequency = claims / exposure,
      avg_premium = premium / exposure,
      avg_severity = if_else(claims > 0, incurred / claims, NA_real_),
      .groups = "drop"
    )
}

money <- label_dollar(prefix = "€", accuracy = 1, big.mark = ",")
pct <- label_percent(accuracy = 0.1)
