library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(knitr)

Sys.setlocale("LC_TIME", "C")

# 1) Load all CSVs
data_files <- list.files("data/models", full.names = TRUE)
df <- data_files %>% purrr::map_df(readr::read_csv)

exclude_models <- c("ModelCardReview", "cp.", "GGUF")

# 2) Normalize dates -> year-month (UTC) and build monthly sums
df_monthly <- df %>%
  filter(!(model_name %in% exclude_models)) %>%
  mutate(
    # robust parse: try datetime first, else Date
    date_time = suppressWarnings(lubridate::as_datetime(date, tz = "UTC")),
    date_time = dplyr::if_else(
      is.na(date_time),
      lubridate::as_datetime(as.Date(date), tz = "UTC"),
      date_time
    ),
    yearmonth = format(lubridate::floor_date(date_time, "month"), "%Y-%m")
  ) %>%
  group_by(model_name, yearmonth) %>%
  summarise(downloads = sum(downloads, na.rm = TRUE), .groups = "drop")

# 3) Pivot to wide: one column per year-month
monthly_wide <- df_monthly %>%
  tidyr::pivot_wider(names_from = yearmonth, values_from = downloads)

# 4) Order the month columns (newest -> oldest)
date_cols <- setdiff(names(monthly_wide), "model_name")
date_cols_sorted <- sort(date_cols, decreasing = TRUE)
monthly_wide <- monthly_wide %>% select(model_name, all_of(date_cols_sorted))

# 5) Recompute downloadsAllTime from the visible month columns
monthly_wide <- monthly_wide %>%
  mutate(downloadsAllTime = rowSums(across(all_of(date_cols_sorted)), na.rm = TRUE)) %>%
  # place the total right after model_name
  relocate(downloadsAllTime, .after = model_name)

# 6) Print the table
print(knitr::kable(monthly_wide, format = "pipe"))
