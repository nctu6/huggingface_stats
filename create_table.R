library(ggplot2)
library(dplyr)

Sys.setlocale("LC_TIME", "C")

# dir.create("plots")
data_files <- list.files("data/models", full.names = TRUE)

df <- data_files %>% purrr::map_df(~readr::read_csv(.))

df_model <- df %>%
  group_by(model_name, date) %>%
  summarise(downloads = sum(downloads), .groups = "drop") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  filter(!(model_name %in% c("ModelCardReview", "cp.", "GGUF")))

df_all <- df %>%
  group_by(model_name) %>%
  summarise(downloadsAllTime = max(downloadsAllTime), .groups = "drop") %>%
  filter(!(model_name %in% c("ModelCardReview", "cp.", "GGUF")))

df_model <- df_model %>%
  mutate(month = stringr::str_pad(month, side = "left", width = 2, pad = "0"),
         yearmonth = paste(year, month, sep = "-"))

df_table <- df_model %>%
  select(model_name, yearmonth, downloads) %>%
  tidyr::pivot_wider(names_from = yearmonth, values_from = downloads)

# Reorder columns: sort yearmonth from newest to oldest
date_cols <- names(df_table)[!(names(df_table) %in% c("model_name"))]
date_cols <- setdiff(date_cols, "downloadsAllTime")
date_cols_sorted <- sort(date_cols, decreasing = TRUE)

df_table <- df_table %>%
  left_join(df_all, by = "model_name") %>%
  select(model_name, downloadsAllTime, all_of(date_cols_sorted))

# Print table
table <- knitr::kable(df_table, format = "pipe")
print(table)
