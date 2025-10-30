library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(knitr)
library(purrr)

Sys.setlocale("LC_TIME", "C")
options(readr.show_col_types = FALSE)

data_files <- list.files("data/models", pattern = "\\.csv$", full.names = TRUE)

colspec <- cols(
  model_url         = col_character(),
  organization      = col_character(),
  model_name        = col_character(),
  downloads         = col_double(),
  downloadsAllTime  = col_double(),
  date              = col_character()
)

raw <- map_df(data_files, ~ read_csv(.x, col_types = colspec))

# 依「包含字串」排除
raw2 <- raw %>%
  filter(!str_detect(model_name, regex("ModelCardReview", ignore_case = TRUE))) %>%
  filter(!str_detect(model_name, regex("^cp\\.", ignore_case = TRUE))) %>%
  filter(!str_detect(model_name, regex("GGUF", ignore_case = TRUE)))

# ✅ 正確的日期解析：兩條路都轉成 POSIXct，再 coalesce
dt_hms <- ymd_hms(raw2$date, tz = "UTC", quiet = TRUE)              # 先試 datetime
dt_day <- as_datetime(ymd(raw2$date, quiet = TRUE), tz = "UTC")     # 再試 date → POSIXct
raw2 <- raw2 %>%
  mutate(
    date_time = coalesce(dt_hms, dt_day),
    yearmonth = format(floor_date(date_time, "month"), "%Y-%m")
  )

# 彙總到月份
df_monthly <- raw2 %>%
  group_by(model_name, yearmonth) %>%
  summarise(downloads = sum(downloads, na.rm = TRUE), .groups = "drop")

# 展寬並補 0
monthly_wide <- df_monthly %>%
  pivot_wider(names_from = yearmonth, values_from = downloads, values_fill = 0)

# 月份欄位排序：新→舊
date_cols <- setdiff(names(monthly_wide), "model_name")
date_cols_sorted <- sort(date_cols, decreasing = TRUE)

# 計總和排序
monthly_wide <- monthly_wide %>%
  select(model_name, all_of(date_cols_sorted)) %>%
  mutate(downloadsAllTime = rowSums(across(all_of(date_cols_sorted)))) %>%
  relocate(downloadsAllTime, .after = model_name) %>%
  arrange(desc(downloadsAllTime))

print(knitr::kable(monthly_wide, format = "pipe", digits = 0))