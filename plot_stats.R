library(ggplot2)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)

Sys.setlocale("LC_TIME", "C")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
data_files <- list.files("data/models", full.names = TRUE)

df <- data_files %>%
  map_df(~ read_csv(., show_col_types = FALSE)) %>%
  mutate(
    date = floor_date(as.Date(date), "month")   # <-- FIXED HERE
  )

# ---------------------------------------------------------
# Total downloads per month
# ---------------------------------------------------------
df_total_sum <- df %>%
  group_by(date) %>%
  summarize(downloads = sum(downloads), .groups = "drop") %>%
  arrange(date)

# ---------------------------------------------------------
# Plot: Total downloads per month
# ---------------------------------------------------------
p_dl_total <- ggplot(
  data = df_total_sum,
  aes(x = date, y = downloads)
) +
  geom_line(colour = "firebrick2", na.rm = TRUE) +
  geom_point(shape = 21, size = 1.5, colour = "black",
             fill = "firebrick2", na.rm = TRUE) +
  theme_light(base_size = 7) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8),
    labels = function(x) format(x, big.mark = " ", decimal.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_labels = "%Y-%b",
    date_breaks = "1 month",
    guide = guide_axis(check.overlap = TRUE)
  ) +
  expand_limits(y = 0) +
  labs(
    y = "Number of downloads",
    x = "Date",
    title = "Total number of downloads per month for TAIDE's models on Huggingface"
  )

# ---------------------------------------------------------
# Top 10 models
# ---------------------------------------------------------
df_model <- df %>%
  group_by(model_name) %>%
  summarize(downloadsAllTime = sum(downloads), .groups = "drop") %>%
  arrange(desc(downloadsAllTime)) %>%
  slice(1:10)

model_order <- df_model$model_name

df_model_top <- df %>%
  filter(model_name %in% df_model$model_name) %>%
  group_by(date, model_name) %>%
  summarize(downloads = sum(downloads), .groups = "drop") %>%
  mutate(model_name = factor(model_name, levels = model_order)) %>%
  arrange(date)

# ---------------------------------------------------------
# Plot: Downloads per model (Top 10)
# ---------------------------------------------------------
p_dl_model <- ggplot(
  data = df_model_top,
  aes(
    x = date,
    y = downloads,
    color = model_name,
    fill  = model_name
  )
) +
  geom_line(na.rm = TRUE) +
  geom_point(shape = 21, size = 1.5, colour = "black", na.rm = TRUE) +
  theme_light(base_size = 7) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8),
    labels = function(x) format(x, big.mark = " ", decimal.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_labels = "%Y-%b",
    date_breaks = "1 month",
    guide = guide_axis(check.overlap = TRUE)
  ) +
  expand_limits(y = 0) +
  labs(
    y = "Number of downloads",
    x = "Date",
    title = "Number of downloads by model name for top 10 models",
    fill = "Model"
  ) +
  guides(color = "none")

# ---------------------------------------------------------
# Save images
# ---------------------------------------------------------
ggsave(
  p_dl_total,
  filename = "plots/downloads_total.jpg",
  device = "jpeg",
  type = "cairo",
  dpi = 300,
  width = 1920,
  height = 1080,
  units = "px"
)

ggsave(
  p_dl_model,
  filename = "plots/downloads_by_model.jpg",
  device = "jpeg",
  type = "cairo",
  dpi = 300,
  width = 1920,
  height = 1080,
  units = "px"
)
