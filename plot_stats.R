library(ggplot2)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)

Sys.setlocale("LC_TIME", "C")

# Prevent stale/default batch plot artifacts.
if (file.exists("Rplots.pdf")) invisible(file.remove("Rplots.pdf"))
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

save_plot_jpeg <- function(plot_obj, output_path, width_px = 1920, height_px = 1080, dpi = 300) {
  tmp_path <- paste0(output_path, ".tmp")
  if (file.exists(tmp_path)) invisible(file.remove(tmp_path))

  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_jpeg(
      filename = tmp_path,
      width = width_px,
      height = height_px,
      units = "px",
      res = dpi,
      quality = 95,
      background = "white"
    )
  } else if (capabilities("cairo")) {
    grDevices::jpeg(
      filename = tmp_path,
      width = width_px,
      height = height_px,
      units = "px",
      quality = 95,
      type = "cairo",
      res = dpi,
      bg = "white"
    )
  } else {
    stop("No headless JPEG device available. Install package 'ragg' or enable cairo.")
  }

  print(plot_obj)
  grDevices::dev.off()

  tmp_size <- file.info(tmp_path)$size
  if (is.na(tmp_size) || tmp_size <= 0) {
    stop("Failed to write non-empty JPEG file: ", output_path)
  }

  if (file.exists(output_path)) invisible(file.remove(output_path))
  if (!file.rename(tmp_path, output_path)) {
    stop("Failed to move temp file to output path: ", output_path)
  }

  out_info <- file.info(output_path)
  message(
    "Wrote: ",
    normalizePath(output_path, winslash = "/", mustWork = FALSE),
    " (", out_info$size, " bytes)"
  )
}

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
data_files <- list.files("data/models", pattern = "\\.csv$", full.names = TRUE)

colspec <- cols(
  model_url = col_character(),
  organization = col_character(),
  model_name = col_character(),
  downloads = col_double(),
  downloadsAllTime = col_double(),
  date = col_character()
)

df <- data_files %>%
  map_df(~ read_csv(., col_types = colspec, show_col_types = FALSE)) %>%
  filter(!str_detect(model_name, regex("ModelCardReview", ignore_case = TRUE))) %>%
  filter(!str_detect(model_name, regex("^cp\\.", ignore_case = TRUE))) %>%
  filter(!str_detect(model_name, regex("GGUF", ignore_case = TRUE))) %>%
  mutate(
    dt_hms = ymd_hms(date, tz = "UTC", quiet = TRUE),
    dt_day = as_datetime(ymd(date, quiet = TRUE), tz = "UTC"),
    date_time = coalesce(dt_hms, dt_day)
  ) %>%
  mutate(
    date = as.Date(floor_date(date_time, "month"))
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
palette_values <- scales::hue_pal()(length(model_order))
names(palette_values) <- model_order

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
  scale_color_manual(values = palette_values, breaks = model_order, limits = model_order, drop = FALSE) +
  scale_fill_manual(values = palette_values, breaks = model_order, limits = model_order, drop = FALSE) +
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
save_plot_jpeg(p_dl_total, "plots/downloads_total.jpg")
save_plot_jpeg(p_dl_model, "plots/downloads_by_model.jpg")

# Clean up default device output if any implicit plotting occurred.
if (file.exists("Rplots.pdf")) invisible(file.remove("Rplots.pdf"))
message("Top 10 model order: ", paste(model_order, collapse = " | "))
