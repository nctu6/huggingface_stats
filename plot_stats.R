library(ggplot2)
library(dplyr)
library(forcats)

Sys.setlocale("LC_TIME", "C")

# dir.create("plots")
data_files <- list.files("data/models", full.names = TRUE)

df <- data_files %>%
  purrr::map_df(~readr::read_csv(.))


df_total_sum <- df %>%
  group_by(date) %>%
  summarize(downloads = sum(downloads))

p_dl_total <- ggplot(data = df_total_sum,
              aes(x = date, y = downloads)) +
  geom_line(colour = "firebrick2") +
  geom_point(shape = 21, size = 1.5, colour = "black", fill = "firebrick2") +
  theme_light(base_size = 7) +
  scale_y_continuous(breaks = seq(0, max(df_total_sum$downloads) + 2000, by = 10000),
                     labels = function(x) format(x, big.mark = " ", decimal.mark = ".", scientific = FALSE)) +
  scale_x_date(date_labels = "%Y-%b",
               breaks = unique(df_total_sum$date)[seq(1, length(unique(df_total_sum$date)), by=2)],
               guide = guide_axis(n.dodge = 2)) +
  expand_limits(y = 0) +
  labs(y = "Number of downloads",
       x = "Date",
       title = "Total number of downloads per month for TAIDE's models on Huggingface")


df_model <- df %>%
  group_by(model_name) %>%
  summarize(downloads = sum(downloads)) %>%
  arrange(-downloads) %>%
  ungroup %>%
  slice_max(downloads, n = 10)

# Models with top 10 most downloads over entire period
df_model_top <- df[df$model_name %in% df_model$model_name, ]

df_model_top <- df_model_top %>%
  group_by(date, model_name) %>%
  summarize(downloads = sum(downloads))

p_dl_model <- ggplot(data = df_model_top,
                     aes(x = date, y = downloads, fill = fct_reorder(model_name, desc(downloads)))) +
  geom_line(aes(color = fct_reorder(model_name, desc(downloads)))) +
  geom_point(shape = 21, size = 1.5, colour = "black") +
  theme_light(base_size = 7) +
  scale_y_continuous(breaks = seq(0, max(df_model_top$downloads) + 2000, by = 10000),
                     labels = function(x) format(x, big.mark = " ", decimal.mark = ".", scientific = FALSE)) +
  scale_x_date(date_labels = "%Y-%b",
               breaks = unique(df_total_sum$date)[seq(1, length(unique(df_total_sum$date)), by=2)],
               guide = guide_axis(n.dodge = 2)) +
  expand_limits(y = 0) +
  labs(y = "Number of downloads",
       x = "Date",
       title = "Number of downloads by model name for top 10 models",
       fill = "Model") +
  guides(color = "none")


ggsave(p_dl_total,
       filename = "plots/downloads_total.jpg",
       dpi = 300,
       width = 1920,
       height = 1080,
       units = "px")


ggsave(p_dl_model,
       filename = "plots/downloads_by_model.jpg",
       dpi = 300,
       width = 1920,
       height = 1080,
       units = "px")
