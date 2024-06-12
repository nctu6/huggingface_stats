library(rvest)
library(dplyr)

get_organization_models <- function(org_url){
  organization <- read_html(org_url)
  xpath <- "/html/body/div[1]/main/div/section[2]/div[3]/div/div/div"

  organization_models <- organization %>%
    html_nodes(xpath=xpath) %>%
    html_nodes("a") %>%
    html_attr("href")

  return(organization_models)
}

get_organization_datasets <- function(org_url){
  html <- read_html(org_url)
  xpath <- "/html/body/div[1]/main/div/section[2]/div[4]/div"

  organization_datasets <- html %>%
    html_nodes(xpath=xpath) %>%
    html_nodes("a") %>%
    html_attr("href")

  return(organization_datasets)
}

get_download_stats <- function(url, type="model"){
  if (type == "model"){
    url <- gsub("https://huggingface.co/", "", url)
    downloads <- hub_repo_info(url)["downloads"]$downloads
  } else if (type == "dataset") {
    url <- gsub("https://huggingface.co/datasets/", "", url)
    downloads <- hub_dataset_info(url)["downloads"]$downloads
  }

  downloads <- gsub("\\,", "", downloads) # remove commas in e.g. 1,281,893
  return(list("model_url" = url,
              "downloads" = as.numeric(downloads)))
}

library(hfhub)

# Copy outer html from Firefox/Chrome Inspect tool after clicking "Expand models"
# https://huggingface.co/taide
taide_models <- get_organization_models("taide.html")
taide_datasets <- get_organization_datasets("taide.html")

# Error handling in case of private models
taide_models <- taide_models[!stringr::str_detect(taide_models, "Model|cp|GGUF")]
taide_datasets <- taide_datasets[!stringr::str_detect(taide_datasets, "ft")]
poss_get_download_stats <- purrr::possibly(get_download_stats, otherwise=NULL)

df_model <- purrr::map_df(taide_models, ~poss_get_download_stats(.x, type="model"))
df_dataset <- purrr::map_df(taide_datasets, ~poss_get_download_stats(.x, type="dataset"))

# Remove all models that are only tokenizers
df_model <- df_model %>%
  filter(!stringr::str_detect(model_url, "tokenizer"))

# Match everything between penultimate and last '/' in URL.
df_model$organization <- stringr::str_extract(string = df_model$model_url, "(?<=co/).*(?=/)")
df_dataset$organization <- stringr::str_extract(string = df_dataset$model_url, "(?<=datasets/).*(?=/)")
# Match everything after last '/' in URL.
df_model$model_name <- stringr::str_extract(string = df_model$model_url, "[^/]*$")
df_dataset$model_name <- stringr::str_extract(string = df_dataset$model_url, "[^/]*$") # dataset name
df_model$date <- Sys.Date()
df_dataset$date <- Sys.Date()

dir.create("data")
readr::write_csv(df_model, file = paste0("data/models/", Sys.Date(), "_hf.csv"))
readr::write_csv(df_dataset, file = paste0("data/datasets/", Sys.Date(), "_hf.csv"))
