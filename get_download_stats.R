library(rvest)
library(dplyr)
library(hfhub)
library(httr)

hf_token <- Sys.getenv("HUGGING_FACE_HUB_TOKEN")
my_token <- paste0("Bearer ", hf_token)

get_organization_models <- function(org){
  stat_url <- paste0("https://huggingface.co/api/models?author=", org)
  html <- GET(stat_url, add_headers("Authorization" = my_token)) %>% content(as = "parsed")
  organization_models <- purrr::map_df(html, ~list("id" = .x$id)) %>%
    group_by(id) %>%
    filter(!stringr::str_detect(id, "ModelCardReview|cp.|GGUF")) %>%
    ungroup()

  return(organization_models)
}

get_organization_datasets <- function(org){
  stat_url <- paste0("https://huggingface.co/api/datasets?author=", org)
  html <- GET(stat_url, add_headers("Authorization" = my_token)) %>% content(as = "parsed")
  organization_datasets <- purrr::map_df(html, ~list("id" = .x$id)) %>%
    group_by(id) %>%
    filter(!stringr::str_detect(id, "ft")) %>%
    ungroup()

  return(organization_datasets)
}

get_download_stats <- function(url, type="model"){
  if (type == "model"){
    stat_url <- paste0("https://huggingface.co/api/models/", url, "?expand%5B%5D=downloads&expand%5B%5D=downloadsAllTime")
  } else if (type == "dataset") {
    stat_url <- paste0("https://huggingface.co/api/datasets/", url, "?expand%5B%5D=downloads&expand%5B%5D=downloadsAllTime")
  }

  html <- GET(stat_url, add_headers("Authorization" = my_token)) %>% content(as = "parsed")
  downloads <- html$downloads
  downloadsAllTime <- html$downloadsAllTime
  downloads <- gsub("\\,", "", downloads) # remove commas in e.g. 1,281,893
  downloadsAllTime <- gsub("\\,", "", downloadsAllTime) # remove commas in e.g. 1,281,893
  return(list("model_url" = url,
              "downloads" = as.numeric(downloads),
              "downloadsAllTime" = as.numeric(downloadsAllTime)
              ))
}


# model_url <- "https://huggingface.co/taide"
taide_models <- get_organization_models("taide")
taide_datasets <- get_organization_datasets("taide")

taide_models <- array(unlist(taide_models))
taide_datasets <- array(unlist(taide_datasets))

poss_get_download_stats <- purrr::possibly(get_download_stats, otherwise=NULL)
df_model <- purrr::map_df(taide_models, ~poss_get_download_stats(.x, type="model"))
df_dataset <- purrr::map_df(taide_datasets, ~poss_get_download_stats(.x, type="dataset"))

# Remove all models that are only tokenizers
df_model <- df_model %>%
  filter(!stringr::str_detect(model_url, "tokenizer"))

# Match everything between penultimate and last '/' in URL.
df_model$organization <- stringr::str_extract(string = df_model$model_url, ".*(?=/)")
df_dataset$organization <- stringr::str_extract(string = df_dataset$model_url, ".*(?=/)")
# Match everything after last '/' in URL.
df_model$model_name <- stringr::str_extract(string = df_model$model_url, "[^/]*$")
df_dataset$model_name <- stringr::str_extract(string = df_dataset$model_url, "[^/]*$") # dataset name
df_model$date <- Sys.Date()
df_dataset$date <- Sys.Date()

dir.create("data")
readr::write_csv(df_model, file = paste0("data/models/", Sys.Date(), "_hf.csv"))
readr::write_csv(df_dataset, file = paste0("data/datasets/", Sys.Date(), "_hf.csv"))
