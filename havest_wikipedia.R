# code to get plant info

library(rvest)
library(tidyverse)
library(xml2)

get_blurb <- function(plant, type = "words") {
  
  webpage <- read_html(paste0("https://en.wikipedia.org/wiki/", plant))
  if (type == "words") {
  blurb <- html_nodes(webpage, "p")
  blurb <- html_text(blurb)
  } else {
   wiki_image <-html_nodes(webpage, ".thumbimage")
   wiki_image <- html_attr(wiki_image[1], "src")
   download.file(paste0("https:", wiki_image), destfile =  here::here(paste0("Invasive_Plants/images/",basename(wiki_image))), mode = "wb", quiet = TRUE)
   blurb <- wiki_image
  }
  
  return(blurb)
}

get_wikipedia_entry <- function(plant, type) {
  
  result <- tryCatch(
    {
      get_blurb(plant, type)
    }, 
    error = function(cond) {
      return(NA)
    },
    warning = function(cond) {
      return("This is a warning")
    }
  )
  return(result)
}

get_entry_with_progress <- function(plants, type){
  pb$tick()$print()
  result <- get_wikipedia_entry(plants, type)
}

plants <- shiny_data %>% 
  select(ACCEPTED_COMMON_NAME) %>% 
  distinct() %>% 
  filter(ACCEPTED_COMMON_NAME != "All Species") %>%
  mutate(ACCEPTED_COMMON_NAME = str_to_sentence(ACCEPTED_COMMON_NAME),
         ACCEPTED_COMMON_NAME = str_replace_all(ACCEPTED_COMMON_NAME, " ", "_"))
  

plants <- plants %>%
  as.list() %>%
  flatten()

names(plants) <- plants


pb <- progress_estimated(length(plants))

# get text
plant_wikipeida <- plants %>%
  map(~get_entry_with_progress(., type = "words"))

# get images
result <- map(plants, get_entry_with_progress, type = "images")

# fomrat wikipedia text to add to data
plant_wikipeida <- plant_wikipeida %>%
  unlist() %>%
  enframe() %>%
  mutate(name = str_remove(name, "[:digit:]+")) %>%
  group_by(name) %>%
  mutate(value = paste(value, collapse = " "),
         number = seq_along(name)) %>%
  ungroup() %>%
  filter(number == 1) %>%
  select(-number) %>%
  rename("wikipedia_text" = value,
         "ACCEPTED_COMMON_NAME" = name) %>%
  mutate(ACCEPTED_COMMON_NAME = str_replace_all(ACCEPTED_COMMON_NAME, "_", " "),
         ACCEPTED_COMMON_NAME = str_to_title(ACCEPTED_COMMON_NAME))

shiny_data <- shiny_data %>%
  left_join(plant_wikipeida, by = "ACCEPTED_COMMON_NAME")

shiny_data <- shiny_data %>%
  mutate(wikipedia_text = if_else(is.na(wikipedia_text)| wikipedia_text == "NA", "No Entry Found", wikipedia_text))


