###########################################################################
# Code to clean shiny data
##########################################################################

# packages
library(tidyverse)
library(readxl)
library(ggmap)
library(lubridate)

#functions
get_lon_lat <- function(.data, value) {
  value <- enquo(value)
  
  forests <- select(.data, !!value)
  forests <- distinct(forests)
  forest_list <- as.list(forests)
  
  lon_lat <- map(forest_list, geocode)
  lon_lat <- unnest(lon_lat[["National_Forest"]])
  together <- bind_cols(forests, lon_lat)
  .data <- left_join(.data, together, by = "National_Forest")
  
  return(.data)
}

# get data
plants <- read_csv(here::here("Current_Invasive_Plants_Feature_Layer.csv"))

# select columnss
shiny_data <- plants %>%
  select(OBJECTID, ACCEPTED_COMMON_NAME, DATE_COLLECTED, INFESTED_AREA, TOTAL_AREA, FS_UNIT_NAME) %>%
  distinct()

# get national forest
shiny_data <- shiny_data %>%
  mutate(FS_UNIT_NAME = str_to_title(FS_UNIT_NAME),
    National_Forest = str_extract(FS_UNIT_NAME, "[:alpha:]+\\sNational Forest")) %>%
  filter(!is.na(National_Forest))

# correct accepted common names
shiny_data <- shiny_data %>%
  mutate(ACCEPTED_COMMON_NAME = str_to_title(ACCEPTED_COMMON_NAME))

# get latitude and longitdue of national forests, alread done
register_google(key=rstudioapi::askForPassword(prompt = "Please enter your Key"))

shiny_data <- shiny_data %>%
  get_lon_lat(National_Forest)

shiny_data_all <- shiny_data %>%
  mutate(DATE_COLLECTED = as.Date(DATE_COLLECTED)) %>%
  group_by(National_Forest, DATE_COLLECTED) %>%
  mutate(TOTAL_AREA = sum(TOTAL_AREA)) %>%
  group_by(National_Forest, DATE_COLLECTED) %>%
  mutate(INFESTED_AREA = sum(INFESTED_AREA),
         ACCEPTED_COMMON_NAME = "All Species") %>%
  ungroup()

shiny_data <- shiny_data %>%
  mutate(DATE_COLLECTED = as.Date(DATE_COLLECTED)) %>%
  group_by(National_Forest, DATE_COLLECTED) %>%
  mutate(TOTAL_AREA = sum(TOTAL_AREA)) %>%
  group_by(National_Forest, DATE_COLLECTED, ACCEPTED_COMMON_NAME) %>%
  mutate(INFESTED_AREA = sum(INFESTED_AREA)) %>%
  ungroup()

shiny_data <- bind_rows(shiny_data_all, shiny_data)

shiny_data <- shiny_data %>%
  mutate(INFESTED_PERCENT = INFESTED_AREA/TOTAL_AREA)

# filter to only include up to June
shiny_data <- shiny_data %>%
  filter(DATE_COLLECTED <= ymd("2019-06-01") & DATE_COLLECTED >= ymd("1960-01-01"))

# get rid of NAs
shiny_data <- shiny_data %>%
  filter(!is.na(ACCEPTED_COMMON_NAME))

# important columns
shiny_data <- shiny_data %>%
  select(ACCEPTED_COMMON_NAME, National_Forest, DATE_COLLECTED, INFESTED_PERCENT, lon, lat) %>%
  distinct()

# save
write_csv(shiny_data, "clean_data.csv")
