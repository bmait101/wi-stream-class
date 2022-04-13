# Process raw WDNR FWIS data pulls
# Bryan M Maitland
# April 2022

# Libraries
library(tidyverse)

# Data -------------------------------------------------------------------------

df_surveys_raw <- readRDS(here::here("data", "raw_surveys_20220413.rds"))
df_efforts_raw <- readRDS(here::here("data", "raw_efforts_20220413.rds"))
df_fishraw <- readRDS(here::here("data", "raw_efforts_20220413.rds"))


# Initial filters  ----------------------------------------------------

# Filter out surveys with no effort data and that are not proofed

target_survey_statuses <- c(
  "data_entry_complete_and_proofed",
  "historical_data_complete_and_proofed",
  "historical_data_entry_complete",
  "historical_data_load_status_unknown"
)

df_surveys <- df_surveys_raw %>% 
  semi_join(df_efforts_raw, by = "survey.seq.no") %>% 
  filter(survey.status %in% target_survey_statuses)

# filter efforts for proofed surveys and remove the test sites
df_efforts <- df_efforts_raw %>% 
  filter(site.seq.no != 315) %>% 
  filter(! is.na(target.species)) %>% 
  filter(survey.seq.no %in% df_surveys$survey.seq.no)

# then filter the surveys once again to remove any without effort data
df_surveys <- df_surveys %>% 
  semi_join(df_efforts, by = "survey.seq.no")

# check unique survey IDS
length(unique(df_surveys$survey.seq.no))  
length(unique(df_efforts$survey.seq.no))  

# Now filter the fish data by survey seq nos
df_fish <- df_fishraw %>% 
  semi_join(df_efforts, by = "survey.seq.no")
