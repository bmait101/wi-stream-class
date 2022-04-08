# Fetch stream and river fish data from WDNR's FMIS
# Bryan Maitland
# April 2022

library(tidyverse)
library(here)
library(wdnr.fmdb)  # internal WDNR package
# wdnr.fmdb::set_fmdb_credentials()


# Set some fetching and filtering parameters

yrs <- 1994:2021

waterbody_types <- c("wadable_stream", "non-wadable_stream")

target_survey_statuses <- c(
  "data_entry_complete_and_proofed",
  "historical_data_complete_and_proofed",
  "historical_data_entry_complete",
  "historical_data_load_status_unknown"
  )


# Fetch surveys and efforts ----------------------------------------------------

# Fetch survey data from 1994-2021 on flowing water
system.time(
  df_surveys_raw <- yrs %>%
    map_df(~get_fmdb_surveys(
      year = .,
      waterbody_type = waterbody_types
    ))
)
# 3.1 minutes

# save raw pull
saveRDS(df_surveys_raw, here("data", "raw_surveys_20220408.rds"))
# df_surveys_raw <- read_rds(here("data", "raw_surveys_20220408.rds"))

# Fetch efforts data from 1994-2021 on flowing water
system.time(
  df_efforts_raw <- yrs %>%
    map_df(~get_fmdb_efforts(
      year = .,
      waterbody_type = waterbody_types
    ))
)
# 4.6 minutes

# save raw pull
saveRDS(df_efforts_raw, here("data", "raw_efforts_20220408.rds"))
# df_efforts_raw <- read_rds(here("data", "raw_efforts_20220408.rds"))


# QAQC surveys and efforts  ----------------------------------------------------

# CHECK: There should be fewer in the efforts tibble b/c net data not pulled:
length(unique(df_surveys_raw$survey.seq.no))  
length(unique(df_efforts_raw$survey.seq.no))  

# filter out surveys with no effort data and that are not proofed
df_surveys <- df_surveys_raw %>% 
  semi_join(df_efforts_raw, by = "survey.seq.no") %>% 
  filter(survey.status %in% target_survey_statuses)

# filter efforts for proofed shocking and remove the test sites
df_efforts <- df_efforts_raw %>% 
  filter(site.seq.no != 315) %>% 
  filter(survey.seq.no %in% df_surveys$survey.seq.no)

# check unique survey IDS
length(unique(df_surveys$survey.seq.no))  
length(unique(df_efforts$survey.seq.no))  


# Fetch fish raw data ----------------------------------------------------------

# using efforts, download fish data 1000 efforts at a time
vs <- unique(df_efforts_raw$visit.fish.seq.no)
chunks <- split(vs, ceiling(seq_along(vs)/1000))

df_fishraw <- map_df(chunks ~get_fmdb_fishraw(visit_seq = .))

# Check records
length_deleted_rows
length_warning_rows

# save
saveRDS(df_fishraw, here("fmdb_fishdata_wadeableAndNonwadeable_20211006.rds"))




# END Data Pull ================================================================

