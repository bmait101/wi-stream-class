# Fetch stream and river fish data from WDNR's FMIS
# Bryan Maitland
# April 2022

# Overview
# - Fetch all data from 1994-2021 on flowing water using target gears

# Notes: 
# - There is a 1000 records max request limit for pulls (server issue)
# - Every pull results in a connection (which slows it all down)


# Libraries
library(wdnr.fmdb)  # internal WDNR package
# wdnr.fmdb::set_fmdb_credentials()


# Set parameters for data pull -------------------------------------------------

yrs <- list(1994:1998, 1999:2003, 2004:2008, 2009:2013, 2014:2018, 2019:2021)

waterbody_types <- c(
  "wadable_stream", 
  "non-wadable_stream", 
  "streams"
)

target_gears <- c(
  "stream_shocker", 
  "backpack_shocker", 
  "mini_boom_shocker", 
  "boom_shocker", 
  "hoop_net", 
  "fyke_net", 
  "mini_fyke_net", 
  "bottom_gill_net",
  "seine", 
  "setline"
)

# Fetch data -------------------------------------------------------------------

# Survey data (42 seconds)
system.time(
  df_surveys_raw <- yrs %>%
    purrr::map_df(~get_fmdb_surveys(
      year = .,
      waterbody_type = waterbody_types
    ))
)

# Efforts data (70 seconds)
system.time(
  df_efforts_raw <- yrs %>%
    purrr::map_df(~get_fmdb_efforts(
      year = .,
      waterbody_type = waterbody_types, 
      gear = target_gears
    ))
)

# Fishraw data (5.4 minutes)
system.time(
  df_fishraw <- yrs %>%
    purrr::map_df(~get_fmdb_fishraw(
      year = .,
      waterbody_type = waterbody_types, 
      gear = target_gears
    ))
)


# Clean up
rm(length_warning_rows); rm(length_deleted_rows) 
rm(yrs); rm(target_gears); rm(waterbody_types)


# Save raw pulls ===============================================================

saveRDS(df_efforts_raw, here::here("data", "raw_efforts_20220413.rds"))
saveRDS(df_surveys_raw, here::here("data", "raw_surveys_20220413.rds"))
saveRDS(df_fishraw, here::here("data", "raw_fish_20220413.rds"))


# END Data Pull ================================================================

