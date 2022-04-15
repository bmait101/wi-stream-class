# Process raw WDNR FWIS data pulls
# Bryan M Maitland
# April 2022


library(dplyr)


# Data -------------------------------------------------------------------------

df_surveys_raw <- readRDS(here::here("data", "raw_surveys_20220413.rds"))
df_efforts_raw <- readRDS(here::here("data", "raw_efforts_20220413.rds"))
df_fishraw <- readRDS(here::here("data", "raw_fish_20220413.rds"))


# Filter for good data  -------------------------

df_surveys <- df_surveys_raw %>% 
  # surveys with fish data
  semi_join(df_fishraw, by = "survey.seq.no") %>%  
  # remove test site
  filter(site.seq.no != 315) %>%  
  # historical / complete and proofed data (don't keep recent, un-proofed data)
  filter(survey.status %in% c(
    "data_entry_complete_and_proofed",
    "historical_data_complete_and_proofed",
    "historical_data_entry_complete",
    "historical_data_load_status_unknown"
  ))


# Filter fish for good surveys
df_fish <- df_fishraw %>% semi_join(df_surveys, by = "survey.seq.no")

# Filter for efforts with fish data from good surveys
df_efforts <- df_efforts_raw %>% semi_join(df_surveys, by = "survey.seq.no")


# QC fish data ------------------------

# How many species == "no_fish_captured" 
df_fish %>% filter(species == "no_fish_captured") %>% tally() # 757

# Save these surveys seq 
no_fish_surveys <- df_fish %>% 
  filter(species == "no_fish_captured") %>% 
  distinct(survey.seq.no) %>% 
  pull()

# and Remove these records 
df_fish <- df_fish %>% filter(!species == "no_fish_captured") %>% droplevels()


# How many NAs for fish counts?
df_fish %>% filter(is.na(fish.count)) %>% tally()  # 0

# How many zero counts?
df_fish %>% filter(fish.count == 0) %>% tally()  # 0


# check species levels
levels(as.factor(df_fish$species))
df_fish %>% group_by(species) %>% count() %>% arrange(desc(n)) %>% print(n=Inf)




## Expand counts ------------------

df_trout <- df_trout %>% 
  wdnr.fmdb::expand_counts() %>% 
  wdnr.fmdb::length_bin_to_length()

# convert lengths to mm
df_trout <- df_trout %>% 
  mutate(length_mm = length * 25.4)

