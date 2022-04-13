# Compile and prepare all covariates for analysis

# Libraries
library(tidyverse)

# Data
whd_ncm <- readRDS(here::here("data", "whd_ncm_preds.rds"))
whd_attr <- readRDS(here::here("data", "whd_data.rds"))
whd_lines <- readRDS(here::here("data", "whd_lines.rds"))

# Extract dataframes from nested tibbble
whd_attr_info <- whd_attr[1,2] %>% unnest(cols = layer_data) %>% 
  select(-table_name) %>% mutate(field_name = tolower(field_name))
whd_attr_base <- whd_attr[2,2] %>% unnest(cols = layer_data)
whd_attr_c <- whd_attr[3,2] %>% unnest(cols = layer_data)
whd_attr_r <- whd_attr[4,2] %>% unnest(cols = layer_data)
whd_attr_trr <- whd_attr[5,2] %>% unnest(cols = layer_data)
whd_attr_trw <- whd_attr[6,2] %>% unnest(cols = layer_data)
whd_attr_w <- whd_attr[7,2] %>% unnest(cols = layer_data)

