# Prepare fish and covariate data for multivariate analysis
# Bryan M Maitland
# June 2022

# Libraries
library(here)
library(tidyverse)


## Data ------------------------------------------------------------------------

# WDNR 24k WHD line feature layer
# whd_lines <- readRDS(here("data", "whd_lines.rds"))

# WDNR 24k WHD VA stream and watershed attribute data
whd_data <- 
  readRDS(here("data", "whd_data.rds")) %>% 
  rename(reach_id = reachid) %>% 
  filter(hyd_cat == "stream")  # just keep stream records

# NCM model predicted flow and temperature metrics
whd_ncm <- readRDS(here("data", "whd_ncm_preds.rds")) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  rename(reach_id = reachid)

# Metatdata for WHD data
# whd_metadata <- readRDS(here("data", "whd_metadata.rds"))

# Fisheries surveys data (cleaned)
load(here("data", "fish_data_clean.RData"))

# FMDB x WHD cross reference table
sites_xref <- readRDS(here("data", "sites_whd_xref.rds"))


## Prep Covariates -------------------------------------------------------------

# NCM classes 
whd_ncm_classes <- whd_ncm %>% select(1:3) 

# NCM flow and temp metrics (used in Diebel et al fish analyses)
whd_ncm <- whd_ncm %>% 
  select(
    1 | 
      c(temp_max_cl_cc,  temp_july_cl_cc, temp_summer_cl_cc) | 
      c(apr_e10_c, aug_e50_c, aug_e50_c, wy_e50_c, wy_e90_c)
    )

# Pull a priori covariates (used in Diebel et al fish analyses)
whd_data <- whd_data %>% 
  select(
    c(
      # ID
      reach_id,
      # stream size 
      trw_area,
      # channel
      gradient, sinuosity, 
      # connectivity
      gl_dist, lake_sm_dist, lake_md_dist, lake_lg_dist, shed_100_dist, shed_1000_dist,
      # Valley form, soil permeability, and groundwater potential
      r_darcy, trr_darcy, w_darcy, trw_darcy, 
      r_slope, trr_slope, w_slope, trw_slope, 
      r_perm, trr_perm, w_perm, trw_perm, 
      # Surficial and bedrock geology in upstream watershed
      trw_br_1, trw_br_2, trw_br_3, trw_br_41, trw_br_42, 
      # Climate
      w_prcp_ann, trw_prcp_ann, 
      w_temp_ann, w_temp_gs, w_temp_july, 
      trw_temp_ann, trw_temp_gs, trw_temp_july,
      # adjacent land cover
      r_lu06_water = r_lu06_11,                      # water
      r_lu06_81, r_lu06_82,                          # ag
      r_lu06_41, r_lu06_42, r_lu06_43,               # forest
      r_lu06_90, r_lu06_95,                          # wetland
      r_lu06_21, r_lu06_22, r_lu06_23, r_lu06_24,    # urban
      r_lu06_grass = r_lu06_71,                      # grassland
      # upstream land cover
      trr_lu06_water = trr_lu06_11, 
      trr_lu06_81, trr_lu06_82,
      trr_lu06_41, trr_lu06_42, trr_lu06_43,
      trr_lu06_90, trr_lu06_95,   
      trr_lu06_21, trr_lu06_22, trr_lu06_23, trr_lu06_24, 
      trr_lu06_grass = trr_lu06_71,
      # adjacent watershed land cover
      w_lu06_water = w_lu06_11, 
      w_lu06_81, w_lu06_82,
      w_lu06_41, w_lu06_42, w_lu06_43,
      w_lu06_90, w_lu06_95,   
      w_lu06_21, w_lu06_22, w_lu06_23, w_lu06_24, 
      w_lu06_grass = w_lu06_71,
      # upstream watershed land cover
      trw_lu06_water = trw_lu06_11, 
      trw_lu06_81, trw_lu06_82,
      trw_lu06_41, trw_lu06_42, trw_lu06_43,
      trw_lu06_90, trw_lu06_95,   
      trw_lu06_21, trw_lu06_22, trw_lu06_23, trw_lu06_24, 
      trw_lu06_grass = trw_lu06_71
      )
    ) %>% 
  
  # Combine difference land classes into broader groups
  mutate(
    r_lu06_agric = r_lu06_81 + r_lu06_82, 
    r_lu06_forst = r_lu06_41 + r_lu06_42 + r_lu06_43, 
    r_lu06_wetld = r_lu06_90 + r_lu06_95,
    r_lu06_urban = r_lu06_21 + r_lu06_22 + r_lu06_23 + r_lu06_24, 
    
    trr_lu06_agric = trr_lu06_81 + trr_lu06_82, 
    trr_lu06_forst = trr_lu06_41 + trr_lu06_42 + trr_lu06_43, 
    trr_lu06_wetld = trr_lu06_90 + trr_lu06_95,
    trr_lu06_urban = trr_lu06_21 + trr_lu06_22 + trr_lu06_23 + r_lu06_24, 
    
    w_lu06_agric = w_lu06_81 + w_lu06_82, 
    w_lu06_forst = w_lu06_41 + w_lu06_42 + w_lu06_43, 
    w_lu06_wetld = w_lu06_90 + w_lu06_95,
    w_lu06_urban = w_lu06_21 + w_lu06_22 + w_lu06_23 + w_lu06_24, 
    
    trw_lu06_agric = trw_lu06_81 + trw_lu06_82, 
    trw_lu06_forst = trw_lu06_41 + trw_lu06_42 + trw_lu06_43, 
    trw_lu06_wetld = trw_lu06_90 + trw_lu06_95,
    trw_lu06_urban = trw_lu06_21 + trw_lu06_22 + trw_lu06_23 + trw_lu06_24, 
    ) %>%
  
  # Remove the land subclasses that were combined above
  select(
    -r_lu06_81, -r_lu06_82, -r_lu06_41, -r_lu06_42, -r_lu06_43, 
    -r_lu06_90, -r_lu06_95, -r_lu06_21, -r_lu06_22, -r_lu06_23, -r_lu06_24,
    -trr_lu06_81, -trr_lu06_82, -trr_lu06_41, -trr_lu06_42, -trr_lu06_43, 
    -trr_lu06_90, -trr_lu06_95, -trr_lu06_21, -trr_lu06_22, -trr_lu06_23, -trr_lu06_24,
    -w_lu06_81, -w_lu06_82, -w_lu06_41, -w_lu06_42, -w_lu06_43, 
    -w_lu06_90, -w_lu06_95, -w_lu06_21, -w_lu06_22, -w_lu06_23, -w_lu06_24,
    -trw_lu06_81, -trw_lu06_82, -trw_lu06_41, -trw_lu06_42, -trw_lu06_43, 
    -trw_lu06_90, -trw_lu06_95, -trw_lu06_21, -trw_lu06_22, -trw_lu06_23, -trw_lu06_24
  )

# Join NCM metrics to selected WHD covariates
whd_data <- whd_data %>% left_join(whd_ncm, by = "reach_id")

# Clean up
rm(whd_ncm)

## Prep fish data --------------------------------------------------------------

### Missing reach_ids ----

# Add WHD key (reachid) to fish data
df_fish_long <- 
  df_fish_long %>% 
  as_tibble() %>% 
  mutate(site.seq.no = as.character(site.seq.no)) %>% 
  left_join(sites_xref, by = "site.seq.no")

# How many records / sites / surveys have missing reach ID?
df_fish_long %>% filter(is.na(reach_id)) %>% tally()  # 51k (>1% of all records)
df_fish_long %>% filter(is.na(reach_id)) %>% distinct(site.seq.no) # 78
df_fish_long %>% filter(is.na(reach_id)) %>% distinct(survey.seq.no) # 203

# Remove data from sites that are missing reachids 
df_fish <- df_fish_long %>% filter(!is.na(reach_id))

rm(sites_xref); rm(df_fish_long)

