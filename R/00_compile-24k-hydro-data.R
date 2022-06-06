# Compile WDNR 24k Hydro Lines and Datasets
# Bryan Maitland
# June 2022

# In: 
# - WDNR 24k Geodatabase
# - WDNR 24k VA Geodatabase

# Out: 
# - whd_lines
# - whd_data
# - whd_metadata

# libraries
library(here)
library(tidyverse)
library(sf)


## Data

# WDNR 24k Hydro Geodatabases must be downloaded to 'data' dir. 
# Available at: https://www.arcgis.com/home/item.html?id=c4bc634ba115498487174bda137f8de8


### Flowline spatial features ----------

# Set paths for 24k WHD flowlines on local drive
path.24k.va <- here("data","whd","wdnr_24k_hydro_va.gdb")
path.24k <- here("data","whd","wdnr_24k_hydro.gdb")

# Read in 24k WHD VA flowlines
whd_lines  <-
  st_read(dsn = path.24k.va, layer = "WD_HYDRO_VA_FLWLN_NTWRK_LN_24K") %>% 
  janitor::clean_names()

# filter out GLs and dangle features, keeping only streams and lake line connectors
whd_lines  <- whd_lines  %>% 
  filter(seedtype %in% c("isolated stream","network stream", "lake")) %>% 
  filter(!is.na(hydroid))  # removes 1 very small lake

# Flowlines do not have WBICS; get from 24k base flowlines  
tmp_lines <-
  rgdal::readOGR(dsn = path.24k, layer = "WD_HYDRO_FLOWLINE_LN_24K") %>%
  st_as_sf() %>%
  janitor::clean_names() %>% 
  st_drop_geometry() %>%
  select(hydroid, hydrocode, hydrotype, wbic = river_sys_wbic) %>%
  as_tibble()

# Add wbics to the flowline sf
whd_lines <- left_join(whd_lines, tmp_lines, by = "hydroid")

# Clean up
rm(tmp_lines)

# Save rds object
saveRDS(whd_lines, here("data", "whd_lines.rds"))


### 24k WHD attribute data -------------

# Data stored as attributes tables in 24k VA GDB
# Characteristic of hydrographic features calculated at 5 spatial scales
# - channel
# - incremental riparian
# - trace (cumulative) riparian
# - incremental watershed
# - trace (cumulative) watershed

# Make list of layers to read in:
target_layers <- 
  st_layers(dsn = path.24k.va)$name %>% 
  as_tibble() %>% 
  filter(str_detect(value, "INFO|BASE|CHANNEL|RIPARIAN|WATERSHED")) %>% 
  pull()

# Make function to read and clean tables:
load_24k_data <- function(path, layer_name) {
  st_read(dsn = path, layer = layer_name) %>% 
    janitor::clean_names() %>% 
    as_tibble()
}

# Apply the function to read data tables into a nested dataframe:
whd_data <-
  data_frame(layer = target_layers) %>%
  mutate(
    layer_data = map(
      layer, 
      ~load_24k_data(path = path.24k.va, layer_name = .))
    )

# Check it
whd_data


# Extract whd attributes

whd_metadata <- whd_data[1,2] %>% 
  unnest(cols = layer_data) %>% 
  select(-table_name) %>% 
  mutate(field_name = tolower(field_name))

whd_base <- whd_data[2,2] %>% unnest(cols = layer_data) 
whd_c <- whd_data[3,2] %>% unnest(cols = layer_data)
whd_r <- whd_data[4,2] %>% unnest(cols = layer_data) 
whd_trr <- whd_data[5,2] %>% unnest(cols = layer_data)
whd_w <- whd_data[7,2] %>% unnest(cols = layer_data) 
whd_trw <- whd_data[6,2] %>% unnest(cols = layer_data)

whd_data_tidy <- whd_base %>% 
  left_join(whd_c, by = "reachid") %>% 
  left_join(whd_r, by = "reachid") %>% 
  left_join(whd_trr, by = "reachid") %>% 
  left_join(whd_w, by = "reachid") %>% 
  left_join(whd_trw, by = "reachid")

# Save rds objects
saveRDS(whd_metadata, here("data", "whd_metadata.rds"))
saveRDS(whd_data_tidy, here("data", "whd_data.rds"))

