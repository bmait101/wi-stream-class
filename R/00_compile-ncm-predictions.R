# Connect to and pull data from local MS Access database
# Bryan M Maitland
# April 2022

# ==============================================================================

# Workflow
# - switch to 32-bit R (global options, then restart R)
# - connect to MS Access database
# - pull data
# - save data
# - switch back to 64-bit R


# Switch to 32-bit R (MS Access is 32-bit) =====================================


# Connect and load data ========================================================

# packages
library(RODBC)  

# Open DB connection
con <- odbcConnectAccess(
  here::here("data","whd","DataHydro24k.mdb"))

# Check available tables in WHD Plus
sqlTables(con)

# Pull tables
whd_flow_temp <- sqlFetch(con, "flow_temperature")

# Close DB connection
odbcClose(con)

# Save data ============================================

saveRDS(whd_flow_temp, here::here("data", "whd_ncm_preds.rds"))


### END 32-bit R session ###


# Switch to 32-bit R (MS Access is 32-bit) =====================================
