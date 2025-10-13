########################################################################
# Title: 1. Importing Monthly US Commdities Imports
# Author: Lea Roeller
# Date: 28/00/2025
#######################################################################

## 0. Housekeeping ----
rm(list = ls())

## 1. Importing data ----

# More information on how to construct the URL / API can be found in the US Census guide linked the line below
# US Census trade manual: https://www.census.gov/foreign-trade/reference/guides/Guide_to_International_Trade_Datasets.pdf
# We are currently downloading US Commodities imports in $ with all available counterpart areas per month using HS2 codes

month <- sprintf("%02d", 1:12) # generate the month variable
year <- as.character(2023:2024) # generate the year variable

full_imports <- list() # initialise a list for the data to be stored in

for (y in year) {
  for (m in month) {
    print(paste("Processing", y, "-", m)) # Print the processing message
    url <- paste0(
      "https://api.census.gov/data/timeseries/intltrade/imports/hs?get=",
      "I_COMMODITY,I_COMMODITY_LDESC,I_COMMODITY_SDESC,", # import variables
      "CTY_CODE,CTY_NAME,GEN_VAL_MO", # levels
      "&YEAR=", y, # year
      "&MONTH=", m, # month
      "&COMM_LVL=HS2" #HS2 codes
    )
    
    json_data <- fromJSON(url) # data comes in JSON format
    monthly_imports <- as.data.frame(json_data)  # convert to a dataframe
    colnames(monthly_imports) <- monthly_imports[1, ] # set the first row as column names
    monthly_imports <- monthly_imports[-1, ]  # remove the first row from data
    full_imports[[length(full_imports) + 1]] <- monthly_imports
  }
}
full_imports <-do.call(rbind,full_imports) #bind it all together

## 2. Exporting data ----

save(full_imports, file = "01 RDS/US Census Monthly.rds")
