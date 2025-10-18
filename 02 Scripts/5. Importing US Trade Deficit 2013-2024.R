########################################################################
# Title: 5. Importing annual trade deficit US - 2013 to 2024
# Author: Lea Roeller
# Date: 18/10/2025
#######################################################################

## 0. Housekeeping ----
rm(list = ls())

## 1. Importing Import Data ----

month <- sprintf("%02d", 1:12)
year <- as.character(2013:2024)
final_data <- list()


for (y in year) {
  for (m in month) {
    # Print the processing message
    print(paste("Processing", y, "-", m))
    # Construct the URL 
    url <- glue("https://api.census.gov/data/timeseries/intltrade/imports/hs?get=CTY_CODE,CTY_NAME,GEN_VAL_MO&YEAR={y}&MONTH={m}")
    json_data <- fromJSON(url)  # Read in JSON
    df <- as.data.frame(json_data) # Convert to a dataframe
    colnames(df) <- df[1, ]  # Set the first row as column names
    df <- df[-1, ] # Remove the first row from data
    final_data[[length(final_data) + 1]] <- df
  }
}
final_data <-do.call(rbind,final_data)

imports <- final_data
rm(df, json_data, final_data)
save(imports, file = "01 RDS/US Imports 2013-2024.rds")

## 2. Importing Export Data ----
final_data <- list()

for (y in year) {
  for (m in month) {
    # Print the processing message
    print(paste("Processing", y, "-", m))
    # Construct the URL 
    url <- glue("https://api.census.gov/data/timeseries/intltrade/exports/hs?get=CTY_CODE,CTY_NAME,ALL_VAL_MO&YEAR={y}&MONTH={m}")
    json_data <- fromJSON(url) # Read in JSON
    df <- as.data.frame(json_data) # Convert to a dataframe
    colnames(df) <- df[1, ] # Set the first row as column names
    df <- df[-1, ] # Remove the first row from data
    final_data[[length(final_data) + 1]] <- df
  }
}
final_data <-do.call(rbind,final_data)
exports <- final_data
rm(df, json_data, final_data)

save(exports, file = "01 RDS/US Exports 2013-2024.rds")

## 3. Merging Import and Export data ----

# Rename columns before merging
imports <- imports %>%
  rename(imports = GEN_VAL_MO)%>%
  mutate(imports = as.numeric(imports))

exports <- exports %>%
  rename(exports = ALL_VAL_MO) %>%
  mutate(exports = as.numeric(exports))

trade_data <- merge(imports, exports, by = c("CTY_CODE", "CTY_NAME", "YEAR", "MONTH"), all = TRUE)

# turn into yearly data
trade_data <- trade_data %>%
  group_by(CTY_CODE, CTY_NAME, YEAR) %>%
  summarise(
    imports = sum(imports, na.rm = TRUE),
    exports = sum(exports, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate the trade deficit
trade_data$deficit <- trade_data$exports - trade_data$imports

## 4. Exporting ----

save(trade_data, file = "01 RDS/US Deficit 2013-2024.rds")

