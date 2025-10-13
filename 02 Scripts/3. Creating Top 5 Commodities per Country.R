########################################################################
# Title: 2. Creating Top 5 
# Author: Lea Roeller
# Date: 28/09/2025
#######################################################################

## 0. Housekeeping ----
rm(list = ls())

## 1. Importing the data ---- 
load("01 RDS/US Census Quarterly.rds")

## 2. Creating Top 5 HS codes ----

countries <- c("CANADA", "MEXICO") # more countries can simply be added to this list

for (c in countries) {

#2.1 Filtering for the Top 5 Commodities of that specific country ----
  
  top5 <- full_imports %>%
    filter(CTY_NAME == c) %>% # filter for country
    group_by(YEAR, I_COMMODITY) %>% # group by year and commodity
    summarise(TOTAL_GEN_VAL_MO = sum(TOTAL_GEN_VAL_MO, na.rm = TRUE), .groups = "drop") %>% # summarise the value per commodity and year
    filter(YEAR == "2024") %>% # we only want to keep the top 5 for 2024 for now
    slice_max(order_by = TOTAL_GEN_VAL_MO, n = 5, with_ties = FALSE) %>% # keep the top 5
    ungroup()

#2.2 Filtering for the Top 5 Countries for specific commodity ----
  
  country <- full_imports %>% semi_join(top5, by = "I_COMMODITY") # semi join makes sure we only keep top 5 commodities
  country <- country %>%
    group_by(YEAR, QUARTER, I_COMMODITY) %>%
    slice_max(order_by = TOTAL_GEN_VAL_MO, n = 6, with_ties = FALSE) %>% # Select top 5 countries + Total for the world
    ungroup()
  
#2.3 Computing the ROW values ---- 
  
  data_with_ROW <- country %>%
    group_by(YEAR, QUARTER, I_COMMODITY,I_COMMODITY_LDESC,) %>%
    summarise(
      TOTAL_ALL_COUNTRIES = sum(TOTAL_GEN_VAL_MO[CTY_CODE == "-"], na.rm = TRUE),  # Get total
      TOTAL_OTHERS = sum(TOTAL_GEN_VAL_MO[CTY_CODE != "-"], na.rm = TRUE),  # Sum all except "-"
      .groups = "drop"
    ) %>%
    mutate(
      CTY_CODE = "ROW", #give it a new code
      CTY_NAME = "ROW", # give it a new name
      TOTAL_GEN_VAL_MO = TOTAL_ALL_COUNTRIES - TOTAL_OTHERS  # ROW = Total - Others
    ) %>%
    select(YEAR, QUARTER, I_COMMODITY, I_COMMODITY_LDESC, CTY_CODE, CTY_NAME, TOTAL_GEN_VAL_MO)
  
  country <- bind_rows(country, data_with_ROW) # Append the ROW data to the original summaries
  
#2.4 Constructing the data set name & housekeeping  ---- 
  assign(paste0("Top_", c), country)
  rm(top5, data_with_ROW, country)
  
}

rm(full_imports) # housekeeping

## 3. Computing shares per commodity ----

for (c in countries) {
  Top_country <- paste0("Top_", c) # Construct the df name
  Top <- get(Top_country)  # Retrieve the actual data frame
  
  Top <- Top %>%
    group_by(YEAR, QUARTER, I_COMMODITY) %>%
    mutate(
      TOTAL_FOR_COMMODITY = sum(TOTAL_GEN_VAL_MO[CTY_CODE == "-"], na.rm = TRUE),  # Get total "-"
      SHARE = ifelse(TOTAL_FOR_COMMODITY > 0, TOTAL_GEN_VAL_MO / TOTAL_FOR_COMMODITY *100, NA) # Obtain the actual share
    ) %>%
    ungroup()
  
  Top <- Top %>% filter(CTY_CODE != "-")  # Remove total rows
  Top <- Top %>% select(-TOTAL_FOR_COMMODITY)  # Drop the total column

  assign(Top_country, Top, envir = .GlobalEnv) # re-assigning name
  
}

rm(Top, c, countries, name, Top_country, top_datasets) # housekeeping

## 4. Exporting the shares ----

top_datasets <- ls(pattern = "^Top_")

for (name in top_datasets) { # Save each one as an individual .rds file
  saveRDS(
    get(name),
    file = file.path("01 RDS/Country Top 5", paste0(name, ".rds"))
  )
}


