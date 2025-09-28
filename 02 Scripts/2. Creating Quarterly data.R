########################################################################
# Title: 2. Creating Quarterly data
# Author: Lea Roeller
# Date: 28/09/2025
#######################################################################

# In this file, we work with monthly trade data
# 1. Clean the data for countries as trade partners only
# 2. We create quarterly data
# 3. We are looking for the top 5 commodities according to HS2 codes, where Canada and Mexico are the strongest importers according to USD value
# 4. We collect those 5 commodities and other strong (5) importing countries. The rest will be summarised as ROW
# 5. We calculate shares to show absolute and relative "import dependence"
# 6. We display data in stacked bar charts

## 0. Housekeeping ----
rm(list = ls())

## 1. Cleaning the countries ---- 

load("01 RDS/US Census Monthly.RData")

#start by dropping country_codes that we do not need anymore: 
# CTY_CODE: - = TOTAL FOR ALL COUNTRIES
# CTY_CODE: 00XX = represents country groupings, e.g. NATO, APEC, EU etc.
# CTY_CODE: 1XXX - 7XXX = represents the continents

full_imports <- full_imports %>% filter(!(grepl("^00\\d{2}$", CTY_CODE)| grepl("^[1-7]\\XXX", CTY_CODE)))

## 2. Creating quarterly data ---- 

get_quarter <- function(month) { #setting up a quarterly function
  case_when(
    month %in% c("01", "02", "03") ~ "Q1", 
    month %in% c("04", "05", "06") ~ "Q2",
    month %in% c("07", "08", "09") ~ "Q3",
    month %in% c("10", "11", "12") ~ "Q4",
    #TRUE ~ NA_character_
  )
}

full_imports <- full_imports %>% mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) #ensure that data is stored as numerical aka destring

full_imports <- full_imports %>%
  mutate(QUARTER = get_quarter(MONTH)) %>% ## call the function and assign quarters accordingly
  group_by(YEAR, QUARTER, I_COMMODITY, I_COMMODITY_LDESC, CTY_CODE, CTY_NAME) %>%  # now group by quarters
  summarise(TOTAL_GEN_VAL_MO = sum(GEN_VAL_MO, na.rm = TRUE), .groups = "drop") # sum up 

## 3. Exporting data ----

save(full_imports, file = "01 RDS/US Census Quarterly.RData")
