########################################################################
# Title: 6  Cleaning  & Preparing US Trade Deficit 2013-2024
# Author: Lea Roeller
# Date: 18/10/2025
#######################################################################

## 0. Housekeeping ----
rm(list = ls())

## 1. Importing Import Data ----

load("01 RDS/US Deficit 2013-2024.rds")

## 2. Cleaning of relevant countries ----

#Clean so that we only keep the country names
# CTY_CODE: - = TOTAL FOR ALL COUNTRIES
# CTY_CODE: 00XX = represents country groupings, e.g. NATO, APEC, EU etc.
# CTY_CODE: 1XXX - 7XXX = represents the continents

trade_data <- trade_data%>%filter(!(grepl("^00\\d{2}$", CTY_CODE) | grepl("^[1-7]\\XXX", CTY_CODE)))

## 3. Obtaining ISO Codes ----

url <- "https://www.census.gov/foreign-trade/schedules/c/country.txt" # Importing the ISO 2 codes
US_codes <- read.delim(url, header = FALSE, sep = "|", strip.white = TRUE)
colnames(US_codes) <- as.character(unlist(US_codes[4, ]))
US_codes<- US_codes[-c(1:5), ]

US_codes <- US_codes %>%
  rename(CTY_CODE = Code) %>%  # Rename 'Code' to 'CTY_CODE'
  select(CTY_CODE, 'ISO Code')  # Keep other relevant columns

trade_data <- trade_data %>% left_join(US_codes, by = "CTY_CODE")

## 4. Getting in interesting country groups ----

# Specified countries that need to be flagged: 

# EU27 countries
# China
# Canada
# Mexico
# Viet Nam
# ASEAN excl Viet Nam
# Other

EU_countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", 
                  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", 
                  "PL", "PT", "RO", "SK", "SI", "ES", "SE")

ASEAN_countries <- c("BN", "KH", "ID", "LA", "MY", "MM", "PH", "SG", "TH") #excluding Viet Nam

trade_data <- trade_data %>%
  mutate(Region = case_when(
    `ISO Code` %in% EU_countries ~ "EU27",
    `ISO Code` == "CN" ~ "China",
    `ISO Code` == "CA" ~ "Canada",
    `ISO Code` == "MX" ~ "Mexico",
    `ISO Code` == "VN" ~ "Viet Nam",
    `ISO Code` %in% ASEAN_countries ~ "ASEAN excl Viet Nam",
    `CTY_CODE` == "-" ~ "Total",  # Exclude "-"
    TRUE ~ "Other"  # Everything else is Other
  ))

## 5. Collapsing per country group for graphics ----

trade_data_graphic <- trade_data %>%
  filter(deficit < 0) %>%  # Keep only rows where deficit is negative
  group_by(Region, YEAR) %>%
  summarise(
    imports = sum(imports, na.rm = TRUE),
    exports = sum(exports, na.rm = TRUE),
    deficit = sum(deficit, na.rm = TRUE),
    .groups = "drop"  # Prevents unnecessary grouping
  )

trade_data_graphic <- trade_data_graphic %>%
  select(Region, YEAR, deficit) %>%  # Keep only relevant columns
  pivot_wider(names_from = YEAR, values_from = deficit)  # Pivot to wide format

## 6. Exporting ----

data_list <- list("full data" = trade_data, "graphic data" = trade_data_graphic) # Create a list of dataframes to write to Excel
write_xlsx(data_list, "03 Outputs/6. US Trade Deficit 2013-2024.xlsx") #save to Excel

