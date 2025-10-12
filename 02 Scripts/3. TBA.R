########################################################################
# Title: 2. Creating Top 5 
# Author: Lea Roeller
# Date: 28/09/2025
#######################################################################

## 0. Housekeeping ----
rm(list = ls())

## 1. Importing the data ---- 
load("01 RDS/US Census Quarterly.RData")

## 2. Creating Top 5 HS codes ----

countries <- c("CANADA", "MEXICO") # more countries can simply be added to this list

for (c in countries) {

  top5 <- final_data %>%
    filter(CTY_NAME == c) %>% # filter for country
    group_by(YEAR, I_COMMODITY) %>%
    summarise(TOTAL_GEN_VAL_MO = sum(TOTAL_GEN_VAL_MO, na.rm = TRUE), .groups = "drop") %>%
    group_by(YEAR) %>%
    filter(YEAR == "2024") %>%
    slice_max(order_by = TOTAL_GEN_VAL_MO, n = 5, with_ties = FALSE) %>%
    ungroup()
  
  country <- final_data %>%
    semi_join(top5, by = "I_COMMODITY")
  
  #let's create the top 5
  country <- country %>%
    group_by(YEAR, QUARTER, I_COMMODITY) %>%
    slice_max(order_by = TOTAL_GEN_VAL_MO, n = 6, with_ties = FALSE) %>% # Select top 6 countries
    ungroup()
  
  # Compute ROW values
  data_with_ROW <- country %>%
    group_by(YEAR, QUARTER, I_COMMODITY,I_COMMODITY_LDESC,) %>%
    summarise(
      TOTAL_ALL_COUNTRIES = sum(TOTAL_GEN_VAL_MO[CTY_CODE == "-"], na.rm = TRUE),  # Get total
      TOTAL_OTHERS = sum(TOTAL_GEN_VAL_MO[CTY_CODE != "-"], na.rm = TRUE),  # Sum all except "-"
      .groups = "drop"
    ) %>%
    mutate(
      CTY_CODE = "ROW",
      CTY_NAME = "ROW",
      TOTAL_GEN_VAL_MO = TOTAL_ALL_COUNTRIES - TOTAL_OTHERS  # ROW = Total - Others
    ) %>%
    select(YEAR, QUARTER, I_COMMODITY, I_COMMODITY_LDESC, CTY_CODE, CTY_NAME, TOTAL_GEN_VAL_MO)
  
  # Append the ROW data to the original summaries
  country <- bind_rows(country, data_with_ROW)
  
  # Save the data set as Excel
  #write_xlsx(country, paste0("Output/2. Top_", c,".xlsx"))
  
  #construct data set name & store data there
  assign(paste0("Top_", c), country)
  
  rm(top5, data_with_ROW, country)
  
}

rm(final_data)




#############################################################
# 4. Computing shares
#############################################################

for (c in countries) {
  
  df_name <- paste0("Top_", c)
  Top_c <- get(df_name)  # Retrieve the actual data frame
  
  # Compute SHARE 
  Top_c <- Top_c %>%
    group_by(YEAR, QUARTER, I_COMMODITY) %>%
    mutate(
      TOTAL_FOR_COMMODITY = sum(TOTAL_GEN_VAL_MO[CTY_CODE == "-"], na.rm = TRUE),  # Get total "-"
      SHARE = ifelse(TOTAL_FOR_COMMODITY > 0, TOTAL_GEN_VAL_MO / TOTAL_FOR_COMMODITY *100, NA)
    ) %>%
    ungroup()
  
  Top_c <- Top_c %>%
    filter(CTY_CODE != "-") %>%  # Remove total rows
    select(-TOTAL_FOR_COMMODITY)  # Drop the total column
  
  assign(df_name, Top_c)
  
  # Now, let's add this back to the Excel that we saved earlier
  
  # Define file path & load workbook
  file_path <- paste0("Output/2. Top_", c, ".xlsx")
  wb <- loadWorkbook(file_path)
  
  # Remove "Shares" sheet if it already exists to update with new data
  if ("Shares" %in% names(wb)) {
    removeWorksheet(wb, "Shares")
  }
  addWorksheet(wb, "Shares")
  writeData(wb, "Shares", Top_c)
  
  # Save the updated workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
}

#############################################################
# 5. Creating stacked bar charts
#############################################################

# This loads the colours / design you want to have 
# Replace with own template if you have one
source()  

for (c in countries) {
  
  df_name <- paste0("Top_", c)
  Top_c <- get(df_name)  # Retrieve the actual data frame
  
  Top_c  <- Top_c  %>%
    mutate(YEAR_QUARTER = paste0(YEAR, "-", QUARTER))
  
  # Loop over unique HS codes
  for (HS_code in unique(Top_c $I_COMMODITY)) {
    
    # Filter data for the current HS code
    data_subset <- Top_c  %>% filter(I_COMMODITY == HS_code)
    
    # Sort countries by total share per quarter (ensuring largest at bottom)
    sorted_countries <- data_subset %>%
      group_by(CTY_NAME) %>%
      summarise(TOTAL_SHARE = sum(SHARE, na.rm = TRUE)) %>%
      arrange(TOTAL_SHARE) %>%
      pull(CTY_NAME)
    
    # Assign colors dynamically from wesp_colors, ensuring "ROW" is grey
    color_mapping <- setNames(wesp_colors[1:length(sorted_countries)], sorted_countries)
    color_mapping["ROW"] <- "grey"  # Set "ROW" to grey
    
    # Create the stacked bar chart
    p <- ggplot(data_subset, aes(x = YEAR_QUARTER, y = SHARE, 
                                 fill = factor(CTY_NAME, levels = sorted_countries))) +
      geom_bar(stat = "identity", position = "stack") +  # Stacked bars
      facet_wrap(~ I_COMMODITY_LDESC, scales = "free_y") +  # One plot per commodity description
      labs(
        title = "Main Trade Partners of the US",
        x = "",
        y = "Share of Total Trade",
        fill = ""
      ) +
      scale_fill_manual(values = color_mapping) +  # Apply pastel colors
      my_theme +  # Apply your custom theme
      theme(
        legend.position = "bottom",         # Move legend below
        legend.justification = "center",    # Center the legend
        legend.direction = "horizontal",    # Arrange legend items in a row
        legend.box = "horizontal"           # Ensures legend stays horizontal
      )
    
    # Save the plot
    ggsave(paste0("Output/2.", c, "_Top5_HS", HS_code, ".png"), plot = p, width = 8, height = 5)
  }
  
}