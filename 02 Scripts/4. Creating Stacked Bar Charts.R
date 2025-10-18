########################################################################
# Title: 2. Creating Top 5 
# Author: Lea Roeller
# Date: 28/09/2025
#######################################################################

## 0. Housekeeping ----
rm(list = ls())
source("02 Scripts/0. Useful functions.R")

## 1. Importing the data ---- 

countries <- c("CANADA", "MEXICO") # check that countries have already been exported before

for (c in countries) {
  file_path <- file.path("01 RDS/Country Top 5", paste0("Top_", c, ".rds")) # setting the file path
  
  if (file.exists(file_path)) { 
    assign(paste0("Top_", c), readRDS(file_path), envir = .GlobalEnv)
    message("Sucessfully loaded ", c) #successfully loaded!
  } else {
    warning("File for ", c, " not found: Please re-run scripts 2-3 with missing countries") 
  }
}

## 2. Importing the ggplot theme ---- 

# source()  # UN internal

## 3. Creating Country Output Subfolder for overview ----

for (c in countries) {
  folder_path <- file.path("03 Outputs/4. Stacked Bar Charts per HS Code/", c) # Define the folder path for the current country
  if (!dir.exists(folder_path)) { # Check if the folder exists; if not, create it
    dir.create(folder_path, recursive = TRUE)
  }
}

## 4. Actually creating the stacked bar charts ----

for (c in countries) {
  
  df_name <- paste0("Top_", c)
  Top_c <- get(df_name)  # Retrieve the actual data frame
  Top_c  <- Top_c  %>% mutate(YEAR_QUARTER = paste0(YEAR, "-", QUARTER)) #obtain a single column for YEAR-Q combination
  
  all_countries <- unique(Top_c$CTY_NAME)
  color_mapping <- setNames(graphic_colours[1:length(all_countries)], all_countries)  # Assign colors dynamically from, ensuring "ROW" is grey
  color_mapping["ROW"] <- "darkgrey"  # assign a fixed color to "ROW"
  
  # Loop over unique HS codes
  for (HS_code in unique(Top_c$I_COMMODITY)) {
    data_subset <- Top_c  %>% filter(I_COMMODITY == HS_code) # Filter data for the current HS code
    
    # Sort countries by total share per quarter (ensuring largest at bottom)
    sorted_countries <- data_subset %>%
      group_by(CTY_NAME) %>%
      summarise(TOTAL_SHARE = sum(SHARE, na.rm = TRUE)) %>%
      arrange(TOTAL_SHARE) %>%
      pull(CTY_NAME)
    
    # Create the stacked bar chart
    p <- ggplot(data_subset, aes(x = YEAR_QUARTER, y = SHARE, 
                                 fill = factor(CTY_NAME, levels = names(color_mapping)))) +
      geom_bar(stat = "identity", position = "stack") +  # Stacked bars
      facet_wrap(~ I_COMMODITY_SDESC, scales = "free_y") +  # One plot per commodity description
      labs(
        title = "Main Trade Partners of the US",
        x = "",
        y = "Share of Total Trade",
        fill = ""
      ) +
      scale_fill_manual(values = color_mapping) +
      # my_theme +  # Apply your custom theme
      theme(
        legend.position = "bottom",         # Move legend below
        legend.justification = "center",    # Center the legend
        legend.direction = "horizontal",    # Arrange legend items in a row
        legend.box = "horizontal"           # Ensures legend stays horizontal
      )

    ggsave(paste0("03 Outputs/4. Stacked Bar Charts per HS Code/", c, "/", c,"_Top5_HS", HS_code, ".png"), plot = p, width = 8, height = 5) # Save the plot
  }
}