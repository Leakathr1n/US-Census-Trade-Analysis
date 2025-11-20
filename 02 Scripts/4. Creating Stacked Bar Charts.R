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
    
    # Exclude ROW when sorting
    other_countries <- data_subset %>%
      filter(CTY_NAME != "ROW") %>%
      group_by(CTY_NAME) %>%
      summarise(TOTAL_SHARE = sum(SHARE, na.rm = TRUE), .groups = "drop") %>%
      arrange(TOTAL_SHARE) %>%  # smallest first → lowest on stack
      pull(CTY_NAME)
    
    sorted_countries <- c(other_countries, "ROW") # Combine ROW at the bottom
    data_subset$CTY_NAME <- factor(data_subset$CTY_NAME, levels = sorted_countries) # Apply factor levels
    
    legend_order <- c(sort(setdiff(unique(data_subset$CTY_NAME), "ROW")), "ROW")
    color_mapping_ordered <- color_mapping[legend_order]
    data_subset <- data_subset %>% mutate(Facet_Label = paste0("HS ", I_COMMODITY, ": ", I_COMMODITY_SDESC))  # Create a new column for facet labels
    
    
    # Plot
    p <- ggplot(data_subset, aes(x = YEAR_QUARTER, y = SHARE, 
                                 fill = factor(CTY_NAME, levels = sorted_countries))) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ Facet_Label, scales = "free_y") +   # use the new facet label
      labs(
        title = "Main Importers to the US",
        x = "",
        y = "Share of Total Imports in %",
        fill = ""
      ) +
      scale_fill_manual(values = color_mapping_ordered, breaks = legend_order) +
      theme(
        # Overall background
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90", size = 0.4, linetype = "dashed"),
        panel.grid.minor = element_line(),
        
        # Plot title
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        
        # Axis text and ticks
        axis.text.x = element_text(color = "black", hjust = 0.5),
        axis.text.y = element_text(color = "black"),
        axis.ticks = element_blank(), # remove the ticks
        axis.title.x = element_blank(),  # remove x-axis title
        axis.title.y = element_text(color = "black", face = "bold"),  # keep y-axis title
        
        # Legend
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        
        # Facets
        strip.background = element_blank(),   # remove box around facet
        strip.text = element_text(face = "bold", hjust = 0.5)
      )
    
    ggsave(paste0("03 Outputs/4. Stacked Bar Charts per HS Code/", c, "/", c,"_Top5_HS", HS_code, ".png"), plot = p, width = 8, height = 5) # Save the plot
  }
}