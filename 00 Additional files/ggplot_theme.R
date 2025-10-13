# Templates for WESP 2025

library(ggplot2)
# WESP ggplot theme
my_theme <- theme(
  text = element_text(size = 8.5, color = "black"), 
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "#FAFAFA", color = NA),
  panel.grid.major = element_line(color = "#FAFAFA"),
  panel.grid.minor = element_line(color = "#FAFAFA"),
  axis.text = element_text(color = "#595959"),
  axis.title = element_text(color = "#595959"),
  legend.position = c(0.9, 0.9),
  legend.justification = c("right", "top"),
  legend.direction = "horizontal", 
  legend.background = element_rect(fill = "#FAFAFA", color = NA)
)

# WESP color palette (currently set to WESP 2025 colors)
wesp_colors <- c(
  "#009EDB", # Accent 1
  "#A05FB4", # Accent 2
  "#C5DFEF", # Accent 3
  "#D5B4D6", # Accent 4
  "#004987", # Accent 5
  "#5B2C86"  # Accent 6
)
