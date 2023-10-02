# Load necessary libraries
library(sf)           # Load the sf library for working with spatial data
library(ggplot2)      # Load ggplot2 for creating plots
library(patchwork)    # Load patchwork for combining plots
library(extrafont)    # Load extrafont for font support
library(tidyverse)    # Load tidyverse for data manipulation
library(Cairo)        # Load Cairo for high-quality graphics

# Load the state and road shapefiles
state_sf <- st_read("state.shp")  # Read the state shapefile
road_sf <- st_read("road.shp")    # Read the road shapefile

# Define the target CRS as WGS 84 (EPSG 4326)
target_crs <- st_crs(4326)

# Transform both shapefiles to WGS 84
state_sf <- st_transform(state_sf, target_crs)  # Transform state data
road_sf <- st_transform(road_sf, target_crs)    # Transform road data

# Create a list of clipped road geometries for each state
clipped_roads <- list()

# Iterate through each state to clip roads
for (i in 1:length(state_sf$statename)) {
  state_name <- state_sf$statename[i]
  state_geom <- state_sf[i, ]
  
  # Clip roads to the boundary of the current state
  clipped_road <- st_intersection(road_sf, state_geom)
  
  # Store the clipped roads in the list
  clipped_roads[[state_name]] <- clipped_road
}

# Create a directory to save the maps
dir.create("output_maps", showWarnings = FALSE)

# Define a color palette for the road classes
class_colors <- c("Trunk" = "#df7a6f", "Primary" = "#e3cb7e", "Secondary" = "#8c82ec")

# Set road class as a factor with specified order
road_sf$class <- factor(road_sf$class, levels = c("Trunk", "Primary", "Secondary"))

# Create a list to store all the plots
map_plots <- list()

# Create maps for each state and add to the list of plots
for (state_name in names(clipped_roads)) {
  # Extract the clipped roads for the current state
  clipped_road <- clipped_roads[[state_name]]
  
  # Create a plot using ggplot2
  plot <- ggplot() +
    geom_sf(data = state_sf[state_sf$statename == state_name, ], fill = "white", color = "grey29", size = 0.009) +
    geom_sf(data = clipped_road, aes(color = class), linewidth = 0.15) +
    scale_color_manual(values = class_colors, guide = guide_legend(nrow = 1)) +  # Set the color scale
    labs(subtitle = state_name) + 
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      text = element_text(family = 'Tw Cen MT'),
      legend.position = 'none',
      axis.text = element_blank(),
      plot.subtitle = element_text(hjust = 0.5),
    )
  
  map_plots[[state_name]] <- plot
}

# Create a list of ggplot objects for each state
plot_list <- list()

# Store the plots in the list
for (state_name in names(map_plots)) {
  plot_list[[state_name]] <- map_plots[[state_name]]
}

# Sort named list
sorted_li <- plot_list[order(names(plot_list))]

# Combine all the plots into a single layout
final_plot <- plot_grid(plotlist = sorted_li, ncol = 5)  # Adjust ncol as needed

# Save the combined plot as an image file (e.g., PNG)
ggsave("output_maps/final_map.png", final_plot, type = "cairo", width = 10, height = 11, bg = "white")
