
#' Import & Transform sf
#'
#' @param path [character] path to sf file.
#'
#' @return 
#' @export
#'
#' @examples load_transform("data/road.shp")
#' 
load_transform <- function(path) {
  
  # Load & Transform shape files to WGS 84
  sf::st_read(path, quiet = TRUE) |>
    sf::st_transform(
      sf::st_crs(4326) # Define the target CRS as WGS 84 (EPSG 4326)
    )
  
}



#' Web outputs
#'
#' @param state_sf [data.frame] State shape file.
#' @param road_sf [data.frame] Road shape file.
#' @param state_capital_name [character] Single State or FCT name.
#'
#' @return
#' @export
#'
#' @examples create_outputs(state_sf, road_sf, "Ogun")
#' 
create_outputs <- function(state_sf = t_state_sf, road_sf = t_road_sf, state_capital_name) {
  
  # Clipped Road of selected state/FCT
  clipped_road <- state_sf |>
    dplyr::filter(.data[["statename"]] == state_capital_name) |>
    sf::st_intersection(x = road_sf, y = _)
  
  # Total Road Length
  total_length_kilometers <- sum(sf::st_length(clipped_road)) / 1000
  
  # Unique Road class
  road_class <- c("Trunk", "Primary", "Secondary")
  
  # Road length of each road class
  road_class_len <- lapply(road_class, \(cls) {
    
    km <- clipped_road |> 
      dplyr::filter(class == cls) |>
      sf::st_length() |>
      sum() / 1000
    
    # Format value
    scales::label_comma(0.01, suffix = "km")(as.numeric(km))
    
  })
  # Rename class length list
  names(road_class_len) <- road_class
  
  # Get state/FCT Zone 
  zone_abb <- unique(clipped_road$geozone)
  
  if (zone_abb == "SSZ") {
    zone <- "South South"
  } else if (zone_abb == "NCZ") {
    zone <- "North Central"
  } else if (zone_abb == "SWZ") {
    zone <- "South West"
  } else if (zone_abb == "NWZ") {
    zone <- "North West"
  } else if (zone_abb == "NEZ") {
    zone <- "North East"
  } else {
    zone <- "South East"
  }
  
  # Plot 
  fig_output <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = dplyr::filter(state_sf, .data[["statename"]] == state_capital_name),
      fill = "white", 
      color = "#050505", 
      size = 0.009
    ) +
    ggplot2::geom_sf(data = clipped_road, aes(color = class), linewidth = 0.15) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = 'none',
      axis.text = ggplot2::element_blank()
    )
  
  # Output
  output_list <- list(
    total_km = scales::label_comma(0.01, suffix = "km")(as.numeric(total_length_kilometers)),
    plot = fig_output,
    loc_zone = zone
  )
  
  append(output_list, road_class_len)
  
}

