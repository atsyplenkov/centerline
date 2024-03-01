check_sf_polygon <- function(input) {
  # Check if input is of class 'sf'
  if (!inherits(input, "sf") && !inherits(input, "sfc")) {
    stop("Input is not of class 'sf' or 'sfc'.")
  }

  # Check if geometry type is POLYGON or MULTIPOLYGON
  geom_type <- sf::st_geometry_type(input, by_geometry = FALSE)
  if (!all(geom_type %in% c("POLYGON"))) {
    stop("Input does not contain 'POLYGON' geometries.")
  }

  # If checks pass
  return(TRUE)
}
