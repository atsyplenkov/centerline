# SF to Terra transformer
terra_to_sf <-
  function(input){

    spatial_data <-
      terra::as.data.frame(input)

    if (length(spatial_data) == 0) {

      terra::geom(input, wk = T) |>
        wk::as_wkt() |>
        sf::st_as_sf() |>
        sf::st_set_crs(terra::crs(input))

    } else {

      terra::geom(input, wk = T) |>
        wk::as_wkt() |>
        sf::st_as_sf() |>
        sf::st_set_crs(terra::crs(input)) |>
        cbind(terra::as.data.frame(spatial_data))

    }

  }

# Polygon checks
check_sf_polygon <- function(input) {
  # Check if input is of class 'sf'
  if (!inherits(input, "sf") && !inherits(input, "sfc")) {
    stop("Input is not of class 'sf' or 'sfc'.")
  }

  # Check if geometry type is POLYGON
  geom_type <- sf::st_geometry_type(input, by_geometry = FALSE)
  if (!all(geom_type %in% c("POLYGON"))) {
    stop("Input does not contain 'POLYGON' geometries.")
  }

  # If checks pass
  return(TRUE)
}

check_terra_polygon <- function(input) {
  # Check if input is of class 'sf'
  if (!inherits(input, "SpatVector")) {
    stop("Input is not of class 'SpatVector'.")
  }

  # Check if geometry type is POLYGON
  geom_type <- terra::geomtype(input)
  if (!all(geom_type %in% c("polygons"))) {
    stop("Input does not contain 'polygons' geometries.")
  }

  # If checks pass
  return(TRUE)
}

# Linestring checks
check_sf_lines <- function(input) {
  # Check if input is of class 'sf'
  if (!inherits(input, "sf") && !inherits(input, "sfc")) {
    stop("Input is not of class 'sf' or 'sfc'.")
  }

  # Check if geometry type is POLYGON
  geom_type <- sf::st_geometry_type(input)
  if (!all(geom_type %in% c("LINESTRING"))) {
    stop("Input does not contain 'LINESTRING' geometries.")
  }

  # If checks pass
  return(TRUE)
}

check_terra_lines <- function(input) {
  # Check if input is of class 'sf'
  if (!inherits(input, "SpatVector")) {
    stop("Input is not of class 'SpatVector'.")
  }

  # Check if geometry type is POLYGON
  geom_type <- terra::geomtype(input)
  if (!all(geom_type %in% c("lines"))) {
    stop("Input does not contain 'lines' geometries.")
  }

  # If checks pass
  return(TRUE)
}
