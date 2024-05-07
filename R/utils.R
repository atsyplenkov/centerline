# SF to Terra transformer
terra_to_sf <-
  function(input) {
    spatial_data <-
      terra::as.data.frame(input)

    if (length(spatial_data) == 0) {
      terra::geom(input, wk = TRUE) |>
        wk::as_wkt() |>
        sf::st_as_sf() |>
        sf::st_set_crs(terra::crs(input))
    } else {
      terra::geom(input, wk = TRUE) |>
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

# Straight skeleton
# straight_skeleton <-
#   function(input) {
#     crs <- sf::st_crs(input)

#     # Return skeleton
#     sk <- raybevel::skeletonize(input)

#     # Keep only inner links
#     sk_links <-
#       subset(sk$links, !sk$links$edge)

#     # Create a data.frame of source nodes
#     source_nodes <-
#       sk$nodes[c("id", "x", "y")]
#     names(source_nodes) <- c("source", "start_x", "start_y")

#     # Create a data.frame of destination nodes
#     destination_nodes <-
#       sk$nodes[c("id", "x", "y")]
#     names(destination_nodes) <- c("destination", "end_x", "end_y")

#     # Build a linestring geometry
#     sk_new <-
#       merge(x = sk_links, y = source_nodes, by = "source", all.x = TRUE) |>
#       merge(y = destination_nodes, by = "destination", all.x = TRUE)

#     sk_new$geom <-
#       sprintf(
#         "LINESTRING(%s %s, %s %s)",
#         sk_new$start_x,
#         sk_new$start_y,
#         sk_new$end_x,
#         sk_new$end_y
#       )

#     # Transform to sf object
#     sk_sf <-
#       sf::st_as_sf(sk_new, wkt = "geom", crs = crs)

#     return(sk_sf)
#   }
