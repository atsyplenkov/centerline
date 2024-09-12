# Inter-class transformers -----------------------------------------------
# Terra to SF transformer
# This function is five time faster than
# st::st_as_sf() due to {wk} package
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

# Terra to GEOS transformer
terra_to_geos <-
  function(input) {
    input |>
      terra::geom(wkt = TRUE) |>
      geos::as_geos_geometry(crs = sf::st_crs(input))
  }

# GEOS to terra transformer
geos_to_terra <-
  function(input) {
    wk_input <- wk::as_wkt(input)

    terra::vect(
      as.character(wk_input),
      crs = wk::wk_crs(wk_input)$wkt
    )
  }

# Class checks -----------------------------------------------------------
# GEOS polygon checks
check_geos_polygon <- function(input) {
  # Check if input is of class 'geos_geometry'
  if (!inherits(input, "geos_geometry")) {
    stop("Input is not of class 'geos_geometry'.")
  }

  # Check if geometry type is POLYGON
  geom_type <- geos::geos_type(input)
  if (!all(geom_type %in% c("polygon"))) {
    stop("Input does not contain 'polygon' geometries.")
  }

  # If checks pass
  return(TRUE)
}

# sf polygon checks
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

# terra polygon checks
check_terra_polygon <- function(input) {
  # Check if input is of class 'SpatVector'
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

# sf linestring checks
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

# GEOS linestring checks
check_geos_lines <- function(input) {
  # Check if input is of class 'geos_geometry'
  if (!inherits(input, "geos_geometry")) {
    stop("Input is not of class 'geos_geometry'.")
  }

  # Check if geometry type is LINESTRING
  geom_type <- geos::geos_type(input)
  if (!all(geom_type %in% c("linestring"))) {
    stop("Input does not contain 'LINESTRING' geometries.")
  }

  # If checks pass
  return(TRUE)
}

# terra linestring checks
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

# Polygon simplifications ------------------------------------------------
# Fast simplification, similiar to {mapshaper} ms_simplify
geos_ms_simplify <-
  function(geom,
           keep) {
    perimeter_length <-
      geos::geos_length(geom)

    point_count <-
      geom |>
      geos::geos_unique_points() |>
      geos::geos_num_coordinates()

    point_density <-
      perimeter_length / point_count

    geos::geos_simplify(
      geom,
      tolerance = point_density / (keep * 7)
    )
  }

# Fast densification, similar behavior to {mapshaper} ms_simplify
geos_ms_densify <-
  function(geom,
           keep) {
    perimeter_length <-
      geos::geos_length(geom)

    point_count <-
      geom |>
      geos::geos_unique_points() |>
      geos::geos_num_coordinates()

    point_density <-
      perimeter_length / point_count

    geos::geos_densify(
      geom,
      tolerance = point_density / (keep)
    )
  }

# Unique points ----------------------------------------------------------
# Return unique points from a geos geometry
unique_points <-
  function(geos_obj) {
    if (!inherits(geos_obj, "geos_geometry") &&
      !inherits(geos_obj, "SpatVector")) {
      geos_obj <- geos::as_geos_geometry(geos_obj)
    } else if (inherits(geos_obj, "SpatVector")) {
      geos_obj <- terra_to_geos(geos_obj)
    }

    geos_obj |>
      geos::geos_unique_points() |>
      geos::geos_unnest(keep_multi = FALSE) |>
      wk::as_wkt() |>
      base::unique() |>
      geos::as_geos_geometry(crs = wk::wk_crs(geos_obj))
  }

# Skeletons ---------------------------------------------------------------
# Skeleton to strtree
skeleton_to_strtree <-
  function(skeleton) {
    if (!inherits(skeleton, "geos_geometry")) {
      sk_crs <- sf::st_crs(skeleton)
      skeleton <- geos::as_geos_geometry(skeleton)
    } else {
      sk_crs <- wk::wk_crs(skeleton)
    }
    skeleton |>
      geos::geos_unique_points() |>
      geos::geos_unnest(keep_multi = FALSE) |>
      wk::as_wkt() |>
      base::unique() |>
      geos::as_geos_geometry(crs = sk_crs) |>
      geos::geos_strtree()
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
