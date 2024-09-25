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
      sf::st_as_sf() |>
      geos::as_geos_geometry()
    # input |>
    #   terra::geom(wkt = TRUE) |>
    #   geos::as_geos_geometry(crs = sf::st_crs(input))
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
# Check that all objects share the same class
check_same_class <- function(obj1, obj2, obj3) {
  class1 <- class(obj1)
  class2 <- class(obj2)
  class3 <- class(obj3)

  class_check <-
    base::identical(class1, class2) &&
      base::identical(class1, class3)

  if (!class_check) {
    stop("All objects must share the same class.")
  }
}

# Get geometry type of the spatial object
get_geom_type <-
  function(input) {
    if (inherits(input, "sf") || inherits(input, "sfc")) {
      sf::st_geometry_type(input, by_geometry = TRUE)
    } else if (inherits(input, "SpatVector")) {
      terra::geomtype(input)
    } else if (inherits(input, "geos_geometry")) {
      geos::geos_type(input)
    }
  }

# Checks for polygon geometries
check_polygons <- function(input) {
  # Check if input is of class 'sf', 'sfc', 'SpatVector', or 'geos_geometry'
  if (!inherits(input, c("sf", "sfc", "SpatVector", "geos_geometry"))) {
    stop(
      "Input is not of
      class 'sf', 'sfc', 'SpatVector', or 'geos_geometry'."
    )
  }

  # Check if geometry type is POLYGON
  geom_type <- get_geom_type(input)
  if (
    !all(geom_type %in%
      c("POLYGON", "polygons", "polygon", "multipolygon", "MULTIPOLYGON"))
  ) {
    stop("Input does not contain 'POLYGON' or 'MULTIPOLYGON' geometries.")
  }

  # If checks pass
  return(TRUE)
}

# Checks for linestring geometries
check_lines <- function(input) {
  # Check if input is of class 'sf', 'sfc', 'SpatVector', or 'geos_geometry'
  if (!inherits(input, c("sf", "sfc", "SpatVector", "geos_geometry"))) {
    stop(
      "Input skeleton is not of
      class 'sf', 'sfc', 'SpatVector', or 'geos_geometry'."
    )
  }

  # Check if geometry type is LINESTRING
  geom_type <- get_geom_type(input)
  if (
    !all(geom_type %in%
      c(
        "LINESTRING", "lines", "linestring",
        "multilinestring", "MULTILINESTRING"
      ))) {
    stop("Input skeleton does not contain 'LINESTRING' geometry.")
  }

  # If checks pass
  return(TRUE)
}

# Checks for points geometries
check_points <- function(input) {
  # Check if input is of class 'sf', 'sfc',
  # 'SpatVector', or 'geos_geometry'
  if (!inherits(input, c("sf", "sfc", "SpatVector", "geos_geometry"))) {
    stop(
      "Input point is not of
      class 'sf', 'sfc', 'SpatVector', or 'geos_geometry'."
    )
  }

  # Check if geometry type is POINT
  geom_type <- get_geom_type(input)
  if (!all(geom_type %in% c("POINT", "points", "point"))) {
    stop("Input point does not contain 'POINT' geometry.")
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
      geos::geos_num_coordinates()

    point_density <-
      perimeter_length / point_count

    geos::geos_densify(
      geom,
      tolerance = point_density / (keep)
    )
  }

# Reverse lines if needed ------------------------------------------------
# Check if we need to reverse the lines
reverse_lines_if_needed <-
  function(lines_list_geos, end_point) {
    start_centerline <- geos::geos_point_start(lines_list_geos[[1]])
    end_centerline <- geos::geos_point_end(lines_list_geos[[1]])
    end_geos <- geos::as_geos_geometry(end_point)

    start_tail <- geos::geos_distance(end_geos, start_centerline)
    end_tail <- geos::geos_distance(end_geos, end_centerline)

    if (start_tail < end_tail) {
      lines_list_geos |>
        lapply(geos::geos_reverse) |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge)
    } else {
      lines_list_geos |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge)
    }
  }

# Outer nodes of the skeleton --------------------------------------------
# Faster alternative to igraph::centr_betw()
find_outer_nodes <-
  function(skeleton_geos) {
    all_index <- geos::geos_strtree(skeleton_geos)

    start_points <- geos::geos_point_start(skeleton_geos)
    end_points <- geos::geos_point_end(skeleton_geos)

    start_intersects <- geos::geos_intersects_matrix(start_points, all_index)
    end_intersects <- geos::geos_intersects_matrix(end_points, all_index)

    lonely_start <- start_points[
      vapply(
        start_intersects,
        FUN = function(x) length(x) == 1,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
      )
    ]
    lonely_end <- end_points[
      vapply(
        end_intersects,
        FUN = function(x) length(x) == 1,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
      )
    ]

    c(lonely_end, lonely_start)
  }

find_closest_nodes <-
  function(sf_graph, nodes_geos) {
    geos_graph <-
      sfnetworks::activate(sf_graph, "nodes") |>
      sf::st_as_sf() |>
      geos::as_geos_geometry() |>
      geos::geos_strtree()

    geos::geos_nearest(nodes_geos, geos_graph)
  }
