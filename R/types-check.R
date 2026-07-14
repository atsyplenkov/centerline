# Check is package installed
check_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(paste(package, "is required but not installed."))
  }
}

# Check that all objects share the same class
check_same_class <- function(obj1, obj2, obj3) {
  class1 <- class(obj1)
  class2 <- class(obj2)
  class3 <- class(obj3)

  class_check <- base::identical(class1, class2) &&
    base::identical(class1, class3)

  if (!class_check) {
    stop("All objects must share the same class.")
  }
}

# Get geometry type of the spatial object
get_geom_type <- function(input) {
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
    !all(
      geom_type %in%
        c("POLYGON", "polygons", "polygon", "multipolygon", "MULTIPOLYGON")
    )
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
    !all(
      geom_type %in%
        c(
          "LINESTRING",
          "lines",
          "linestring",
          "multilinestring",
          "MULTILINESTRING"
        )
    )
  ) {
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

# Validate optional boundary anchors for cnt_skeleton()
check_anchors <- function(input, anchors) {
  if (is.null(anchors)) {
    return(TRUE)
  }

  n_anchors <- if (inherits(anchors, "SpatVector") || inherits(anchors, "sf")) {
    nrow(anchors)
  } else {
    length(anchors)
  }
  if (n_anchors == 0L) {
    return(TRUE)
  }

  input_family <- spatial_container_family(input)
  anchors_family <- spatial_container_family(anchors)
  if (
    is.na(input_family) ||
      is.na(anchors_family) ||
      input_family != anchors_family
  ) {
    stop("anchors must use the same spatial class as input")
  }

  tryCatch(stopifnot(check_points(anchors)), error = function(e) {
    stop("anchors must contain non-empty POINT geometries")
  })

  if (input_family == "geos_geometry") {
    if (!wk::wk_crs_equal(wk::wk_crs(input), wk::wk_crs(anchors))) {
      stop("anchors and input must use the same CRS")
    }
    anchors_geos <- anchors
  } else if (input_family == "sf" || input_family == "sfc") {
    if (sf::st_crs(input) != sf::st_crs(anchors)) {
      stop("anchors and input must use the same CRS")
    }
    anchors_geos <- geos::as_geos_geometry(anchors)
  } else {
    if (!terra::same.crs(input, anchors)) {
      stop("anchors and input must use the same CRS")
    }
    anchors_geos <- terra_to_geos(anchors)
  }

  if (anyNA(anchors_geos) || any(geos::geos_is_empty(anchors_geos))) {
    stop("anchors must contain non-empty POINT geometries")
  }

  equals_hits <- geos::geos_equals_matrix(
    anchors_geos,
    geos::geos_strtree(anchors_geos)
  )
  has_duplicate <- any(vapply(
    seq_along(equals_hits),
    function(i) {
      any(equals_hits[[i]] != i)
    },
    logical(1),
    USE.NAMES = FALSE
  ))
  if (has_duplicate) {
    stop("anchors must not contain duplicate points")
  }

  TRUE
}

spatial_container_family <- function(x) {
  if (inherits(x, "SpatVector")) {
    "SpatVector"
  } else if (inherits(x, "sf")) {
    "sf"
  } else if (inherits(x, "sfc")) {
    "sfc"
  } else if (inherits(x, "geos_geometry")) {
    "geos_geometry"
  } else {
    NA_character_
  }
}
