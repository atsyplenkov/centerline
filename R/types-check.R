# Check is package installed
check_package <-
  function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(paste(package, "is required but not installed."))
    }
  }

# Check that all objects share the same class
check_same_class <-
  function(obj1, obj2, obj3) {
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
check_polygons <-
  function(input) {
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
check_lines <-
  function(input) {
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
check_points <-
  function(input) {
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
