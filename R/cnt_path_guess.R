#' Guess polygon's centerline
#'
#' @description
#' This function, as follows from the title, tries to guess
#' the polygon centerline by connecting the most distant
#' points from each other. First, it finds the point most
#' distant from the polygon's centroid, then it searches
#' for a second point, which is most distant from the first.
#' The line connecting these two points will be the desired
#' centerline.
#'
#' @param input \code{sf}, \code{sfc} or \code{SpatVector} polygons object
#' @param skeleton \code{NULL} (default) or [centerline::cnt_skeleton()] output.
#' If \code{NULL} then polygon's skeleton would be estimated in the background
#' using specified parameters (see inherit params below). Forwarded
#' \code{anchors} from \code{...} apply only when a skeleton is built or
#' rebuilt; they are ignored when a valid \code{skeleton} is supplied.
#' @param return_geos \code{FALSE} (default). A logical flag that controls
#' whether the \code{geos_geometry} should be returned.
#'
#' @inheritDotParams cnt_skeleton
#'
#' @return An \code{sf}, \code{sfc} or \code{SpatVector} class
#' object of a \code{LINESTRING} geometry
#' @export
#'
#' @examples
#' library(sf)
#' library(geos)
#' lake <-
#'   sf::st_read(
#'     system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "lake",
#'     quiet = TRUE
#'   ) |>
#'   geos::as_geos_geometry()
#' # Find lake's centerline
#' lake_centerline <- cnt_path_guess(input = lake, keep = 1)
#' # Plot
#' plot(lake)
#' plot(lake_centerline, col = "firebrick", lwd = 2, add = TRUE)
#'
cnt_path_guess <- function(input, skeleton = NULL, return_geos = FALSE, ...) {
  UseMethod("cnt_path_guess")
}

#' @export
cnt_path_guess.geos_geometry <- function(input, skeleton = NULL, ...) {
  # Check if input is of geometry type 'POLYGON'
  stopifnot(check_polygons(input))
  input_geom_type <- get_geom_type(input)

  # Save CRS
  crs <- wk::wk_crs(input)

  # Find skeleton
  if (base::is.null(skeleton)) {
    skeleton_geos <- cnt_skeleton(input = input, ...)
  } else if (inherits(skeleton, "geos_geometry")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- skeleton
  } else if (inherits(skeleton, "SpatVector")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- terra_to_geos(skeleton)
  } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- geos::as_geos_geometry(skeleton)
  } else {
    warning("skeleton is not of supported class, rebuilding it...")
    skeleton_geos <- cnt_skeleton(input = input, ...)
  }

  longest_path_geos <- do.call(c, lapply(skeleton_geos, cnt_path_guess_master))

  # Set CRS
  wk::wk_crs(longest_path_geos) <- crs

  longest_path_geos
}

#' @export
cnt_path_guess.sf <- function(
  input,
  skeleton = NULL,
  return_geos = FALSE,
  ...
) {
  # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
  stopifnot(check_polygons(input))

  # Save CRS
  crs <- sf::st_crs(input)

  # Transform to geos_geometry
  input_geos <- geos::as_geos_geometry(input)
  input_geom_type <- geos::geos_type(input)

  # Find skeleton
  if (base::is.null(skeleton)) {
    skeleton_geos <- cnt_skeleton(input = input, ...) |>
      geos::as_geos_geometry()
  } else if (inherits(skeleton, "geos_geometry")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- skeleton
  } else if (inherits(skeleton, "SpatVector")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- terra_to_geos(skeleton)
  } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- geos::as_geos_geometry(skeleton)
  } else {
    warning("skeleton is not of supported class, rebuilding it...")
    skeleton_geos <- cnt_skeleton(input = input, ...) |>
      geos::as_geos_geometry()
  }

  # Find the longest path
  longest_path_geos <- do.call(c, lapply(skeleton_geos, cnt_path_guess_master))

  if (return_geos) {
    # Return the `geos_geometry` object
    return(longest_path_geos)
  } else {
    # Return the `sf` object
    longest_path_geos |>
      sf::st_as_sf() |>
      sf::st_set_crs(crs) |>
      cbind(sf::st_drop_geometry(input))
  }
}

#' @export
cnt_path_guess.sfc <- function(
  input,
  skeleton = NULL,
  return_geos = FALSE,
  ...
) {
  # Check if input is of class 'POLYGON'
  stopifnot(check_polygons(input))

  # Save CRS
  crs <- sf::st_crs(input)

  # Transform to geos_geometry
  input_geos <- geos::as_geos_geometry(input)
  input_geom_type <- geos::geos_type(input)

  # Find skeleton
  if (base::is.null(skeleton)) {
    skeleton_geos <- cnt_skeleton(input = input, ...) |>
      geos::as_geos_geometry()
  } else if (inherits(skeleton, "geos_geometry")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- skeleton
  } else if (inherits(skeleton, "SpatVector")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- terra_to_geos(skeleton)
  } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- geos::as_geos_geometry(skeleton)
  } else {
    warning("skeleton is not of supported class, rebuilding it...")
    skeleton_geos <- cnt_skeleton(input = input, ...) |>
      geos::as_geos_geometry()
  }

  # Find the longest path
  longest_path_geos <- do.call(c, lapply(skeleton_geos, cnt_path_guess_master))

  if (return_geos) {
    # Return the `geos_geometry` object
    return(longest_path_geos)
  } else {
    # Return the `sfc` object
    longest_path_geos |> sf::st_as_sfc() |> sf::st_set_crs(crs)
  }
}

#' @export
cnt_path_guess.SpatVector <- function(
  input,
  skeleton = NULL,
  return_geos = FALSE,
  ...
) {
  # Check if input is of class 'POLYGON'
  stopifnot(check_polygons(input))

  # Input attributes
  input_data <- terra::as.data.frame(input)

  # Save CRS
  crs <- terra::crs(input)

  # Transform to geos_geometry
  input_geos <- terra_to_geos(input)
  input_geom_type <- geos::geos_type(input_geos)

  # Find skeleton
  if (base::is.null(skeleton)) {
    skeleton_geos <- cnt_skeleton(input = input, ...) |> terra_to_geos()
  } else if (inherits(skeleton, "geos_geometry")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- skeleton
  } else if (inherits(skeleton, "SpatVector")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- terra_to_geos(skeleton)
  } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
    stopifnot(check_lines(skeleton))
    skeleton_geos <- geos::as_geos_geometry(skeleton)
  } else {
    warning("skeleton is not of supported class, rebuilding it...")
    skeleton_geos <- cnt_skeleton(input = input, ...) |> terra_to_geos()
  }

  # Find the longest path
  longest_path_geos <- do.call(c, lapply(skeleton_geos, cnt_path_guess_master))

  if (return_geos) {
    # Return the `geos_geometry` object
    return(longest_path_geos)
  } else {
    # Return the `SpatVector` object
    longest_path_geos <- geos_to_terra(longest_path_geos)

    if (nrow(input_data) == 0) {
      return(longest_path_geos)
    } else if (nrow(input_data) == nrow(longest_path_geos)) {
      longest_path_geos <- longest_path_geos |> cbind(input_data)
      return(longest_path_geos)
    } else if (nrow(input_data) == 1 && nrow(longest_path_geos) > 1) {
      longest_path_geos <- longest_path_geos |>
        cbind(input_data[rep(1, nrow(longest_path_geos)), ])
      return(longest_path_geos)
    } else {
      warning(
        "input and centerline have different number of rows,
        returning centerline without attributes"
      )
      return(longest_path_geos)
    }
  }
}

cnt_path_guess_master <- function(skeleton_geos) {
  g <- skeleton_graph_build(skeleton_geos, crs = wk::wk_crs(skeleton_geos))

  # find_outer_nodes expects unnested linework
  outer_lines <- skeleton_geos
  if (any(geos::geos_type(outer_lines) == "multilinestring")) {
    outer_lines <- geos::geos_unnest(outer_lines, keep_multi = FALSE)
  }

  # Find border points of skeleton
  terminal_ids <- find_closest_nodes(g, find_outer_nodes(outer_lines))

  # Find the most distant point from center (lowest closeness).
  # It will serve as the end point
  closest_end_points <- terminal_ids[which.min(igraph::closeness(
    g$graph,
    vids = terminal_ids,
    weights = g$edges$weight
  ))]

  remaining <- terminal_ids[terminal_ids != closest_end_points]
  edge_paths <- skeleton_graph_paths(
    g,
    from = closest_end_points,
    to = remaining
  )

  # Filter non-zero multi-edge paths
  paths_length <- lengths(edge_paths)
  paths_length_flag <- paths_length > 1L
  edge_paths_nonzero <- edge_paths[paths_length_flag]

  true_paths_igraph <- vapply(
    edge_paths_nonzero,
    function(epath) sum(g$edges$weight[epath]),
    numeric(1),
    USE.NAMES = FALSE
  )

  longest_path_igraph <- which.max(true_paths_igraph)
  skeleton_graph_path_line(g, edge_paths_nonzero[[longest_path_igraph]])
}
