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
#' using specified parameters (see inherit params below).
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
cnt_path_guess <-
  function(input, skeleton = NULL, return_geos = FALSE, ...) {
    UseMethod("cnt_path_guess")
  }

#' @export
cnt_path_guess.geos_geometry <-
  function(input, skeleton = NULL, ...) {
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

    longest_path_geos <-
      do.call(c, lapply(skeleton_geos, cnt_path_guess_master))

    # Set CRS
    wk::wk_crs(longest_path_geos) <- crs

    longest_path_geos
  }

#' @export
cnt_path_guess.sf <-
  function(input, skeleton = NULL, return_geos = FALSE, ...) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_polygons(input))

    # Save CRS
    crs <- sf::st_crs(input)

    # Transform to geos_geometry
    input_geos <- geos::as_geos_geometry(input)
    input_geom_type <- geos::geos_type(input)

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton_geos <- cnt_skeleton(input = input_geos, ...)
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
      skeleton_geos <- cnt_skeleton(input = input_geos, ...)
    }

    # Find the longest path
    longest_path_geos <-
      do.call(c, lapply(skeleton_geos, cnt_path_guess_master))

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
cnt_path_guess.sfc <-
  function(input, skeleton = NULL, return_geos = FALSE, ...) {
    # Check if input is of class 'POLYGON'
    stopifnot(check_polygons(input))

    # Save CRS
    crs <- sf::st_crs(input)

    # Transform to geos_geometry
    input_geos <- geos::as_geos_geometry(input)
    input_geom_type <- geos::geos_type(input)

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton_geos <- cnt_skeleton(input = input_geos, ...)
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
      skeleton_geos <- cnt_skeleton(input = input_geos, ...)
    }

    # Find the longest path
    longest_path_geos <-
      do.call(c, lapply(skeleton_geos, cnt_path_guess_master))

    if (return_geos) {
      # Return the `geos_geometry` object
      return(longest_path_geos)
    } else {
      # Return the `sfc` object
      longest_path_geos |>
        sf::st_as_sfc() |>
        sf::st_set_crs(crs)
    }
  }

#' @export
cnt_path_guess.SpatVector <-
  function(input, skeleton = NULL, return_geos = FALSE, ...) {
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
      skeleton_geos <- cnt_skeleton(input = input_geos, ...)
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
      skeleton_geos <- cnt_skeleton(input = input_geos, ...)
    }

    # Find the longest path
    longest_path_geos <-
      do.call(c, lapply(skeleton_geos, cnt_path_guess_master))

    if (return_geos) {
      # Return the `geos_geometry` object
      return(longest_path_geos)
    } else {
      # Return the `SpatVector` object
      longest_path_geos <- geos_to_terra(longest_path_geos)

      if (nrow(input_data) == 0) {
        return(longest_path_geos)
      } else if (nrow(input_data) == nrow(longest_path_geos)) {
        longest_path_geos <-
          longest_path_geos |>
          cbind(input_data)
        return(longest_path_geos)
      } else if (nrow(input_data) == 1 && nrow(longest_path_geos) > 1) {
        longest_path_geos <-
          longest_path_geos |>
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

cnt_path_guess_master <-
  function(skeleton_geos) {
    if (geos::geos_type(skeleton_geos) == "multilinestring") {
      skeleton_geos <-
        geos::geos_unnest(skeleton_geos, keep_multi = FALSE)
    }

    # Build graph from geos linestrings
    graph <- build_graph_geos(skeleton_geos)

    # Find border (outer) points and map them to graph node IDs
    outer_points <- find_outer_nodes(skeleton_geos)
    outer_nodes  <- find_closest_nodes(graph$node_points, outer_points)
    outer_nodes  <- unique(outer_nodes)

    if (length(outer_nodes) <= 1) {
      return(
        skeleton_geos |>
          geos::geos_make_collection() |>
          geos::geos_line_merge()
      )
    }

    # Find the most peripheral outer node using a closeness-like measure:
    # the node with the largest sum of distances to all other nodes.
    dist_sums <- vapply(outer_nodes, function(on) {
      res <- dijkstra(graph, from = on)
      sum(res$dist[is.finite(res$dist)])
    }, FUN.VALUE = numeric(1))

    closest_end_points <- outer_nodes[which.max(dist_sums)]

    # Run one all-target Dijkstra from the peripheral node
    other_nodes <- outer_nodes[outer_nodes != closest_end_points]
    all_dist <- dijkstra(graph, from = closest_end_points)

    other_dists <- all_dist$dist[other_nodes]
    valid <- is.finite(other_dists)

    if (!any(valid)) {
      return(
        skeleton_geos |>
          geos::geos_make_collection() |>
          geos::geos_line_merge()
      )
    }

    target_node <- other_nodes[which.max(other_dists)]

    # Reconstruct shortest path to the farthest outer node
    path <- dijkstra(graph, from = closest_end_points, to = target_node)

    if (is.null(path)) {
      return(
        skeleton_geos |>
          geos::geos_make_collection() |>
          geos::geos_line_merge()
      )
    }

    longest_path_geos <-
      graph$geometry[path$edges] |>
      geos::geos_make_collection() |>
      geos::geos_line_merge()

    longest_path_geos
  }
