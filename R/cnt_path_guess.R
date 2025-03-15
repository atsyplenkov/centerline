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
        return(geos_to_terra(longest_path_geos))
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

    skeleton_sf <- sf::st_as_sf(skeleton_geos)
    # Convert skeleton to sfnetworks
    pol_network <-
      sfnetworks::as_sfnetwork(
        x = skeleton_sf,
        directed = FALSE,
        length_as_weight = TRUE,
        edges_as_lines = TRUE
      )
    # Convert sfnetworks to igraph
    # pol_graph <- igraph::as.igraph(pol_network)
    df_graph <- igraph::as_data_frame(pol_network)
    names(df_graph)[3] <- "geometry"
    df_graph <- df_graph[, c("weight", "geometry")]
    df_graph$weight <- as.numeric(df_graph$weight)

    # Find border points of skeleton
    closest_points <-
      find_closest_nodes(
        pol_network,
        find_outer_nodes(skeleton_geos)
      )

    # Find the most distant point from center
    # It will serve as the end point
    closest_end_points <-
      closest_points[which.min(
        igraph::closeness(pol_network, vid = closest_points)
      )]

    # Find paths
    paths <-
      base::suppressWarnings(
        sfnetworks::st_network_paths(
          pol_network,
          to = closest_points[closest_points != closest_end_points],
          from = closest_end_points,
          weights = "weight"
        )
      )

    # Paths lengths in counts
    paths_length <-
      base::vapply(
        paths$edge_paths,
        length,
        FUN.VALUE = integer(1),
        USE.NAMES = FALSE
      )

    # Filter non-zero paths
    paths_length_flag <- paths_length > 1
    paths_length_nonzero <- paths_length[paths_length_flag]
    edge_paths_nonzero <- paths$edge_paths[paths_length_flag]

    # Estimate paths lengths
    edge_paths_vec <- unlist(edge_paths_nonzero, use.names = FALSE)
    edge_paths_groups <-
      rep(
        seq_along(edge_paths_nonzero),
        times = paths_length_nonzero
      )
    edge_paths_length <- df_graph[edge_paths_vec, "weight"]

    # Sum paths lengths in meters
    true_paths_igraph <-
      tapply(edge_paths_length, edge_paths_groups, FUN = sum)

    # Return the longest path
    longest_path_igraph <- which.max(true_paths_igraph)
    longest_path_geos <-
      df_graph[edge_paths_nonzero[[longest_path_igraph]], "geometry"] |>
      geos::as_geos_geometry() |>
      geos::geos_make_collection() |>
      geos::geos_line_merge()

    longest_path_geos
  }
