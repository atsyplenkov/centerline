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
#'
#' @inheritDotParams cnt_skeleton
#'
#' @return An \code{sf}, \code{sfc} or \code{SpatVector} class
#' object of a \code{LINESTRING} geometry
#' @export
#'
#' @examples
#' library(terra)
#'
#' lake <-
#'   terra::vect(
#'     system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "lake"
#'   )
#'
#' plot(lake)
#'
#' cnt_path_guess(
#'   input = lake,
#'   keep = 1
#' ) |>
#'   plot(
#'     col = "firebrick",
#'     lwd = 2,
#'     add = TRUE
#'   )
#'
cnt_path_guess <-
  function(input,
           skeleton = NULL,
           ...) {
    UseMethod("cnt_path_guess")
  }

#' @export
cnt_path_guess.geos_geometry <-
  function(input,
           skeleton = NULL,
           ...) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_polygons(input))

    # Save CRS
    crs <- wk::wk_crs(input)

    # Transform to sf geometry
    input_sf <- sf::st_as_sf(input)

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton_sf <- cnt_skeleton(input = input_sf, ...)
    } else if (inherits(skeleton, "geos_geometry")) {
      skeleton_sf <- sf::st_as_sf(skeleton)
    } else if (inherits(skeleton, "SpatVector")) {
      skeleton_sf <- sf::st_as_sf(skeleton)
    } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
      skeleton_sf <- skeleton
    } else {
      warning("skeleton is not of supported class, rebuilding it...")
      skeleton_sf <- cnt_skeleton(input = input_sf, ...)
    }

    longest_path_geos <-
      cnt_path_guess_master(skeleton_sf)

    # Set CRS
    wk::wk_crs(longest_path_geos) <- crs

    longest_path_geos
  }

#' @export
cnt_path_guess.sf <-
  function(input,
           skeleton = NULL,
           ...) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_polygons(input))

    # Save CRS
    crs <- sf::st_crs(input)

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton_sf <-
        cnt_skeleton(input = input, ...)
    } else if (inherits(skeleton, "geos_geometry")) {
      skeleton_sf <-
        sf::st_as_sf(skeleton)
    } else if (inherits(skeleton, "SpatVector")) {
      skeleton_sf <-
        sf::st_as_sf(skeleton)
    } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
      skeleton_sf <- skeleton
    } else {
      warning("skeleton is not of supported class, rebuilding it...")
      skeleton_sf <-
        cnt_skeleton(input = input, ...)
    }

    # Find the longest path
    cnt_path_guess_master(skeleton_sf) |>
      sf::st_as_sf() |>
      sf::st_set_crs(crs)
  }

#' @export
cnt_path_guess.sfc <-
  function(input,
           skeleton = NULL,
           ...) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_polygons(input))

    # Save CRS
    crs <- sf::st_crs(input)

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton_sf <-
        cnt_skeleton(input = input, ...)
    } else if (
      inherits(skeleton, "SpatVector") || inherits(skeleton, "geos_geometry")
    ) {
      skeleton_sf <-
        sf::st_as_sf(skeleton)
    } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
      skeleton_sf <- skeleton
    } else {
      warning("skeleton is not of supported class, rebuilding it...")
      skeleton_sf <-
        cnt_skeleton(input = input, ...)
    }

    # Find the longest path
    cnt_path_guess_master(skeleton_sf) |>
      sf::st_as_sfc() |>
      sf::st_set_crs(crs)
  }

#' @export
cnt_path_guess.SpatVector <-
  function(input,
           skeleton = NULL,
           ...) {
    # Check if input is of class 'SpatVector' and 'polygons'
    stopifnot(check_polygons(input))

    # Save CRS
    crs <- terra::crs(input)

    # Transform to sf geometry
    input_sf <- sf::st_as_sf(input)

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton_sf <-
        cnt_skeleton(input = input_sf, ...)
    } else if (
      inherits(skeleton, "SpatVector") || inherits(skeleton, "geos_geometry")
    ) {
      skeleton_sf <-
        sf::st_as_sf(skeleton)
    } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
      skeleton_sf <- skeleton
    } else {
      warning("skeleton is not of supported class, rebuilding it...")
      skeleton_sf <-
        cnt_skeleton(input = input_sf, ...)
    }
    # Find the longest path
    cnt_path_guess_master(skeleton_sf) |>
      wk::as_wkt() |>
      base::as.character() |>
      terra::vect(crs = crs)
  }

cnt_path_guess_master <-
  function(skeleton_sf) {
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
        find_outer_nodes(geos::as_geos_geometry(skeleton_sf))
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
