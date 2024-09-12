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
#'   keep = 1.1
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
    cnt_path_guess_geos(
      input = input,
      skeleton = skeleton,
      ...
    )
  }

#' @export
cnt_path_guess.sf <-
  function(input,
           skeleton = NULL,
           ...) {
    cnt_path_guess_sf(
      input = input,
      skeleton = skeleton,
      ...
    )
  }

#' @export
cnt_path_guess.sfc <-
  function(input,
           skeleton = NULL,
           ...) {
    cnt_path_guess_sf(
      input = input,
      skeleton = skeleton,
      ...
    )
  }

#' @export
cnt_path_guess.SpatVector <-
  function(input,
           skeleton = NULL,
           ...) {
    cnt_path_guess_terra(
      input = input,
      skeleton = skeleton,
      ...
    )
  }

cnt_path_guess_geos <-
  function(input,
           skeleton = NULL,
           ...) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_geos_polygon(input))

    # Save CRS
    crs <- wk::wk_crs(input)

    # Transform to sf geometry
    input_sf <- sf::st_as_sf(input)

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton <- cnt_skeleton(input = input_sf, ...)
    } else if (inherits(skeleton, "geos_geometry")) {
      skeleton <- sf::st_as_sf(skeleton)
    } else if (inherits(skeleton, "SpatVector")) {
      skeleton <- terra_to_sf(skeleton)
    } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
      skeleton <- skeleton
    } else {
      warning("skeleton is not of supported class, rebuilding it...")
      skeleton <- cnt_skeleton(input = input_sf, ...)
    }

    # Convert skeleton to sfnetworks
    pol_network <-
      sfnetworks::as_sfnetwork(
        x = skeleton,
        directed = FALSE,
        length_as_weight = TRUE,
        edges_as_lines = TRUE
      )
    # Convert sfnetworks to igraph
    pol_graph <- igraph::as.igraph(pol_network)

    # Find border points of skeleton
    closest_points <- which(igraph::centr_betw(pol_graph)$res == 0)

    # Find the most distant point from center
    # It will serve as the end point
    closest_end_points <-
      closest_points[which.min(
        igraph::closeness(pol_graph, vid = closest_points)
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

    # Paths lengths
    paths_length <-
      base::vapply(
        paths$edge_paths,
        length,
        FUN.VALUE = numeric(1)
      )
    # Find path with the greatest amount of segments
    # I.e. it doesn't mean that it is the longest path
    length_quantile <-
      stats::quantile(paths_length, 0.95, na.rm = TRUE)

    # Transform greatest paths to geos objects
    edge_paths_filtered <- paths$edge_paths[paths_length >= length_quantile]
    df_paths <- igraph::as_data_frame(pol_graph)[, "geometry"]
    true_paths_geos <-
      edge_paths_filtered |>
      base::lapply(function(.x) {
        df_paths[.x, ] |>
          geos::as_geos_geometry()
      })

    # Find total lengths of an object
    paths_length_geos <-
      base::vapply(
        true_paths_geos,
        function(i) {
          sum(geos::geos_length(i))
        },
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

    # Return the longest path
    longest_path_geos <-
      true_paths_geos[[base::which.max(paths_length_geos)]] |>
      geos::geos_make_collection() |>
      geos::geos_line_merge()

    # Set CRS
    wk::wk_crs(longest_path_geos) <- crs

    return(longest_path_geos)
  }

cnt_path_guess_terra <-
  function(input,
           skeleton = NULL,
           ...) {
    # Check if input is of class 'SpatVector' and 'polygons'
    stopifnot(check_terra_polygon(input))

    # Save CRS
    crs <- terra::crs(input)

    # Transform to sf geometry
    input_sf <- terra_to_sf(input)

    # Transform to GEOS geometry
    input_geos <- geos::as_geos_geometry(input_sf)

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton <-
        cnt_skeleton(input = input_sf, ...)
    } else if (!inherits(skeleton, "SpatVector")) {
      skeleton <-
        terra_to_sf(skeleton)
    } else {
      skeleton <- skeleton
    }

    # Convert skeleton to sfnetworks
    pol_network <-
      sfnetworks::as_sfnetwork(
        x = skeleton,
        directed = FALSE,
        length_as_weight = TRUE,
        edges_as_lines = TRUE
      )
    # Convert sfnetworks to igraph
    pol_graph <- igraph::as.igraph(pol_network)

    # Find border points of skeleton
    closest_points <- which(igraph::centr_betw(pol_graph)$res == 0)

    # Find the most distant point from center
    # It will serve as the end point
    closest_end_points <-
      closest_points[which.min(
        igraph::closeness(pol_graph, vid = closest_points)
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

    # Paths lengths
    paths_length <-
      base::vapply(
        paths$edge_paths,
        length,
        FUN.VALUE = numeric(1)
      )
    # Find path with the greatest amount of segments
    # I.e. it doesn't mean that it is the longest path
    length_quantile <-
      stats::quantile(paths_length, 0.95, na.rm = TRUE)

    # Transform greatest paths to geos objects
    edge_paths_filtered <- paths$edge_paths[paths_length >= length_quantile]
    df_paths <- igraph::as_data_frame(pol_graph)[, "geometry"]
    true_paths_geos <-
      edge_paths_filtered |>
      base::lapply(function(.x) {
        df_paths[.x, ] |>
          geos::as_geos_geometry()
      })

    # Find total lengths of an object
    paths_length_geos <-
      base::vapply(
        true_paths_geos,
        function(i) {
          sum(geos::geos_length(i))
        },
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

    # Return the longest path
    true_paths_geos[[base::which.max(paths_length_geos)]] |>
      geos::geos_make_collection() |>
      geos::geos_line_merge() |>
      wk::as_wkt() |>
      base::as.character() |>
      terra::vect(crs = crs)
  }

cnt_path_guess_sf <-
  function(input,
           skeleton = NULL,
           ...) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_sf_polygon(input))

    # Save CRS
    crs <- sf::st_crs(input)

    # Transform to GEOS geometry
    input_geos <-
      input |>
      geos::as_geos_geometry()

    # Find skeleton
    if (base::is.null(skeleton)) {
      skeleton <-
        cnt_skeleton(input = input, ...)
    } else if (inherits(skeleton, "SpatVector")) {
      skeleton <-
        terra_to_sf(skeleton)
    } else if (inherits(skeleton, "sf") || inherits(skeleton, "sfc")) {
      skeleton <- skeleton
    } else {
      warning("skeleton is not of supported class, rebuilding it...")
      skeleton <-
        cnt_skeleton(input = input, ...)
    }

    # Convert skeleton to sfnetworks
    pol_network <-
      sfnetworks::as_sfnetwork(
        x = skeleton,
        directed = FALSE,
        length_as_weight = TRUE,
        edges_as_lines = TRUE
      )
    # Convert sfnetworks to igraph
    pol_graph <- igraph::as.igraph(pol_network)

    # Find border points of skeleton
    closest_points <- which(igraph::centr_betw(pol_graph)$res == 0)

    # Find the most distant point from center
    # It will serve as the end point
    closest_end_points <-
      closest_points[which.min(
        igraph::closeness(pol_graph, vid = closest_points)
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

    # Paths lengths
    paths_length <-
      base::vapply(
        paths$edge_paths,
        length,
        FUN.VALUE = numeric(1)
      )
    # Find path with the greatest amount of segments
    # I.e. it doesn't mean that it is the longest path
    length_quantile <-
      stats::quantile(paths_length, 0.95, na.rm = TRUE)

    # Transform greatest paths to geos objects
    edge_paths_filtered <- paths$edge_paths[paths_length >= length_quantile]
    df_paths <- igraph::as_data_frame(pol_graph)[, "geometry"]
    true_paths_geos <-
      edge_paths_filtered |>
      base::lapply(function(.x) {
        df_paths[.x, ] |>
          geos::as_geos_geometry()
      })

    # Find total lengths of an object
    paths_length_geos <-
      base::vapply(
        true_paths_geos,
        function(i) {
          sum(geos::geos_length(i))
        },
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

    # Return the longest path
    true_paths_geos[[base::which.max(paths_length_geos)]] |>
      geos::geos_make_collection() |>
      geos::geos_line_merge() |>
      sf::st_as_sf() |>
      sf::st_set_crs(crs)
  }
