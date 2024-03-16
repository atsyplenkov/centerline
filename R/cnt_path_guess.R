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
#'
cnt_path_guess <-
  function(input,
           skeleton = NULL,
           ...) {
    UseMethod("cnt_path_guess")
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
    cnt_path_guess_sfc(
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

cnt_path_guess_terra <-
  function(
      input,
      skeleton = NULL,
      ...) {
    # Check if input is of class 'SpatVector' and 'polygons'
    stopifnot(check_terra_polygon(input))

    # Save CRS
    crs <- terra::crs(input)

    # Transform to GEOS geometry
    input_geos <-
      input |>
      terra::geom(wkt = TRUE) |>
      geos::as_geos_geometry()

    input <-
      terra_to_sf(input)

    if (base::is.null(skeleton)) {
      skeleton <-
        cnt_skeleton(input = input, ...)
    } else {
      skeleton <-
        terra_to_sf(skeleton)
    }

    # Convert skeleton to sfnetworks
    pol_network <-
      skeleton |>
      sfnetworks::as_sfnetwork(directed = FALSE)

    # Find the most distant nodes from the polygon center
    end_node <-
      pol_network |>
      sfnetworks::activate("nodes") |>
      dplyr::mutate(cc = tidygraph::centrality_closeness()) |>
      dplyr::filter(cc == base::min(cc)) |>
      # Keep only one point
      sf::st_as_sf() |>
      utils::head(n = 1)

    # Find main points of the polygon
    # main_points <-
    #   input |>
    #   rmapshaper::ms_simplify(
    #     keep = 0.01,
    #     method = "dp",
    #     keep_shapes = T
    #   ) |>
    #   sf::st_cast("POINT")

    perimeter_length <-
      geos::geos_length(input_geos)

    point_count <-
      input_geos |>
      geos::geos_unique_points() |>
      geos::geos_num_coordinates()

    point_density <-
      perimeter_length / point_count

    main_points <-
      geos::geos_simplify_preserve_topology(
        input_geos,
        tolerance = point_density / 0.1
      ) |>
      geos::geos_unique_points() |>
      sf::st_as_sf() |>
      sf::st_cast("POINT")

    # Find closest nodes to the above found points
    closest_points <-
      main_points |>
      sf::st_nearest_feature(pol_network, check_crs = FALSE)

    closest_end_points <-
      end_node |>
      sf::st_nearest_feature(pol_network, check_crs = FALSE)

    # Estimate distance between key points and end point
    net <-
      pol_network |>
      sfnetworks::activate("edges") |>
      dplyr::mutate(weight = sfnetworks::edge_length())

    paths <-
      base::suppressWarnings(
        sfnetworks::st_network_paths(
          net,
          to = closest_points,
          from = base::rep(
            closest_end_points,
            base::length(closest_points)
          ),
          weights = "weight"
        )
      )

    # Remove paths of zero length
    true_paths <-
      paths$edge_paths[base::sapply(paths$edge_paths, length) > 0]

    # Transform paths to sf objects
    true_paths_sf <-
      true_paths |>
      base::lapply(function(.x) {
        dplyr::slice(sfnetworks::activate(net, "edges"), .x)
      }) |>
      base::lapply(sf::st_as_sf)

    # Find total lengths of an object
    paths_length <-
      base::sapply(
        true_paths_sf,
        function(i) {
          geos::as_geos_geometry(i) |>
            geos::geos_length() |>
            base::sum()
        }
      )

    true_paths_sf[[base::which.max(paths_length)]] |>
      geos::as_geos_geometry() |>
      geos::geos_line_merge() |>
      wk::as_wkt() |>
      base::as.character() |>
      terra::vect(crs = crs)
  }

cnt_path_guess_sf <-
  function(
    input,
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

    if (base::is.null(skeleton)) {
      skeleton <-
        cnt_skeleton(input = input, ...)
    } else if (inherits(skeleton, "SpatVector")) {
      skeleton <-
        terra_to_sf(skeleton)
    } else if (inherits(skeleton, "sf") | inherits(skeleton, "sfc")){
      skeleton <- skeleton
    } else {
      warning("skeleton is not of supported class, rebuilding it...")
      skeleton <-
        cnt_skeleton(input = input, ...)
    }

    # Convert skeleton to sfnetworks
    pol_network <-
      skeleton |>
      sfnetworks::as_sfnetwork(directed = FALSE)

    # Find the most distant nodes from the polygon center
    end_node <-
      pol_network |>
      sfnetworks::activate("nodes") |>
      dplyr::mutate(cc = tidygraph::centrality_closeness()) |>
      dplyr::filter(cc == base::min(cc)) |>
      # Keep only one point
      sf::st_as_sf() |>
      utils::head(n = 1)

    # Find main points of the polygon
    # main_points <-
    #   input |>
    #   rmapshaper::ms_simplify(
    #     keep = 0.01,
    #     method = "dp",
    #     keep_shapes = T
    #   ) |>
    #   sf::st_cast("POINT")

    perimeter_length <-
      geos::geos_length(input_geos)

    point_count <-
      input_geos |>
      geos::geos_unique_points() |>
      geos::geos_num_coordinates()

    point_density <-
      perimeter_length / point_count

    main_points <-
      geos::geos_simplify_preserve_topology(
        input_geos,
        tolerance = point_density / 0.1
      ) |>
      geos::geos_unique_points() |>
      sf::st_as_sf() |>
      sf::st_cast("POINT")

    # Find closest nodes to the above found points
    closest_points <-
      main_points |>
      sf::st_nearest_feature(pol_network, check_crs = FALSE)

    closest_end_points <-
      end_node |>
      sf::st_nearest_feature(pol_network, check_crs = FALSE)

    # Estimate distance between key points and end point
    net <-
      pol_network |>
      sfnetworks::activate("edges") |>
      dplyr::mutate(weight = sfnetworks::edge_length())

    paths <-
      base::suppressWarnings(
        sfnetworks::st_network_paths(
          net,
          to = closest_points,
          from = base::rep(
            closest_end_points,
            base::length(closest_points)
          ),
          weights = "weight"
        )
      )

    # Remove paths of zero length
    true_paths <-
      paths$edge_paths[base::sapply(paths$edge_paths, length) > 0]

    # Transform paths to sf objects
    true_paths_sf <-
      true_paths |>
      base::lapply(function(.x) {
        dplyr::slice(sfnetworks::activate(net, "edges"), .x)
      }) |>
      base::lapply(sf::st_as_sf)

    # Find total lengths of an object
    paths_length <-
      base::sapply(
        true_paths_sf,
        function(i) {
          geos::as_geos_geometry(i) |>
            geos::geos_length() |>
            base::sum()
        }
      )

    true_paths_sf[[base::which.max(paths_length)]] |>
      geos::as_geos_geometry() |>
      geos::geos_line_merge() |>
      sf::st_as_sf() |>
      sf::st_set_crs(crs)
  }


cnt_path_guess_sfc <-
  function(
    input,
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

    if (base::is.null(skeleton)) {
      skeleton <-
        cnt_skeleton(input = input, ...)
    } else if (inherits(skeleton, "SpatVector")) {
      skeleton <-
        terra_to_sf(skeleton)
    } else if (inherits(skeleton, "sf") | inherits(skeleton, "sfc")){
      skeleton <- skeleton
    } else {
      warning("skeleton is not of supported class, rebuilding it...")
      skeleton <-
        cnt_skeleton(input = input, ...)
    }

    # Convert skeleton to sfnetworks
    pol_network <-
      skeleton |>
      sfnetworks::as_sfnetwork(directed = F)

    # Find the most distant nodes from the polygon center
    end_node <-
      pol_network |>
      sfnetworks::activate("nodes") |>
      dplyr::mutate(cc = tidygraph::centrality_closeness()) |>
      dplyr::filter(cc == base::min(cc)) |>
      # Keep only one point
      sf::st_as_sf() |>
      utils::head(n = 1)

    # Find main points of the polygon
    # main_points <-
    #   input |>
    #   rmapshaper::ms_simplify(
    #     keep = 0.01,
    #     method = "dp",
    #     keep_shapes = T
    #   ) |>
    #   sf::st_cast("POINT")

    perimeter_length <-
      geos::geos_length(input_geos)

    point_count <-
      input_geos |>
      geos::geos_unique_points() |>
      geos::geos_num_coordinates()

    point_density <-
      perimeter_length / point_count

    main_points <-
      geos::geos_simplify_preserve_topology(
        input_geos,
        tolerance = point_density / 0.1
      ) |>
      geos::geos_unique_points() |>
      sf::st_as_sf() |>
      sf::st_cast("POINT")

    # Find closest nodes to the above found points
    closest_points <-
      main_points |>
      sf::st_nearest_feature(pol_network, check_crs = FALSE)

    closest_end_points <-
      end_node |>
      sf::st_nearest_feature(pol_network, check_crs = FALSE)

    # Estimate distance between key points and end point
    net <-
      pol_network |>
      sfnetworks::activate("edges") |>
      dplyr::mutate(weight = sfnetworks::edge_length())

    paths <-
      base::suppressWarnings(
        sfnetworks::st_network_paths(
          net,
          to = closest_points,
          from = base::rep(
            closest_end_points,
            base::length(closest_points)
          ),
          weights = "weight"
        )
      )

    # Remove paths of zero length
    true_paths <-
      paths$edge_paths[base::sapply(paths$edge_paths, length) > 0]

    # Transform paths to sf objects
    true_paths_sf <-
      true_paths |>
      base::lapply(function(.x) {
        dplyr::slice(sfnetworks::activate(net, "edges"), .x)
      }) |>
      base::lapply(sf::st_as_sf)

    # Find total lengths of an object
    paths_length <-
      base::sapply(
        true_paths_sf,
        function(i) {
          geos::as_geos_geometry(i) |>
            geos::geos_length() |>
            base::sum()
        }
      )

    true_paths_sf[[base::which.max(paths_length)]] |>
      geos::as_geos_geometry() |>
      geos::geos_line_merge() |>
      sf::st_as_sf() |>
      sf::st_set_crs(crs)
  }
