#' Find the shortest path between start and end points within a polygon
#'
#' @param skeleton an output from [centerline::cnt_skeleton()] function
#' @param start_point one or more starting points. It should be of the same
#' class as the \code{skeleton} parameter
#' @param end_point one ending point of the same class as \code{skeleton} and
#' \code{start_point} parameters.
#'
#' @details
#' The following function uses the [sfnetworks::st_network_paths()] approach to
#' connect \code{start_point} with \code{end_point} by using the
#' \code{skeleton} of a closed polygon as potential routes.
#'
#' It is important to note that multiple starting points are permissible,
#' but there can only be **one ending point**. Should there be two or more
#' ending points, the algorithm will return an error.
#'
#' Neither starting nor ending points are required to be located
#' on the edges of a polygon (i.e., snapped to the boundary);
#' they can be positioned wherever possible inside the polygon.
#'
#' The algorithm identifies the closest nodes of the polygon's skeleton
#' to the starting and ending points and then connects them
#' using the shortest path possible along the skeleton.
#' Therefore, if more precise placement of start and end
#' points is necessary, consider executing the [centerline::cnt_skeleton()]
#' function with the \code{keep = 1} option. In doing so, the resulting
#' skeleton may be more detailed, increasing the likelihood that the starting
#' and ending points are already situated on the skeleton paths.
#'
#' @return a list of \code{sf}, \code{sfc}, \code{SpatVector}
#' or \code{geos_geometry} class objects of a \code{LINESTRING} geometry
#'
#' @export
#'
#' @examples
#' library(sf)
#' library(geos)
#' # Load Polygon and points data
#' polygon <-
#'   sf::st_read(
#'     system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "polygon",
#'     quiet = TRUE
#'   ) |>
#'   geos::as_geos_geometry()
#'
#' points <-
#'   sf::st_read(
#'     system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "polygon_points",
#'     quiet = TRUE
#'   ) |>
#'   geos::as_geos_geometry()
#'
#' # Find polygon's skeleton
#' pol_skeleton <- cnt_skeleton(polygon)
#'
#' # Connect points
#' pol_path <-
#'   cnt_path(
#'     skeleton = pol_skeleton,
#'     start_point = points[2],
#'     end_point = points[1]
#'   )
#'
#' # Plot
#' plot(polygon)
#' plot(pol_skeleton, col = "blue", add = TRUE)
#' plot(points[1:2], col = "red", add = TRUE)
#' plot(pol_path, lwd = 3, add = TRUE)

cnt_path <-
  function(skeleton, start_point, end_point) {
    UseMethod("cnt_path")
  }

#' @export
cnt_path.geos_geometry <-
  function(skeleton, start_point, end_point) {
    # Check input classes
    stopifnot(check_lines(skeleton))
    stopifnot(check_points(start_point))
    stopifnot(check_points(end_point))
    check_same_class(
      skeleton,
      start_point,
      end_point
    )

    if (any(get_geom_type(skeleton) == "multilinestring")) {
      skeleton <-
        geos::geos_unnest(skeleton, keep_multi = FALSE)
    }

    # Transform to sf
    skeleton <-
      sf::st_as_sf(skeleton)
    start_point <-
      sf::st_as_sf(start_point)
    end_point <-
      sf::st_as_sf(end_point)

    # Find the paths
    cnt_path_master(skeleton, start_point, end_point)
  }

#' @export
cnt_path.sf <-
  function(skeleton, start_point, end_point) {
    # Check input classes
    stopifnot(check_lines(skeleton))
    stopifnot(check_points(start_point))
    stopifnot(check_points(end_point))
    check_same_class(
      skeleton,
      start_point,
      end_point
    )

    if (any(get_geom_type(skeleton) == "MULTILINESTRING")) {
      skeleton <-
        sf::st_cast(skeleton, "LINESTRING")
    }

    # Find the paths
    cnt_path_master(skeleton, start_point, end_point) |>
      sf::st_as_sf() |>
      cbind(sf::st_drop_geometry(start_point))
  }

#' @export
cnt_path.sfc <-
  function(skeleton, start_point, end_point) {
    # Check input classes
    stopifnot(check_lines(skeleton))
    stopifnot(check_points(start_point))
    stopifnot(check_points(end_point))

    if (any(get_geom_type(skeleton) == "MULTILINESTRING")) {
      skeleton <-
        sf::st_cast(skeleton, "LINESTRING")
    }

    # Find the paths
    cnt_path_master(skeleton, start_point, end_point) |>
      sf::st_as_sfc()
  }

#' @export
cnt_path.SpatVector <-
  function(skeleton, start_point, end_point) {
    # Check input classes
    stopifnot(check_lines(skeleton))
    stopifnot(check_points(start_point))
    stopifnot(check_points(end_point))
    check_same_class(
      skeleton,
      start_point,
      end_point
    )

    # Save CRS
    crs <- terra::crs(skeleton)

    # Transform to sf objects
    skeleton <-
      sf::st_as_sf(skeleton)
    start_point <-
      sf::st_as_sf(start_point)
    end_point <-
      sf::st_as_sf(end_point)

    if (any(get_geom_type(skeleton) == "MULTILINESTRING")) {
      skeleton <-
        sf::st_cast(skeleton, "LINESTRING")
    }

    # Find the paths
    cnt_path_master(skeleton, start_point, end_point) |>
      wk::as_wkt() |>
      as.character() |>
      terra::vect(crs = crs) |>
      cbind(sf::st_drop_geometry(start_point))
  }


cnt_path_master <-
  function(skeleton_sf, start_point_sf, end_point_sf) {
    # Convert skeleton sf object to sfnetworks
    pol_network <-
      sfnetworks::as_sfnetwork(
        x = skeleton_sf,
        directed = FALSE,
        length_as_weight = TRUE,
        edges_as_lines = TRUE
      )

    # Convert sfnetworks to igraph
    df_graph <- igraph::as_data_frame(pol_network)
    names(df_graph)[3] <- "geometry"
    df_graph <- df_graph[, c("weight", "geometry")]
    df_graph$weight <- as.numeric(df_graph$weight)

    # Find indices of nearest nodes for start ...
    start_nodes <-
      sf::st_nearest_feature(start_point_sf, pol_network)
    # ... and end points
    end_nodes <-
      sf::st_nearest_feature(end_point_sf, pol_network)

    # Check if there are several end nodes
    stopifnot(
      "Only one end point is allowed" = length(end_nodes) == 1
    )

    # Find the shortest path between two points
    paths <-
      sfnetworks::st_network_paths(
        pol_network,
        from = end_nodes,
        to = start_nodes,
        weights = "weight"
      )

    # Convert to GEOS geometries and create a GEOS collection
    lines_list_geos <-
      lapply(paths$edge_paths, function(x) df_graph[x, "geometry"]) |>
      lapply(geos::as_geos_geometry) |>
      lapply(geos::geos_make_collection) |>
      lapply(geos::geos_line_merge)

    # Check if we need to reverse the lines
    rev_lines_list <-
      reverse_lines_if_needed(lines_list_geos, end_point_sf)

    # Return pathes binded together as GEOS geometry
    do.call(c, rev_lines_list)
  }
