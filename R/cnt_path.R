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
#' connect \code{start_point} with \code{end_point} by employing the
#' \code{skeleton} of a closed polygon as potential routes.
#'
#' It is important to note that multiple starting points are permissible,
#' but there can only be one ending point. Should there be two or more
#' ending points, the algorithm will generate an error.
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
#' function with the \code{simplify = FALSE} option. In doing so, the resulting
#' skeleton may be more detailed, increasing the likelihood that the starting
#' and ending points are already situated on the skeleton paths.
#'
#' @return a list of \code{sf}, \code{sfc} or \code{SpatVector} class
#' objects of a \code{LINESTRING} geometry
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(terra)
#' library(centerline)
#'
#' # Load Polygon and points data
#' polygon <-
#'   terra::vect(system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "polygon"
#'   )
#'
#' points <-
#'   terra::vect(system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "polygon_points"
#'   )
#'
#' # Find polygon's skeleton
#' pol_skeleton <-
#'   cnt_skeleton(polygon)
#'
#' # Connect points
#' pol_path <-
#'   cnt_path(
#'     skeleton = pol_skeleton,
#'     start_point = subset(points, points$type == "start"),
#'     end_point = subset(points, points$type != "start")
#'   )
#'
#' # Plot
#' plot(polygon)
#' plot(pol_skeleton, col = "blue", add = T)
#' plot(points[1:2, ], col = "red", add = T)
#' plot(pol_path[[1]], lwd = 3, add = T)
#' }
#'
cnt_path <-
  function(skeleton,
           start_point,
           end_point) {
    UseMethod("cnt_path")
  }

#' @export
cnt_path.geos_geometry <-
  function(skeleton,
           start_point,
           end_point) {
    cnt_path_geos(
      skeleton = skeleton,
      start_point = start_point,
      end_point = end_point
    )
  }

#' @export
cnt_path.sf <-
  function(skeleton,
           start_point,
           end_point) {
    cnt_path_sf(
      skeleton = skeleton,
      start_point = start_point,
      end_point = end_point
    )
  }

#' @export
cnt_path.sfc <-
  function(skeleton,
           start_point,
           end_point) {
    cnt_path_sf(
      skeleton = skeleton,
      start_point = start_point,
      end_point = end_point
    )
  }

#' @export
cnt_path.SpatVector <-
  function(skeleton,
           start_point,
           end_point) {
    cnt_path_terra(
      skeleton = skeleton,
      start_point = start_point,
      end_point = end_point
    )
  }


cnt_path_terra <-
  function(skeleton,
           start_point,
           end_point) {
    # Check if input is of class 'SpatVector' and 'lines'
    stopifnot(check_terra_lines(skeleton))

    crs <- terra::crs(skeleton)

    # Transform to sf objects
    skeleton <-
      terra_to_sf(skeleton)
    start_point <-
      terra_to_sf(start_point)
    end_point <-
      terra_to_sf(end_point)

    # Transform to sfnetwork
    pol_network <-
      skeleton |>
      sfnetworks::as_sfnetwork(directed = FALSE)

    # Find indices of nearest nodes for start ...
    start_nodes <-
      sf::st_nearest_feature(start_point, pol_network)

    # ... and end points
    end_nodes <-
      sf::st_nearest_feature(end_point, pol_network)

    # Check if there are several end nodes
    stopifnot(
      length(end_nodes) == 1
    )

    # Measure length of the edges
    net <-
      pol_network |>
      sfnetworks::activate("edges") |>
      dplyr::mutate(length = sfnetworks::edge_length())

    # Find the shortest path among centerline
    paths <-
      sfnetworks::st_network_paths(
        net,
        from = end_nodes,
        to = start_nodes,
        weights = "length"
      )

    # Convert to GEOS geometries and create a GEOS collection
    lines_list_geos <-
      lapply(
        paths$edge_paths,
        function(i) dplyr::slice(sfnetworks::activate(net, "edges"), i)
      ) |>
      lapply(sf::st_as_sf) |>
      lapply(geos::as_geos_geometry) |>
      lapply(geos::geos_make_collection) |>
      lapply(geos::geos_line_merge)

    # Check if we need to reverse the lines
    start_centerline <- geos::geos_point_start(lines_list_geos[[1]])
    end_centerline <- geos::geos_point_end(lines_list_geos[[1]])
    end_geos <-
      geos::as_geos_geometry(end_point)

    start_tail <- geos::geos_distance(end_geos, start_centerline)
    end_tail <- geos::geos_distance(end_geos, end_centerline)

    if (start_tail < end_tail) {
      lines_list_terra <-
        lines_list_geos |>
        lapply(geos::geos_reverse) |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge) |>
        lapply(wk::as_wkt) |>
        lapply(as.character) |>
        lapply(terra::vect, crs = crs)
    } else {
      lines_list_terra <-
        lines_list_geos |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge) |>
        lapply(wk::as_wkt) |>
        lapply(as.character) |>
        lapply(terra::vect, crs = crs)
    }

    # Return pathes binded with the start point df
    Reduce(rbind, lines_list_terra) |>
      cbind(sf::st_drop_geometry(start_point))
  }

cnt_path_sf <-
  function(skeleton,
           start_point,
           end_point) {
    # Check if input is of class 'sf' and 'LINESTRING'
    stopifnot(check_sf_lines(skeleton))

    # Transform to sfnetwork
    pol_network <-
      skeleton |>
      sfnetworks::as_sfnetwork(directed = FALSE)

    # Find indices of nearest nodes for start ...
    start_nodes <-
      sf::st_nearest_feature(start_point, pol_network)

    # ... and end points
    end_nodes <-
      sf::st_nearest_feature(end_point, pol_network)

    # Check if there are several end nodes
    stopifnot(
      length(end_nodes) == 1
    )

    # Measure length of the edges
    net <-
      pol_network |>
      sfnetworks::activate("edges") |>
      dplyr::mutate(length = sfnetworks::edge_length())

    # Find the shortest path among centerline
    paths <-
      sfnetworks::st_network_paths(
        net,
        from = end_nodes,
        to = start_nodes,
        weights = "length"
      )

    # Convert to GEOS geometries and create a GEOS collection
    lines_list_geos <-
      lapply(
        paths$edge_paths,
        function(i) dplyr::slice(sfnetworks::activate(net, "edges"), i)
      ) |>
      lapply(sf::st_as_sf) |>
      lapply(geos::as_geos_geometry) |>
      lapply(geos::geos_make_collection) |>
      lapply(geos::geos_line_merge)

    # Check if we need to reverse the lines
    start_centerline <- geos::geos_point_start(lines_list_geos[[1]])
    end_centerline <- geos::geos_point_end(lines_list_geos[[1]])
    end_geos <-
      geos::as_geos_geometry(end_point)

    start_tail <- geos::geos_distance(end_geos, start_centerline)
    end_tail <- geos::geos_distance(end_geos, end_centerline)

    if (start_tail < end_tail) {
      lines_list_sf <-
        lines_list_geos |>
        lapply(geos::geos_reverse) |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge) |>
        lapply(sf::st_as_sf)
    } else {
      lines_list_sf <-
        lines_list_geos |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge) |>
        lapply(sf::st_as_sf)
    }

    # Return pathes binded with the start point df
    Reduce(rbind, lines_list_sf) |>
      cbind(sf::st_drop_geometry(start_point))
  }

cnt_path_geos <-
  function(skeleton,
           start_point,
           end_point) {
    # Check if input is of class 'geos_geometry' and 'LINESTRING'
    stopifnot(check_geos_lines(skeleton))

    # Transform to sf
    skeleton <-
      sf::st_as_sf(skeleton)
    start_point <-
      sf::st_as_sf(start_point)
    end_point <-
      sf::st_as_sf(end_point)

    # Transform to sfnetwork
    pol_network <-
      skeleton |>
      sfnetworks::as_sfnetwork(directed = FALSE)

    # Find indices of nearest nodes for start ...
    start_nodes <-
      sf::st_nearest_feature(start_point, pol_network)

    # ... and end points
    end_nodes <-
      sf::st_nearest_feature(end_point, pol_network)

    # Check if there are several end nodes
    stopifnot(
      length(end_nodes) == 1
    )

    # Measure length of the edges
    net <-
      pol_network |>
      sfnetworks::activate("edges") |>
      dplyr::mutate(length = sfnetworks::edge_length())

    # Find the shortest path among centerline
    paths <-
      sfnetworks::st_network_paths(
        net,
        from = end_nodes,
        to = start_nodes,
        weights = "length"
      )

    # Convert to GEOS geometries and create a GEOS collection
    lines_list_geos <-
      lapply(
        paths$edge_paths,
        function(i) dplyr::slice(sfnetworks::activate(net, "edges"), i)
      ) |>
      lapply(sf::st_as_sf) |>
      lapply(geos::as_geos_geometry) |>
      lapply(geos::geos_make_collection) |>
      lapply(geos::geos_line_merge)

    # Check if we need to reverse the lines
    start_centerline <- geos::geos_point_start(lines_list_geos[[1]])
    end_centerline <- geos::geos_point_end(lines_list_geos[[1]])
    end_geos <- geos::as_geos_geometry(end_point)

    start_tail <- geos::geos_distance(end_geos, start_centerline)
    end_tail <- geos::geos_distance(end_geos, end_centerline)

    if (start_tail < end_tail) {
      lines_list_sf <-
        lines_list_geos |>
        lapply(geos::geos_reverse) |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge)
    } else {
      lines_list_sf <-
        lines_list_geos |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge)
    }

    # Return pathes
    Reduce(c, lines_list_sf)
  }
