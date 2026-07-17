#' Find the shortest path between start and end points within a polygon
#'
#' @param skeleton an output from [centerline::cnt_skeleton()] function
#' @param start_point one or more starting points. It should be of the same
#' class as the \code{skeleton} parameter
#' @param end_point one ending point of the same class as \code{skeleton} and
#' \code{start_point} parameters.
#'
#' @details
#' The function connects start and end points with weighted shortest paths
#' on an undirected skeleton graph using [igraph::shortest_paths()].
#' The \code{skeleton} of a closed polygon provides the potential routes.
#'
#' It is important to note that multiple starting points are permissible,
#' but there can only be **one ending point**. Should there be two or more
#' ending points, the algorithm will return an error.
#'
#' Neither starting nor ending points are required to be located
#' on the edges of a polygon (i.e., snapped to the boundary);
#' they can be positioned wherever possible inside the polygon.
#'
#' By default, the algorithm identifies the closest nodes of the polygon's
#' skeleton to the starting and ending points and then connects them using the
#' shortest path possible along the skeleton. Therefore, if more precise
#' placement of start and end points is necessary, consider executing
#' [centerline::cnt_skeleton()] with \code{keep = 1}. In doing so, the resulting
#' skeleton may be more detailed, increasing the likelihood that the starting
#' and ending points are already situated on the skeleton paths.
#'
#' For exact boundary terminals, pass the same boundary points to
#' [cnt_skeleton()] as \code{anchors}. Those anchors become degree-one graph
#' nodes, so [cnt_path()] returns zero endpoint distance from the supplied
#' points rather than nearest-node snaps.
#'
#' Every start point's nearest skeleton node must be connected to the end
#' point's nearest node. Using \code{anchors} in [cnt_skeleton()] ensures that
#' terminal nodes exist, but it does not make a multi-component skeleton
#' globally connected. If any requested route is empty, [cnt_path()] stops
#' without returning partial paths.
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
#' # Connect points (nearest-node snapping)
#' pol_path <-
#'   cnt_path(
#'     skeleton = pol_skeleton,
#'     start_point = points[2],
#'     end_point = points[1]
#'   )
#'
#' # Exact boundary terminals via anchors
#' boundary <- geos::geos_boundary(polygon)
#' is_boundary <- geos::geos_equals(
#'   geos::geos_intersection(boundary, points),
#'   points
#' )
#' anchors <- points[is_boundary]
#' anchored_skeleton <- cnt_skeleton(polygon, keep = 1, anchors = anchors)
#' anchored_path <- cnt_path(anchored_skeleton, anchors[1], anchors[2])
#'
#' # Plot
#' plot(polygon)
#' plot(pol_skeleton, col = "blue", add = TRUE)
#' plot(points[1:2], col = "red", add = TRUE)
#' plot(pol_path, lwd = 3, add = TRUE)

cnt_path <- function(skeleton, start_point, end_point) {
  UseMethod("cnt_path")
}

#' @export
cnt_path.geos_geometry <- function(skeleton, start_point, end_point) {
  # Check input classes
  stopifnot(check_lines(skeleton))
  stopifnot(check_points(start_point))
  stopifnot(check_points(end_point))
  check_same_class(skeleton, start_point, end_point)

  cnt_path_master(
    skeleton_geos = skeleton,
    start_geos = start_point,
    end_geos = end_point
  )
}

#' @export
cnt_path.sf <- function(skeleton, start_point, end_point) {
  # Check input classes
  stopifnot(check_lines(skeleton))
  stopifnot(check_points(start_point))
  stopifnot(check_points(end_point))
  check_same_class(skeleton, start_point, end_point)

  # Find the paths
  cnt_path_master(
    skeleton_geos = geos::as_geos_geometry(skeleton),
    start_geos = geos::as_geos_geometry(start_point),
    end_geos = geos::as_geos_geometry(end_point)
  ) |>
    sf::st_as_sf() |>
    cbind(sf::st_drop_geometry(start_point))
}

#' @export
cnt_path.sfc <- function(skeleton, start_point, end_point) {
  # Check input classes
  stopifnot(check_lines(skeleton))
  stopifnot(check_points(start_point))
  stopifnot(check_points(end_point))

  # Find the paths
  cnt_path_master(
    skeleton_geos = geos::as_geos_geometry(skeleton),
    start_geos = geos::as_geos_geometry(start_point),
    end_geos = geos::as_geos_geometry(end_point)
  ) |>
    sf::st_as_sfc()
}

#' @export
cnt_path.SpatVector <- function(skeleton, start_point, end_point) {
  # Check input classes
  stopifnot(check_lines(skeleton))
  stopifnot(check_points(start_point))
  stopifnot(check_points(end_point))
  check_same_class(skeleton, start_point, end_point)

  # Save CRS
  crs <- terra::crs(skeleton)

  start_sf <- sf::st_as_sf(start_point)

  # Find the paths
  cnt_path_master(
    skeleton_geos = terra_to_geos(skeleton),
    start_geos = terra_to_geos(start_point),
    end_geos = terra_to_geos(end_point)
  ) |>
    wk::as_wkt() |>
    as.character() |>
    terra::vect(crs = crs) |>
    cbind(sf::st_drop_geometry(start_sf))
}


cnt_path_master <- function(skeleton_geos, start_geos, end_geos) {
  g <- skeleton_graph_build(skeleton_geos, crs = wk::wk_crs(skeleton_geos))

  start_nodes <- skeleton_graph_nearest_nodes(g, start_geos)
  end_nodes <- skeleton_graph_nearest_nodes(g, end_geos)

  # Check if there are several end nodes
  stopifnot("Only one end point is allowed" = length(end_nodes) == 1)

  # Route from the single end ID to all start IDs
  edge_paths <- skeleton_graph_paths(g, from = end_nodes, to = start_nodes)

  failed_start_indices <- which(lengths(edge_paths) == 0L)
  if (length(failed_start_indices) > 0L) {
    if (length(start_nodes) > 1L) {
      stop(
        "Start and end points are not connected by the skeleton graph ",
        "(nearest nodes lie in different connected components or path is empty).",
        " Failed start indices: ",
        paste(failed_start_indices, collapse = ", "),
        "."
      )
    }
    stop(
      "Start and end points are not connected by the skeleton graph ",
      "(nearest nodes lie in different connected components or path is empty)."
    )
  }

  lines_list_geos <- lapply(edge_paths, function(epath) {
    skeleton_graph_path_line(g, epath)
  })

  # Check if we need to reverse the lines
  rev_lines_list <- reverse_lines_if_needed(lines_list_geos, end_geos)

  # Return paths binded together as GEOS geometry
  do.call(c, rev_lines_list)
}
