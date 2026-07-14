#' Create a skeleton of a closed polygon object
#'
#' This function generates skeletons of closed polygon objects. Optional
#' boundary `anchors` can be attached as exact terminal graph nodes so that
#' [cnt_path()] can start or end at those points without nearest-node snapping.
#'
#' @param input \code{sf}, \code{sfc}, \code{SpatVector}, or
#' \code{geos_geometry} polygons object
#' @param keep numeric, proportion of points to retain (0.05-5.0; default 0.5).
#' See Details.
#' @param method character, either \code{"voronoi"} (default) or
#' \code{"straight"}, or just the first letter \code{"v"} or \code{"s"}.
#' See Details.
#' @param anchors \code{NULL} (default) or boundary \code{POINT} geometries of
#' the same spatial class and CRS as \code{input}. When supplied, each point
#' must lie on exactly one polygon-part boundary (exterior or hole ring) and is
#' attached to the ordinary skeleton by a direct interior connector. Anchors
#' never become Voronoi sites, and ordinary skeleton branches are never pruned.
#' Attribute columns on \code{anchors} are ignored; the returned skeleton still
#' inherits polygon attributes exactly as without anchors.
#'
#' @details
#' ## Polygon simplification/densification
#' - If \code{keep = 1}, no transformation will occur. The
#' function will use the original geometry to find the skeleton.
#'
#' - If the \code{keep} parameter is below 1, then the [geos::geos_simplify()]
#' function will be used. So the original input
#' geometry would be simplified, and the resulting skeleton will be cleaner but
#' maybe more edgy.
#' The current realisation of simplification is similar (*but not identical*)
#' to \code{rmapshaper::ms_simplify()} one with Douglas-Peuker algorithm.
#' However, due to \code{geos} superpower, it performs several times faster.
#' If you find that the built-in simplification algorithm performs poorly,
#' try \code{rmapshaper::ms_simplify()} first and then find the polygon skeleton
#' with \code{keep = 1}, i.e.
#' \code{cnt_skeleton(rmapshaper::ms_simplify(polygon_sf), keep = 1)}
#'
#' - If the \code{keep} is above 1, then the densification
#' algorithm is applied using the [geos::geos_densify()] function. This may
#'  produce a very large object if keep is set more than 2. However, the
#'  resulting skeleton would potentially be more accurate.
#'
#' ## Skeleton method
#' - If \code{method = "voronoi"} (default), the skeleton will be generated
#' using the [geos::geos_voronoi_edges()] function. This is application of the
#' Voronoi diagram algorithm (Voronoi, 1908).
#' A Voronoi diagram partitions space into regions based on the distance to
#' the polygon's vertices. The edges of these
#' cells form a network of lines (skeletons) that represent
#' the structure of the polygon while preserving its overall shape.
#'
#' - If \code{method = "straight"}, the skeleton will be generated
#' using the [raybevel::skeletonize()] function. See
#' \url{https://www.tylermw.com/posts/rayverse/raybevel-introduction.html}
#'
#' ## Boundary anchors
#' When \code{anchors} is supplied, the ordinary skeleton is still built from
#' the polygon alone under the existing \code{keep} and \code{method} behavior.
#' Each accepted boundary point is then joined to a valid interior junction by a
#' direct connector that stays inside the polygon, meets the boundary only at
#' that anchor, and meets the ordinary skeleton only at a pre-existing
#' degree-at-least-three junction. Connector selection uses planar Euclidean
#' distance and turn-angle scoring in the input coordinate space; use projected
#' coordinates when that ranking must be spatially meaningful. Calls fail when
#' an anchor is off-boundary, shared by multiple parts, duplicated, or has no
#' valid direct connector.
#'
#' @references Voronoi, G. (1908). Nouvelles applications des paramètres
#' continus à la théorie des formes quadratiques. Journal für die reine und
#' angewandte Mathematik, 134, 198-287. \doi{10.1515/crll.1908.134.198}
#'
#' @return a \code{sf}, \code{sfc}, \code{SpatVector}
#' or \code{geos_geometry} class object of a \code{MULTILINESTRING} geometry
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' polygon <-
#'   sf::st_read(system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "polygon",
#'     quiet = TRUE
#'   )
#'
#' plot(polygon)
#'
#' pol_skeleton <- cnt_skeleton(polygon)
#'
#' plot(pol_skeleton)
#'
#' # Attach exact boundary terminals for routing
#' points <- sf::st_read(
#'   system.file("extdata/example.gpkg", package = "centerline"),
#'   layer = "polygon_points",
#'   quiet = TRUE
#' )
#' boundary <- sf::st_boundary(polygon)
#' anchors <- points[
#'   as.numeric(sf::st_distance(points, boundary)) == 0,
#' ]
#' anchored <- cnt_skeleton(polygon, keep = 1, anchors = anchors)
#' plot(anchored)
cnt_skeleton <- function(
  input,
  keep = 0.5,
  method = "voronoi",
  anchors = NULL
) {
  UseMethod("cnt_skeleton")
}

#' @export
cnt_skeleton.geos_geometry <- function(
  input,
  keep = 0.5,
  method = c("voronoi", "straight"),
  anchors = NULL
) {
  # Check input arguments
  stopifnot(check_polygons(input))
  stopifnot(check_anchors(input, anchors))
  checkmate::assert_number(keep, lower = 0.05, upper = 5.0)
  method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

  # Save CRS
  crs <- wk::wk_crs(input)

  anchors_geos <- if (is.null(anchors) || length(anchors) == 0L) {
    NULL
  } else {
    anchors
  }

  pol_skeleton <- cnt_skeleton_geos_all(
    input = input,
    anchors = anchors_geos,
    keep = keep,
    method = method
  )

  wk::wk_crs(pol_skeleton) <- crs

  pol_skeleton
}

#' @export
cnt_skeleton.sf <- function(
  input,
  keep = 0.5,
  method = c("voronoi", "straight"),
  anchors = NULL
) {
  # Check input arguments
  stopifnot(check_polygons(input))
  stopifnot(check_anchors(input, anchors))
  checkmate::assert_number(keep, lower = 0.05, upper = 5.0)
  method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

  # Save CRS
  crs <- sf::st_crs(input)

  # Transform to geos_geometry
  input_geos <- geos::as_geos_geometry(input)
  anchors_geos <- if (
    is.null(anchors) || (inherits(anchors, "sf") && nrow(anchors) == 0L)
  ) {
    NULL
  } else {
    geos::as_geos_geometry(anchors)
  }

  pol_skeleton <- cnt_skeleton_geos_all(
    input = input_geos,
    anchors = anchors_geos,
    keep = keep,
    method = method
  )

  # Transform back to sf
  pol_skeleton_crs <- pol_skeleton |>
    sf::st_as_sf() |>
    sf::st_set_crs(crs) |>
    cbind(sf::st_drop_geometry(input))

  pol_skeleton_crs
}

#' @export
cnt_skeleton.sfc <- function(
  input,
  keep = 0.5,
  method = c("voronoi", "straight"),
  anchors = NULL
) {
  # Check input arguments
  stopifnot(check_polygons(input))
  stopifnot(check_anchors(input, anchors))
  checkmate::assert_number(keep, lower = 0.05, upper = 5.0)
  method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

  # Save CRS
  crs <- sf::st_crs(input)

  # Transform to geos_geometry
  input_geos <- geos::as_geos_geometry(input)
  anchors_geos <- if (is.null(anchors) || length(anchors) == 0L) {
    NULL
  } else {
    geos::as_geos_geometry(anchors)
  }

  pol_skeleton <- cnt_skeleton_geos_all(
    input = input_geos,
    anchors = anchors_geos,
    keep = keep,
    method = method
  )

  # Transform back to sfc
  pol_skeleton_crs <- pol_skeleton |> sf::st_as_sfc() |> sf::st_set_crs(crs)

  pol_skeleton_crs
}

#' @export
cnt_skeleton.SpatVector <- function(
  input,
  keep = 0.5,
  method = c("voronoi", "straight"),
  anchors = NULL
) {
  check_package("terra")

  # Check input arguments
  stopifnot(check_polygons(input))
  stopifnot(check_anchors(input, anchors))
  checkmate::assert_number(keep, lower = 0.05, upper = 5.0)
  method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

  # Input attributes
  input_data <- terra::as.data.frame(input)

  # Transform to GEOS geometry
  input_geos <- terra_to_geos(input)
  anchors_geos <- if (is.null(anchors) || nrow(anchors) == 0L) {
    NULL
  } else {
    terra_to_geos(anchors)
  }

  pol_skeleton <- cnt_skeleton_geos_all(
    input = input_geos,
    anchors = anchors_geos,
    keep = keep,
    method = method
  )

  # Transform back to SpatVect
  pol_skeleton_crs <- geos_to_terra(pol_skeleton)

  if (nrow(input_data) == 0) {
    return(pol_skeleton_crs)
  } else if (nrow(input_data) == nrow(pol_skeleton_crs)) {
    pol_skeleton_crs <- pol_skeleton_crs |> cbind(input_data)
    return(pol_skeleton_crs)
  } else if (nrow(input_data) == 1 && nrow(pol_skeleton_crs) > 1) {
    pol_skeleton_crs <- pol_skeleton_crs |>
      cbind(input_data[rep(1, nrow(pol_skeleton_crs)), ])
    return(pol_skeleton_crs)
  } else {
    warning(
      "input and skeleton have different number of rows,
      returning skeleton without attributes"
    )
    return(pol_skeleton_crs)
  }
}

# Internal coordinator: unnest parts, generate ordinary skeletons, then
# attach any validated boundary anchors per part.
cnt_skeleton_geos_all <- function(
  input,
  anchors = NULL,
  keep = 0.5,
  method = c("voronoi", "straight")
) {
  method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

  input_geom_type <- geos::geos_type(input)
  if (any(input_geom_type == "multipolygon")) {
    input <- geos::geos_unnest(input, keep_multi = FALSE)
  }

  membership <- match_boundary_anchors(input, anchors)

  generator <- if (method == "voronoi") {
    cnt_skeleton_geos
  } else {
    cnt_skeleton_straight
  }

  skeletons <- lapply(seq_along(input), function(i) {
    sk <- generator(input[i], keep = keep)
    part_anchor_idx <- membership[[i]]
    if (length(part_anchor_idx) == 0L) {
      return(sk)
    }
    add_boundary_anchors(
      skeleton = sk,
      polygon = input[i],
      anchors = anchors[part_anchor_idx]
    )
  })

  do.call(c, skeletons)
}

cnt_skeleton_geos <- function(input, keep = 0.5) {
  # Simplify or densify or do nothing
  if (keep == 1) {
    pol_geos <- input
  } else if (keep < 1 && keep >= 0.05) {
    # Simplify
    pol_geos <- geos_ms_simplify(input, keep = keep)
  } else if (keep < 0.05) {
    # Simplify at lower bound, otherwise result
    # might be poor
    pol_geos <- geos_ms_simplify(input, keep = 0.05)
  } else {
    # Densify
    pol_geos <- geos_ms_densify(input, keep = keep)
  }

  # Find polygon skeleton
  pol_skeleton <- pol_geos |>
    geos::geos_unique_points() |>
    geos::geos_voronoi_edges() |>
    geos::geos_intersection(pol_geos) |>
    geos::geos_unnest(keep_multi = FALSE) |>
    geos::geos_make_collection() |>
    geos::geos_line_merge()

  pol_skeleton
}

cnt_skeleton_straight <- function(input, keep = 0.5) {
  check_package("raybevel")

  if (keep > 1) {
    warning(
      "Generating a straight skeleton with
        keep > 1 is not recommended and may take a very long time."
    )
  }

  # Simplify or densify or do nothing
  if (keep == 1) {
    pol_geos <- input
  } else if (keep < 1 && keep >= 0.05) {
    # Simplify
    pol_geos <- geos_ms_simplify(input, keep = keep)
  } else if (keep < 0.05) {
    # Simplify at lower bound, otherwise result
    # might be poor
    pol_geos <- geos_ms_simplify(input, keep = 0.05)
  } else {
    # Densify
    pol_geos <- geos_ms_densify(input, keep = keep)
  }

  # Estimate polygon holes
  num_rings <- geos::geos_num_interior_rings(pol_geos)
  stopifnot(!is.null(num_rings))

  if (num_rings == 0) {
    sk <- raybevel::skeletonize(
      vertices = geos_to_matrix(pol_geos),
      return_raw_ss = FALSE
    )
  } else {
    geos_outer <- pol_geos |> get_geos_ring(1) |> geos_to_matrix()
    geos_inner <- list_geos_inner_rings(pol_geos, num_rings)
    sk <- raybevel::skeletonize(
      vertices = geos_outer,
      holes = geos_inner,
      return_raw_ss = FALSE
    )
  }
  raybevel_to_geos(sk, crs = wk::wk_crs(input))
}
