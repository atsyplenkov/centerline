#' Create a skeleton of a closed polygon object
#'
#' This function generates skeletons of closed polygon objects.
#'
#' @param input \code{sf}, \code{sfc}, \code{SpatVector}, or
#' \code{geos_geometry} polygons object
#' @param keep numeric, proportion of points to retain (0.05-5.0; default 0.5).
#' See Details.
#' @param method character, either \code{"voronoi"} (default) or
#' \code{"straight"}, or just the first letter \code{"v"} or \code{"s"}.
#' See Details.
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
cnt_skeleton <-
  function(input, keep = 0.5, method = "voronoi") {
    UseMethod("cnt_skeleton")
  }

#' @export
cnt_skeleton.geos_geometry <-
  function(input, keep = 0.5, method = c("voronoi", "straight")) {
    # Check input arguments
    stopifnot(check_polygons(input))
    checkmate::assert_number(keep, lower = 0.05, upper = 5.0)
    method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

    # Save CRS
    crs <- wk::wk_crs(input)

    # Transform to geos_geometry
    input_geom_type <- geos::geos_type(input)

    # Check if input is of geometry type 'MULTIPOLYGON'
    if (any(input_geom_type == "multipolygon")) {
      input <-
        geos::geos_unnest(input, keep_multi = FALSE)
    }

    # Find GEOS skeleton
    if (method == "voronoi") {
      pol_skeleton <-
        do.call(c, lapply(input, cnt_skeleton_geos, keep = keep))
    } else if (method == "straight") {
      pol_skeleton <-
        do.call(c, lapply(input, cnt_skeleton_straight, keep = keep))
    }

    wk::wk_crs(pol_skeleton) <- crs

    pol_skeleton
  }

#' @export
cnt_skeleton.sf <-
  function(input, keep = 0.5, method = c("voronoi", "straight")) {
    # Check input arguments
    stopifnot(check_polygons(input))
    checkmate::assert_number(keep, lower = 0.05, upper = 5.0)
    method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

    # Save CRS
    crs <- sf::st_crs(input)

    # Transform to geos_geometry
    input_geos <- geos::as_geos_geometry(input)
    input_geom_type <- geos::geos_type(input)

    # Check if input is of geometry type 'MULTIPOLYGON'
    if (any(input_geom_type == "multipolygon")) {
      input_geos <-
        geos::geos_unnest(input_geos, keep_multi = FALSE)
    }

    # Find GEOS skeleton
    if (method == "voronoi") {
      pol_skeleton <-
        do.call(c, lapply(input_geos, cnt_skeleton_geos, keep = keep))
    } else if (method == "straight") {
      pol_skeleton <-
        do.call(c, lapply(input_geos, cnt_skeleton_straight, keep = keep))
    }

    # Transform back to sf
    pol_skeleton_crs <-
      pol_skeleton |>
      sf::st_as_sf() |>
      sf::st_set_crs(crs) |>
      cbind(sf::st_drop_geometry(input))

    pol_skeleton_crs
  }

#' @export
cnt_skeleton.sfc <-
  function(input, keep = 0.5, method = c("voronoi", "straight")) {
    # Check input arguments
    stopifnot(check_polygons(input))
    checkmate::assert_number(keep, lower = 0.05, upper = 5.0)
    method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

    # Save CRS
    crs <- sf::st_crs(input)

    # Transform to geos_geometry
    input_geos <- geos::as_geos_geometry(input)
    input_geom_type <- geos::geos_type(input)

    # Check if input is of geometry type 'MULTIPOLYGON'
    if (any(input_geom_type == "multipolygon")) {
      input_geos <-
        geos::geos_unnest(input_geos, keep_multi = FALSE)
    }

    # Find GEOS skeleton
    if (method == "voronoi") {
      pol_skeleton <-
        do.call(c, lapply(input_geos, cnt_skeleton_geos, keep = keep))
    } else if (method == "straight") {
      pol_skeleton <-
        do.call(c, lapply(input_geos, cnt_skeleton_straight, keep = keep))
    }

    # Transform back to sf
    pol_skeleton_crs <-
      pol_skeleton |>
      sf::st_as_sfc() |>
      sf::st_set_crs(crs)

    pol_skeleton_crs
  }

#' @export
cnt_skeleton.SpatVector <-
  function(input, keep = 0.5, method = c("voronoi", "straight")) {
    check_package("terra")

    # Check input arguments
    stopifnot(check_polygons(input))
    checkmate::assert_number(keep, lower = 0.05, upper = 5.0)
    method <- checkmate::matchArg(method, choices = c("voronoi", "straight"))

    # Input attributes
    input_data <- terra::as.data.frame(input)

    # Transform to GEOS geometry
    input_geos <- terra_to_geos(input)
    input_geom_type <- geos::geos_type(input_geos)

    # Check if input is of geometry type 'MULTIPOLYGON'
    if (any(input_geom_type == "multipolygon")) {
      input_geos <-
        geos::geos_unnest(input_geos, keep_multi = FALSE)
    }

    # Find GEOS skeleton
    if (method == "voronoi") {
      pol_skeleton <-
        do.call(c, lapply(input_geos, cnt_skeleton_geos, keep = keep))
    } else if (method == "straight") {
      pol_skeleton <-
        do.call(c, lapply(input_geos, cnt_skeleton_straight, keep = keep))
    }

    # Transform back to SpatVect
    pol_skeleton_crs <-
      geos_to_terra(pol_skeleton)

    if (nrow(input_data) == 0) {
      return(pol_skeleton_crs)
    } else if (nrow(input_data) == nrow(pol_skeleton_crs)) {
      pol_skeleton_crs <-
        pol_skeleton_crs |>
        cbind(input_data)
      return(pol_skeleton_crs)
    } else if (nrow(input_data) == 1 && nrow(pol_skeleton_crs) > 1) {
      pol_skeleton_crs <-
        pol_skeleton_crs |>
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

cnt_skeleton_geos <-
  function(input, keep = 0.5) {
    # Simplify or densify or do nothing
    if (keep == 1) {
      pol_geos <- input
    } else if (keep < 1 && keep >= 0.05) {
      # Simplify
      pol_geos <-
        geos_ms_simplify(input, keep = keep)
    } else if (keep < 0.05) {
      # Simplify at lower bound, otherwise result
      # might be poor
      pol_geos <-
        geos_ms_simplify(input, keep = 0.05)
    } else {
      # Densify
      pol_geos <-
        geos_ms_densify(input, keep = keep)
    }

    # Find polygon skeleton
    pol_skeleton <-
      pol_geos |>
      geos::geos_voronoi_edges() |>
      geos::geos_intersection(pol_geos) |>
      geos::geos_unnest(keep_multi = FALSE) |>
      geos::geos_make_collection() |>
      geos::geos_line_merge()

    pol_skeleton
  }

cnt_skeleton_straight <-
  function(input, keep = 0.5) {
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
      pol_geos <-
        geos_ms_simplify(input, keep = keep)
    } else if (keep < 0.05) {
      # Simplify at lower bound, otherwise result
      # might be poor
      pol_geos <-
        geos_ms_simplify(input, keep = 0.05)
    } else {
      # Densify
      pol_geos <-
        geos_ms_densify(input, keep = keep)
    }

    # Estimate polygon holes
    num_rings <-
      geos::geos_num_interior_rings(pol_geos)
    stopifnot(!is.null(num_rings))

    if (num_rings == 0) {
      sk <-
        raybevel::skeletonize(
          vertices = geos_to_matrix(pol_geos),
          return_raw_ss = FALSE
        )
    } else {
      geos_outer <-
        pol_geos |>
        get_geos_ring(1) |>
        geos_to_matrix()
      geos_inner <- list_geos_inner_rings(pol_geos, num_rings)
      sk <-
        raybevel::skeletonize(
          vertices = geos_outer,
          holes = geos_inner,
          return_raw_ss = FALSE
        )
    }
    raybevel_to_geos(sk, crs = wk::wk_crs(input))
  }
