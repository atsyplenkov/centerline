#' Create a skeleton of a closed polygon object
#'
#' @param input \code{sf}, \code{sfc}, \code{SpatVector}, or
#' \code{geos_geometry} polygons object
#' @param keep numeric, proportion of points to retain (0.05-Inf; default 0.5).
#' See Details.
#'
#' @details
#' - If \code{keep} equals 1, no transformation will occur. The
#' function will use the original geometry to find the skeleton.
#'
#' - If the \code{keep} parameter is below 1, then the [geos::geos_simplify()]
#' function will be used. So the original input
#' geometry would be simplified, and the resulting skeleton will be cleaner but
#' maybe more edgy.
#' The current realisation of simplification is similar (*but not identical*)
#' to `rmapshaper::ms_simplify()` one with Douglas-Peuker algorithm. However,
#' due to \code{geos} superpower, it performs *ca.* 50x times faster.
#' If you find that the built-in simplification algorithm performs poorly,
#' try `rmapshaper::ms_simplify()` first and then find the polygon skeleton
#' with `keep = 1`, i.e.
#' \code{cnt_skeleton(rmapshaper::ms_simplify(polygon_sf), keep = 1)}
#'
#' - If the \code{keep} is above 1, then the densification
#' algorithm is applied using the [geos::geos_densify()] function. This may
#'  produce a very large object if keep is set more than 2. However, the
#'  resulting skeleton would potentially be more accurate.
#'
#' @return a \code{sf}, \code{sfc}, \code{SpatVector}
#' or \code{geos_geometry} class object of a \code{LINESTRING} geometry
#'
#' @export
#'
#'
#' @examples
#' library(sf)
#'
#' polygon <-
#'   sf::st_read(system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "polygon"
#'   )
#'
#' plot(polygon)
#'
#' pol_skeleton <- cnt_skeleton(polygon)
#'
#' plot(pol_skeleton)
cnt_skeleton <-
  function(input,
           keep = 0.5) {
    UseMethod("cnt_skeleton")
  }

#' @export
cnt_skeleton.geos_geometry <-
  function(input,
           keep = 0.5) {
    cnt_skeleton_geos(
      input = input,
      keep = keep
    )
  }

#' @export
cnt_skeleton.sf <-
  function(input,
           keep = 0.5) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_sf_polygon(input))

    # Save CRS
    crs <-
      sf::st_crs(input)

    # Transform to GEOS geometry
    input_geos <-
      geos::as_geos_geometry(input)

    # Find GEOS skeleton
    pol_skeleton <-
      cnt_skeleton_geos(input = input_geos, keep = keep)

    # Transform back to sf
    pol_skeleton_crs <-
      sf::st_as_sf(x = pol_skeleton, crs = crs)

    pol_skeleton_crs
  }

#' @export
cnt_skeleton.sfc <-
  function(input,
           keep = 0.5) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_sf_polygon(input))

    # Save CRS
    crs <-
      sf::st_crs(input)

    # Transform to GEOS geometry
    input_geos <-
      geos::as_geos_geometry(input)

    # Find GEOS skeleton
    pol_skeleton <-
      cnt_skeleton_geos(input = input_geos, keep = keep)

    # Transform back to sf
    pol_skeleton_crs <-
      sf::st_as_sfc(x = pol_skeleton, crs = crs)

    pol_skeleton_crs
  }

#' @export
cnt_skeleton.SpatVector <-
  function(input,
           keep = 0.5) {
    # Check if input is of class 'SpatVector' and 'polygons'
    stopifnot(check_terra_polygon(input))

    # Transform to GEOS geometry
    input_geos <-
      terra_to_geos(input)

    # Find GEOS skeleton
    pol_skeleton <-
      cnt_skeleton_geos(input = input_geos, keep = keep)

    # Transform back to SpatVect
    pol_skeleton_crs <-
      geos_to_terra(pol_skeleton)

    pol_skeleton_crs
  }

cnt_skeleton_geos <-
  function(input,
           keep = 0.5) {
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
      geos::geos_unnest(keep_multi = FALSE)

    pol_skeleton
  }
