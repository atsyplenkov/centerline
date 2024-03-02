#' Create a skeleton of a closed polygon object
#'
#' @param input \code{sf}, \code{sfc} or \code{SpatVector} polygons object
#' @param simplify logical, indicating if the returning skeleton should be
#' simplified using [rmapshaper::ms_simplify()] approach
#' @param keep see below
#' @param method see below
#'
#' @inheritDotParams rmapshaper::ms_simplify
#'
#' @return An \code{sf}, \code{sfc} or \code{SpatVector} class
#' object of a \code{LINESTRING} geometry
#' @export
#'
#'
#' @examples
#' library(sf)
#'
#' polygon <-
#'   sf::st_read(system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "polygon")
#'
#' plot(polygon)
#'
#' pol_skeleton <- cnt_skeleton(polygon)
#'
#' plot(pol_skeleton)
cnt_skeleton <-
  function(input,
           simplify = TRUE,
           keep = 0.1,
           method = "dp",
           ...) {
    UseMethod("cnt_skeleton")
  }

#' @export
cnt_skeleton.sf <-
  function(input,
           simplify = TRUE,
           keep = 0.1,
           method = "dp",
           ...) {
    cnt_skeleton_sf(
      input = input,
      simplify = simplify,
      keep = keep,
      method = method,
      ...
    )
  }

#' @export
cnt_skeleton.sfc <-
  function(input,
           simplify = TRUE,
           keep = 0.1,
           method = "dp",
           ...) {
    cnt_skeleton_sf(
      input = input,
      simplify = simplify,
      keep = keep,
      method = method,
      ...
    )
  }

#' @export
cnt_skeleton.SpatVector <-
  function(input,
           simplify = TRUE,
           keep = 0.1,
           method = "dp",
           ...) {
    cnt_skeleton_terra(
      input = input,
      simplify = simplify,
      keep = keep,
      method = method,
      ...
    )
  }

#' @export
cnt_skeleton.character <-
  function(input,
           simplify = TRUE,
           keep = 0.1,
           method = "dp",
           ...) {
    cnt_skeleton_terra(
      input = input,
      simplify = simplify,
      keep = keep,
      method = method,
      ...
    )
  }

cnt_skeleton_sf <-
  function(input,
           simplify = TRUE,
           keep = 0.1,
           method = "dp",
           ...) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_sf_polygon(input))

    crs <-
      sf::st_crs(input)

    pol <- input

    if (simplify) {

      fixed_keep <-
        ifelse(keep < 0.05, 0.05, keep)

      pol <-
        rmapshaper::ms_simplify(
          pol,
          keep = fixed_keep,
          method = method,
          keep_shapes = T,
          ...
        )
    }

    pol_geos <-
      geos::as_geos_geometry(pol)

    pol_skeleton <-
      pol_geos |>
      geos::geos_voronoi_edges() |>
      geos::geos_intersection(pol_geos) |>
      geos::geos_unnest(keep_multi = F)

    pol_sf <-
      pol_skeleton |>
      sf::st_as_sf()

    pol_skeleton_crs <-
      pol_sf |>
      sf::st_set_crs(crs)

    return(pol_skeleton_crs)
  }


cnt_skeleton_terra <-
  function(input,
           simplify = TRUE,
           keep = 0.1,
           method = "dp",
           ...) {
    # Check if input is of class 'SpatVector' and 'polygons'
    stopifnot(check_terra_polygon(input))

    pol <- sf::st_as_sf(input)

    crs <-
      sf::st_crs(pol)


    if (simplify) {

      fixed_keep <-
        ifelse(keep < 0.05, 0.05, keep)

      pol <-
        rmapshaper::ms_simplify(
          pol,
          keep = fixed_keep,
          method = method,
          keep_shapes = T,
          ...
        )
    }

    pol_geos <-
      geos::as_geos_geometry(pol)

    pol_skeleton <-
      pol_geos |>
      geos::geos_voronoi_edges() |>
      geos::geos_intersection(pol_geos) |>
      geos::geos_unnest(keep_multi = F)

    pol_sf <-
      pol_skeleton |>
      sf::st_as_sf()

    pol_skeleton_crs <-
      pol_sf |>
      sf::st_set_crs(crs) |>
      terra::vect()

    return(pol_skeleton_crs)
  }
