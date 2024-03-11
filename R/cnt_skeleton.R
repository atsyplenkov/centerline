#' Create a skeleton of a closed polygon object
#'
#' @param input \code{sf}, \code{sfc} or \code{SpatVector} polygons object
#' @param keep numeric, proportion of points to retain (0.05-Inf; default 1).
#' See Details.
#'
#' @details
#' - If \code{keep} equals 1 (default), no transformation will occur. The
#' function will use the original geometry to find the skeleton.
#'
#' - If the keep parameter is below 1, then the [rmapshaper::ms_simplify()]
#' function will be used with Douglas-Peuker algorithm. So the original input
#' geometry would be simplified, and the resulting skeleton will be cleaner but
#' maybe more edgy.
#'
#' - If the \code{keep} is above 1, then the densification
#' algorithm is applied using the [geos::geos_densify()] function. This may
#'  produce a very large object if keep is set more than 2. However, the
#'  resulting skeleton would potentially be more accurate.
#'
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
           keep = 0.1) {
    UseMethod("cnt_skeleton")
  }

#' @export
cnt_skeleton.sf <-
  function(input,
           keep = 0.1) {
    cnt_skeleton_sf(
      input = input,
      keep = keep
    )
  }

#' @export
cnt_skeleton.sfc <-
  function(input,
           keep = 0.1) {
    cnt_skeleton_sfc(
      input = input,
      keep = keep
    )
  }

#' @export
cnt_skeleton.SpatVector <-
  function(input,
           keep = 0.1) {
    cnt_skeleton_terra(
      input = input,
      keep = keep
    )
  }

#' @export
cnt_skeleton.character <-
  function(input,
           keep = 0.1) {
    cnt_skeleton_terra(
      input = input,
      keep = keep
    )
  }


cnt_skeleton_sfc <-
  function(input,
           keep = 0.1) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_sf_polygon(input))

    # Save CRS
    crs <-
      sf::st_crs(input)

    # Transform to GEOS geometry
    input_geos <-
      input |>
      geos::as_geos_geometry()

    # Simplify or densify or do nothing
    if (keep == 1) {

      pol_geos <- input_geos

    } else if (keep < 1 & keep >= 0.05) {

      pol_geos <-
        rmapshaper::ms_simplify(
          input,
          keep = keep,
          method = "dp",
          keep_shapes = TRUE
        ) |>
        geos::as_geos_geometry()

    } else if (keep < 0.05) {

      pol_geos <-
        rmapshaper::ms_simplify(
          input,
          keep = 0.05,
          method = "dp",
          keep_shapes = TRUE
        ) |>
        geos::as_geos_geometry()

    } else {

      perimeter_length <-
        geos::geos_length(input_geos)

      point_count <-
        input_geos |>
        geos::geos_unique_points() |>
        geos::geos_num_coordinates()

      point_density <-
        perimeter_length / point_count

      pol_geos <-
        geos::geos_densify(
          input_geos,
          tolerance = point_density / keep
        )

    }

    # Find polygon skeleton
    pol_skeleton <-
      pol_geos |>
      geos::geos_voronoi_edges() |>
      geos::geos_intersection(pol_geos) |>
      geos::geos_unnest(keep_multi = F)

    # Transform back to sf
    pol_skeleton_crs <-
      pol_skeleton |>
      sf::st_as_sfc(crs = crs)

    return(pol_skeleton_crs)
  }

cnt_skeleton_sf <-
  function(input,
           keep = 0.1) {
    # Check if input is of class 'sf' or 'sfc' and 'POLYGON'
    stopifnot(check_sf_polygon(input))

    # Save CRS
    crs <-
      sf::st_crs(input)

    # Transform to GEOS geometry
    input_geos <-
      input |>
      geos::as_geos_geometry()

    # Simplify or densify or do nothing
    if (keep == 1) {

      pol_geos <- input_geos

    } else if (keep < 1 & keep >= 0.05) {

      pol_geos <-
        rmapshaper::ms_simplify(
          input,
          keep = keep,
          method = "dp",
          keep_shapes = TRUE
        ) |>
        geos::as_geos_geometry()

    } else if (keep < 0.05) {

      pol_geos <-
        rmapshaper::ms_simplify(
          input,
          keep = 0.05,
          method = "dp",
          keep_shapes = TRUE
        ) |>
        geos::as_geos_geometry()

    } else {

      perimeter_length <-
        geos::geos_length(input_geos)

      point_count <-
        input_geos |>
        geos::geos_unique_points() |>
        geos::geos_num_coordinates()

      point_density <-
        perimeter_length / point_count

      pol_geos <-
        geos::geos_densify(
          input_geos,
          tolerance = point_density / keep
        )

    }

    # Find polygon skeleton
    pol_skeleton <-
      pol_geos |>
      geos::geos_voronoi_edges() |>
      geos::geos_intersection(pol_geos) |>
      geos::geos_unnest(keep_multi = F)

    # Transform back to sf
    pol_skeleton_crs <-
      pol_skeleton |>
      sf::st_as_sf(crs = crs) ##|>
      # cbind(sf::st_drop_geometry(input))

    return(pol_skeleton_crs)
  }

cnt_skeleton_terra <-
  function(input,
           keep = 0.1) {

    # Check if input is of class 'SpatVector' and 'polygons'
    stopifnot(check_terra_polygon(input))

    # Save CRS
    crs <-
      terra::crs(input)

    # Transform to GEOS geometry
    input_geos <-
      input |>
      terra::geom(wkt = T) |>
      geos::as_geos_geometry()

    # Simplify or densify or do nothing
    if (keep == 1) {

      pol_geos <- input_geos

    } else if (keep < 1 & keep >= 0.05) {

      pol_geos <-
        rmapshaper::ms_simplify(
          sf::st_as_sf(input_geos),
          keep = keep,
          method = "dp",
          keep_shapes = TRUE
        ) |>
        geos::as_geos_geometry()

    } else if (keep < 0.05) {

      pol_geos <-
        rmapshaper::ms_simplify(
          sf::st_as_sf(input_geos),
          keep = 0.05,
          method = "dp",
          keep_shapes = TRUE
        ) |>
        geos::as_geos_geometry()

    } else {

      perimeter_length <-
        geos::geos_length(input_geos)

      point_count <-
        input_geos |>
        geos::geos_unique_points() |>
        geos::geos_num_coordinates()

      point_density <-
        perimeter_length / point_count

      pol_geos <-
        geos::geos_densify(
          input_geos,
          tolerance = point_density / keep
        )

    }

    # Find skeleton
    pol_skeleton <-
      pol_geos |>
      geos::geos_voronoi_edges() |>
      geos::geos_intersection(pol_geos) |>
      geos::geos_unnest(keep_multi = F)

    # Transform back to SpatVect
    pol_skeleton_crs <-
      pol_skeleton |>
      wk::as_wkt() |>
      as.character() |>
      terra::vect(crs = crs)
      # cbind(terra::as.data.frame(input))

    return(pol_skeleton_crs)
  }
