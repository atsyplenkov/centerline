#' Plot centerline with ggplot2
#'
#' @description Binding for [ggplot2::geom_sf()], therefore it supports
#' only `sf` objects.
#'
#' @param simplify logical, if \code{TRUE} (default) then the
#' centerline will be smoothed with [smoothr::smooth_ksmooth()]
#'
#' @inheritParams cnt_skeleton
#' @inheritParams ggplot2::geom_sf
#'
#' @inheritSection ggplot2::geom_sf CRS
#' @inheritSection ggplot2::geom_sf Combining sf layers and regular geoms
#'
#' @seealso [geom_cnt_text()], [geom_cnt_label()], [ggplot2::geom_sf()]
#'
#' @return A `Layer` ggproto object that can be added to a plot.
#'
#' @export
#'
#' @examplesIf requireNamespace("geomtextpath", quietly = TRUE)
#' library(sf)
#' library(ggplot2)
#'
#' lake <-
#'   sf::st_read(
#'     system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "lake",
#'     quiet = TRUE
#'   )
#'
#' ggplot() +
#'   geom_sf(data = lake) +
#'   geom_cnt(
#'     data = lake,
#'     keep = 1,
#'     simplify = TRUE
#'   ) +
#'   theme_void()
geom_cnt <-
  function(
    mapping = ggplot2::aes(),
    data = NULL,
    stat = "sf",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    keep = 0.5,
    method = c("voronoi", "straight"),
    simplify = TRUE,
    ...
  ) {
    check_package("ggplot2")

    # Check if the input data is an sf object
    checkmate::assert_class(data, "sf")
    check_polygons(data)
    checkmate::assert_logical(simplify)

    data_centerline_geos <-
      cnt_path_guess(
        input = data,
        keep = keep,
        method = method,
        return_geos = TRUE
      )

    if (simplify) {
      check_package("smoothr")
      data_centerline <-
        geos_ksmooth(data_centerline_geos) |>
        sf::st_as_sf() |>
        cbind(sf::st_drop_geometry(data))
    } else {
      data_centerline <-
        sf::st_as_sf(data_centerline_geos) |>
        cbind(sf::st_drop_geometry(data))
    }

    ggplot2::geom_sf(
      mapping = mapping,
      data = data_centerline,
      stat = stat,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  }

#' Plot label or text on centerline with ggplot2
#' @rdname geom_cnt_text
#'
#' @description Binding for [geomtextpath::geom_textsf()] and
#' [geomtextpath::geom_labelsf()]
#'
#' @param simplify logical, if \code{TRUE} (default) then the
#' centerline will be smoothed with [smoothr::smooth_ksmooth()]
#'
#' @inheritParams cnt_skeleton
#' @inheritParams geomtextpath::geom_textsf
#'
#' @details
#' ## Aesthetics
#' \code{geom_cnt_text()} understands the following aesthetics:
#' \itemize{
#'   \item `x`
#'   \item `y`
#'   \item `label`
#'   \item `alpha`
#'   \item `angle`
#'   \item `colour`
#'   \item `family`
#'   \item `fontface`
#'   \item `group`
#'   \item `hjust`
#'   \item `linecolour`
#'   \item `lineheight`
#'   \item `linetype`
#'   \item `linewidth`
#'   \item `size`
#'   \item `spacing`
#'   \item `textcolour`
#'   \item `vjust`
#' }
#'
#' In addition to aforementioned aesthetics, \code{geom_cnt_label()} also
#' understands:
#' \itemize{
#'   \item `boxcolour`
#'   \item `boxlinetype`
#'   \item `boxlinewidth`
#'   \item `fill`
#' }
#'
#' @seealso [geom_cnt()], [geomtextpath::geom_textsf()],
#' [geomtextpath::geom_labelsf()], [ggplot2::geom_sf()]
#'
#' @export
#'
#' @examplesIf requireNamespace("geomtextpath", quietly = TRUE)
#' library(sf)
#' library(ggplot2)
#'
#' lake <-
#'   sf::st_read(
#'     system.file("extdata/example.gpkg", package = "centerline"),
#'     layer = "lake",
#'     quiet = TRUE
#'   )
#'
#' # Plot centerline and lake name as text
#' ggplot() +
#'   geom_sf(data = lake) +
#'   geom_cnt_text(
#'     data = lake,
#'     aes(label = "Lake Ohau"),
#'     size = 8,
#'     simplify = TRUE
#'   ) +
#' theme_void()
#'
#' # Plot lake name as label
#' ggplot() +
#'   geom_sf(data = lake) +
#'   geom_cnt_label(
#'     data = lake,
#'     aes(label = "Lake Ohau"),
#'     linecolor = NA, # disable line drawing
#'     size = 10,
#'     method = "s",
#'     simplify = TRUE
#'   ) +
#' theme_void()
#'
geom_cnt_text <-
  function(
    mapping = ggplot2::aes(),
    data = NULL,
    stat = "sf",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    keep = 0.5,
    method = c("voronoi", "straight"),
    simplify = TRUE,
    ...
  ) {
    check_package("geomtextpath")

    # Check if the input data is an sf object
    checkmate::assert_class(data, "sf")
    check_polygons(data)
    checkmate::assert_logical(simplify)

    data_centerline_geos <-
      cnt_path_guess(
        input = data,
        keep = keep,
        method = method,
        return_geos = TRUE
      )

    if (simplify) {
      check_package("smoothr")
      data_centerline <-
        geos_ksmooth(data_centerline_geos) |>
        sf::st_as_sf() |>
        cbind(sf::st_drop_geometry(data))
    } else {
      data_centerline <-
        sf::st_as_sf(data_centerline_geos) |>
        cbind(sf::st_drop_geometry(data))
    }

    # Call geomtextpath::geom_textsf() with the transformed data
    geomtextpath::geom_textsf(
      mapping = mapping,
      data = data_centerline,
      stat = stat,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  }


#' @export
#' @rdname geom_cnt_text
#' @inheritParams cnt_skeleton
geom_cnt_label <-
  function(
    mapping = ggplot2::aes(),
    data = NULL,
    stat = "sf",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    keep = 0.5,
    method = c("voronoi", "straight"),
    simplify = TRUE,
    ...
  ) {
    check_package("geomtextpath")

    # Check if the input data is an sf object
    checkmate::assert_class(data, "sf")
    check_polygons(data)
    checkmate::assert_logical(simplify)

    data_centerline_geos <-
      cnt_path_guess(
        input = data,
        keep = keep,
        method = method,
        return_geos = TRUE
      )

    if (simplify) {
      check_package("smoothr")
      data_centerline <-
        geos_ksmooth(data_centerline_geos) |>
        sf::st_as_sf() |>
        cbind(sf::st_drop_geometry(data))
    } else {
      data_centerline <-
        sf::st_as_sf(data_centerline_geos) |>
        cbind(sf::st_drop_geometry(data))
    }

    # Call geomtextpath::geom_textsf() with the transformed data
    geomtextpath::geom_labelsf(
      mapping = mapping,
      data = data_centerline,
      stat = stat,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  }
