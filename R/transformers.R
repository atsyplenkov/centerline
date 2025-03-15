# Inter-class transformers -----------------------------------------------
# Terra to SF transformer
# This function is five time faster than
# st::st_as_sf() due to {wk} package
terra_to_sf <-
  function(input) {
    spatial_data <-
      terra::as.data.frame(input)

    if (length(spatial_data) == 0) {
      terra::geom(input, wk = TRUE) |>
        wk::as_wkt() |>
        sf::st_as_sf() |>
        sf::st_set_crs(terra::crs(input))
    } else {
      terra::geom(input, wk = TRUE) |>
        wk::as_wkt() |>
        sf::st_as_sf() |>
        sf::st_set_crs(terra::crs(input)) |>
        cbind(terra::as.data.frame(spatial_data))
    }
  }

# Terra to GEOS transformer
terra_to_geos <-
  function(input) {
    input |>
      sf::st_as_sf() |>
      geos::as_geos_geometry()
    # input |>
    #   terra::geom(wkt = TRUE) |>
    #   geos::as_geos_geometry(crs = sf::st_crs(input))
  }

# GEOS to terra transformer
geos_to_terra <-
  function(input) {
    wk_input <- wk::as_wkt(input)

    terra::vect(
      as.character(wk_input),
      crs = wk::wk_crs(wk_input)$wkt
    )
  }

# geos_geometry polygon to matrix of coordinates
geos_to_matrix <-
  function(geos_obj) {
    coords <- wk::wk_coords(geos_obj)
    matrix(c(coords$x, coords$y), ncol = 2)
  }
