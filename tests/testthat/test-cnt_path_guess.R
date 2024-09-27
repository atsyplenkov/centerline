# 91 POLYGONS
shapes_sf <-
  sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "shapes",
    quiet = TRUE
  )
shapes_sf$id <- seq_len(nrow(shapes_sf))

# One POLYGON
polygon_sf <- subset(shapes_sf, id == 89)
skeleton_sf <- cnt_skeleton(polygon_sf, keep = 1)

# One MULTIPOLYGON
multipolygon_sf <-
  sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "lake_island",
    quiet = TRUE
  )


test_that(
  "cnt_path_guess works with 'sf' geometries",
  {
    polygon <- polygon_sf
    skeleton <- skeleton_sf
    result <- cnt_path_guess(polygon, keep = 1)

    ## Check classes and CRS
    expect_s3_class(result, c("sf"))
    expect_contains(get_geom_type(result), "LINESTRING")
    expect_equal(wk::wk_crs(result), wk::wk_crs(polygon))

    ## Check centerline lengths
    expect_equal(nrow(result), 1)
    expect_equal(
      as.numeric(round(sf::st_length(result), 5)),
      50.95459
    )

    # Various input classes are OK
    result2 <- cnt_path_guess(polygon, skeleton_sf)

    expect_s3_class(result2, c("sf"))
    expect_contains(get_geom_type(result2), "LINESTRING")
    expect_equal(wk::wk_crs(result2), wk::wk_crs(polygon))
    expect_equal(result, result2)

    # Path guessing should work with any 'keep' parameter
    keep_list <-
      lapply(seq(0.1, 2, by = 0.2), function(x) {
        tryCatch(
          cnt_path_guess(polygon, keep = x),
          error = \(e) NA
        )
      })

    ## Estimate lengths
    keep_lengths <-
      vapply(
        keep_list, sf::st_length,
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

    ## Check that all paths are not NA nor NULL nor zero
    expect_true(all(!is.na(keep_list)))
    expect_true(all(!is.null(keep_lengths)))
    expect_true(all(keep_lengths > 0))
    expect_vector(keep_lengths, ptype = double(), size = 10)
    expect_gt(length(unique(keep_lengths)), 1)

    # Several POLYGON objects are handled correctly
    shapes <- shapes_sf
    shapes_centerline <- cnt_path_guess(shapes)

    expect_s3_class(shapes_centerline, c("sf"))
    expect_contains(get_geom_type(shapes_centerline), "LINESTRING")
    expect_equal(wk::wk_crs(shapes_centerline), wk::wk_crs(shapes))
    expect_equal(
      nrow(shapes_centerline),
      nrow(shapes)
    )
    expect_identical(
      sf::st_drop_geometry(shapes_centerline),
      sf::st_drop_geometry(shapes)
    )

    # MULTIPOLYGON objects are handled correctly
    multipolygon <- multipolygon_sf
    multipolygon_centerline <- cnt_path_guess(multipolygon)

    expect_s3_class(multipolygon_centerline, c("sf"))
    expect_contains(get_geom_type(multipolygon_centerline), "LINESTRING")
    expect_equal(
      wk::wk_crs(multipolygon_centerline),
      wk::wk_crs(multipolygon)
    )
    expect_equal(nrow(multipolygon_centerline), 8L)
    expect_identical(
      sf::st_drop_geometry(multipolygon_centerline)[1, ],
      sf::st_drop_geometry(multipolygon)
    )

    # Incorrect inputs
    expect_error(
      cnt_path_guess(
        input = polygon,
        skeleton = polygon
      )
    )
    expect_error(
      cnt_path_guess(
        input = skeleton
      )
    )
    expect_error(
      cnt_path_guess(
        skeleton = polygon
      )
    )
  }
)

test_that(
  "cnt_path_guess works with 'sfc' geometries",
  {
    polygon <- sf::st_as_sfc(polygon_sf)
    skeleton <- sf::st_as_sfc(skeleton_sf)
    result <- cnt_path_guess(polygon, keep = 1)

    ## Check classes and CRS
    expect_s3_class(result, c("sfc"))
    expect_contains(get_geom_type(result), "LINESTRING")
    expect_equal(wk::wk_crs(result), wk::wk_crs(polygon))

    ## Check centerline lengths
    expect_length(result, 1)
    expect_equal(
      as.numeric(round(sf::st_length(result), 5)),
      50.95459
    )

    # Various input classes are OK
    result2 <- cnt_path_guess(polygon, skeleton_sf)

    expect_s3_class(result2, c("sfc"))
    expect_contains(get_geom_type(result2), "LINESTRING")
    expect_equal(wk::wk_crs(result2), wk::wk_crs(polygon))
    expect_equal(
      sf::st_as_sf(result),
      sf::st_as_sf(result2)
    )

    # Path guessing should work with any 'keep' parameter
    keep_list <-
      lapply(seq(0.1, 2, by = 0.2), function(x) {
        tryCatch(
          cnt_path_guess(polygon, keep = x),
          error = \(e) NA
        )
      })

    ## Estimate lengths
    keep_lengths <-
      vapply(
        keep_list, sf::st_length,
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

    ## Check that all paths are not NA nor NULL nor zero
    expect_true(all(!is.na(keep_list)))
    expect_true(all(!is.null(keep_lengths)))
    expect_true(all(keep_lengths > 0))
    expect_vector(keep_lengths, ptype = double(), size = 10)
    expect_gt(length(unique(keep_lengths)), 1)

    # Several POLYGON objects are handled correctly
    shapes <- sf::st_as_sfc(shapes_sf)
    shapes_centerline <- cnt_path_guess(shapes)

    expect_s3_class(shapes_centerline, c("sfc"))
    expect_contains(get_geom_type(shapes_centerline), "LINESTRING")
    expect_equal(wk::wk_crs(shapes_centerline), wk::wk_crs(shapes))
    expect_equal(
      length(shapes_centerline),
      length(shapes)
    )

    # MULTIPOLYGON objects are handled correctly
    multipolygon <- sf::st_as_sfc(multipolygon_sf)
    multipolygon_centerline <- cnt_path_guess(multipolygon)

    expect_s3_class(multipolygon_centerline, c("sfc"))
    expect_contains(get_geom_type(multipolygon_centerline), "LINESTRING")
    expect_equal(
      wk::wk_crs(multipolygon_centerline),
      wk::wk_crs(multipolygon)
    )
    expect_equal(length(multipolygon_centerline), 8L)

    # Incorrect inputs
    expect_error(
      cnt_path_guess(
        input = polygon,
        skeleton = polygon
      )
    )
    expect_error(
      cnt_path_guess(
        input = skeleton
      )
    )
    expect_error(
      cnt_path_guess(
        skeleton = polygon
      )
    )
  }
)

test_that(
  "cnt_path_guess works with 'geos' geometries",
  {
    polygon <- geos::as_geos_geometry(polygon_sf)
    skeleton <- geos::as_geos_geometry(skeleton_sf)
    result <- cnt_path_guess(polygon, keep = 1)

    ## Check classes and CRS
    expect_s3_class(result, c("geos_geometry"))
    expect_contains(get_geom_type(result), "linestring")
    expect_equal(wk::wk_crs(result), wk::wk_crs(polygon))

    ## Check centerline lengths
    expect_length(result, 1)
    expect_equal(
      round(geos::geos_length(result), 5),
      50.95459
    )

    # Various input classes are OK
    result2 <- cnt_path_guess(polygon, skeleton_sf)

    expect_s3_class(result2, c("geos_geometry"))
    expect_contains(get_geom_type(result2), "linestring")
    expect_equal(wk::wk_crs(result2), wk::wk_crs(polygon))
    expect_equal(
      sf::st_as_sf(result),
      sf::st_as_sf(result2)
    )

    # Path guessing should work with any 'keep' parameter
    keep_list <-
      lapply(seq(0.1, 2, by = 0.2), function(x) {
        tryCatch(
          cnt_path_guess(polygon, keep = x),
          error = \(e) NA
        )
      })

    ## Estimate lengths
    keep_lengths <-
      vapply(
        keep_list, geos::geos_length,
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

    ## Check that all paths are not NA nor NULL nor zero
    expect_true(all(!is.na(keep_list)))
    expect_true(all(!is.null(keep_lengths)))
    expect_true(all(keep_lengths > 0))
    expect_vector(keep_lengths, ptype = double(), size = 10)
    expect_gt(length(unique(keep_lengths)), 1)

    # Several POLYGON objects are handled correctly
    shapes <- geos::as_geos_geometry(shapes_sf)
    shapes_centerline <- cnt_path_guess(shapes)

    expect_s3_class(shapes_centerline, c("geos_geometry"))
    expect_contains(get_geom_type(shapes_centerline), "linestring")
    expect_equal(wk::wk_crs(shapes_centerline), wk::wk_crs(shapes))
    expect_equal(
      length(shapes_centerline),
      length(shapes)
    )

    # MULTIPOLYGON objects are handled correctly
    multipolygon <- geos::as_geos_geometry(multipolygon_sf)
    multipolygon_centerline <- cnt_path_guess(multipolygon)

    expect_s3_class(multipolygon_centerline, c("geos_geometry"))
    expect_contains(get_geom_type(multipolygon_centerline), "linestring")
    expect_equal(
      wk::wk_crs(multipolygon_centerline),
      wk::wk_crs(multipolygon)
    )
    expect_equal(length(multipolygon_centerline), 8L)

    # Incorrect inputs
    expect_error(
      cnt_path_guess(
        input = polygon,
        skeleton = polygon
      )
    )
    expect_error(
      cnt_path_guess(
        input = skeleton
      )
    )
    expect_error(
      cnt_path_guess(
        skeleton = polygon
      )
    )
  }
)


test_that(
  "cnt_path_guess works with 'terra' geometries",
  {
    skip_if_not_installed("terra")

    polygon <- terra::vect(polygon_sf)
    skeleton <- terra::vect(skeleton_sf)
    result <- cnt_path_guess(polygon, keep = 1)

    ## Check classes and CRS
    expect_s4_class(result, c("SpatVector"))
    expect_contains(get_geom_type(result), "lines")
    expect_equal(terra::crs(result), terra::crs(polygon))

    ## Check centerline lengths
    expect_length(result, 1)
    expect_equal(
      round(terra::perim(result), 5),
      50.95459
    )

    # Various input classes are OK
    result2 <- cnt_path_guess(polygon, skeleton_sf)

    expect_s4_class(result2, c("SpatVector"))
    expect_contains(get_geom_type(result2), "lines")
    expect_equal(terra::crs(result2), terra::crs(polygon))
    expect_equal(
      sf::st_as_sf(result),
      sf::st_as_sf(result2)
    )

    # Path guessing should work with any 'keep' parameter
    keep_list <-
      lapply(seq(0.1, 2, by = 0.2), function(x) {
        tryCatch(
          cnt_path_guess(polygon, keep = x),
          error = \(e) NA
        )
      })

    ## Estimate lengths
    keep_lengths <-
      vapply(
        keep_list, terra::perim,
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

    ## Check that all paths are not NA nor NULL nor zero
    expect_true(all(!is.na(keep_list)))
    expect_true(all(!is.null(keep_lengths)))
    expect_true(all(keep_lengths > 0))
    expect_vector(keep_lengths, ptype = double(), size = 10)
    expect_gt(length(unique(keep_lengths)), 1)

    # Several POLYGON objects are handled correctly
    shapes <- terra::vect(shapes_sf)
    shapes_centerline <- cnt_path_guess(shapes)

    expect_s4_class(shapes_centerline, c("SpatVector"))
    expect_contains(get_geom_type(shapes_centerline), "lines")
    expect_equal(terra::crs(shapes_centerline), terra::crs(shapes))
    expect_equal(
      nrow(shapes_centerline),
      nrow(shapes)
    )
    expect_identical(
      terra::as.data.frame(shapes_centerline),
      terra::as.data.frame(shapes)
    )

    # MULTIPOLYGON objects are handled correctly
    multipolygon <- terra::vect(multipolygon_sf)
    multipolygon_centerline <- cnt_path_guess(multipolygon)

    expect_s4_class(multipolygon_centerline, c("SpatVector"))
    expect_contains(get_geom_type(multipolygon_centerline), "lines")
    expect_equal(
      terra::crs(multipolygon_centerline),
      terra::crs(multipolygon)
    )
    expect_equal(nrow(multipolygon_centerline), 8L)
    expect_identical(
      terra::as.data.frame(multipolygon_centerline)[1, ],
      terra::as.data.frame(multipolygon)
    )

    # Incorrect inputs
    expect_error(
      cnt_path_guess(
        input = polygon,
        skeleton = polygon
      )
    )
    expect_error(
      cnt_path_guess(
        input = skeleton
      )
    )
    expect_error(
      cnt_path_guess(
        skeleton = polygon
      )
    )
  }
)


test_that(
  "cnt_path_guess errors on incorrect input classes",
  {
    expect_error(
      cnt_path_guess(
        input = "polygon_sf"
      )
    )
    expect_error(
      cnt_path_guess(
        input = 1
      )
    )
    expect_error(
      cnt_path_guess(
        input = "polygon_sfc",
        skeleton = polygon_sf
      )
    )
  }
)
