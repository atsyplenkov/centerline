test_that(
  "cnt_path handles 'terra' geometries",
  {
    skip_if_not_installed("terra")

    polygon <-
      terra::vect(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon"
      )
    start_point <-
      terra::vect(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'start'"
      )
    end_point <-
      terra::vect(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'end'"
      )

    # Find polygon's skeleton
    skeleton <- cnt_skeleton(polygon, keep = 1)

    # Two starting points
    result <-
      cnt_path(
        skeleton,
        start_point,
        end_point
      )

    # One starting point
    result_one <-
      cnt_path(
        skeleton,
        start_point[1, ],
        end_point
      )

    # Test Output Structure
    expect_length(result, 2)
    expect_length(result_one, 1)
    expect_contains(get_geom_type(result), "lines")
    expect_contains(get_geom_type(result_one), "lines")
    # Class, CRS and attributes are inherited
    expect_true(inherits(result, c("SpatVector")))
    expect_true(inherits(result_one, c("SpatVector")))
    expect_equal(terra::crs(result), terra::crs(polygon))
    expect_equal(terra::crs(result_one), terra::crs(polygon))
    expect_equal(as.data.frame(result), as.data.frame(start_point))
    expect_equal(as.data.frame(result_one), as.data.frame(start_point[1, ]))

    # Invalid inputs
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = skeleton,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = polygon,
        start_point = end_point,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = end_point,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = "not a skeleton",
        start_point = start_point,
        end_point = end_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = "start_point",
        end_point = end_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = "start_point",
        end_point = "end_point"
      )
    )
  }
)

test_that(
  "cnt_path handles 'sf' geometries",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      )
    start_point <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'start'",
        quiet = TRUE
      )
    end_point <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'end'",
        quiet = TRUE
      )

    # Find polygon's skeleton
    skeleton <- cnt_skeleton(polygon, keep = 1)

    # Two starting points
    result <-
      cnt_path(
        skeleton,
        start_point,
        end_point
      )

    # One starting point
    result_one <-
      cnt_path(
        skeleton,
        start_point[1, ],
        end_point
      )

    # Test Output Structure
    expect_equal(nrow(result), 2)
    expect_equal(nrow(result_one), 1)
    expect_contains(get_geom_type(result), "LINESTRING")
    expect_contains(get_geom_type(result_one), "LINESTRING")
    # Class, CRS and attributes are inherited
    expect_true(inherits(result, c("sf")))
    expect_true(inherits(result_one, c("sf")))
    expect_equal(sf::st_crs(result), sf::st_crs(polygon))
    expect_equal(sf::st_crs(result_one), sf::st_crs(polygon))
    expect_equal(
      sf::st_drop_geometry(result),
      sf::st_drop_geometry(start_point)
    )
    expect_equal(
      sf::st_drop_geometry(result_one),
      sf::st_drop_geometry(start_point[1, ])
    )

    # Invalid inputs
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = skeleton,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = polygon,
        start_point = end_point,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = end_point,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = "not a skeleton",
        start_point = start_point,
        end_point = end_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = "start_point",
        end_point = end_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = "start_point",
        end_point = "end_point"
      )
    )
  }
)


test_that(
  "cnt_path handles 'sfc' geometries",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      ) |>
      sf::st_as_sfc()
    start_point <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'start'",
        quiet = TRUE
      ) |>
      sf::st_as_sfc()
    end_point <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'end'",
        quiet = TRUE
      ) |>
      sf::st_as_sfc()

    # Find polygon's skeleton
    skeleton <- cnt_skeleton(polygon, keep = 1)

    # Two starting points
    result <-
      cnt_path(
        skeleton,
        start_point,
        end_point
      )

    # One starting point
    result_one <-
      cnt_path(
        skeleton,
        start_point[1],
        end_point
      )

    # Test Output Structure
    expect_equal(length(result), 2)
    expect_equal(length(result_one), 1)
    expect_contains(get_geom_type(result), "LINESTRING")
    expect_contains(get_geom_type(result_one), "LINESTRING")
    # Class, CRS and attributes are inherited
    expect_true(inherits(result, c("sfc")))
    expect_true(inherits(result_one, c("sfc")))
    expect_equal(sf::st_crs(result), sf::st_crs(polygon))
    expect_equal(sf::st_crs(result_one), sf::st_crs(polygon))

    # Invalid inputs
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = skeleton,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = polygon,
        start_point = end_point,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = end_point,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = "not a skeleton",
        start_point = start_point,
        end_point = end_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = "start_point",
        end_point = end_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = "start_point",
        end_point = "end_point"
      )
    )
  }
)


test_that(
  "cnt_path handles 'geos' geometries",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      ) |>
      geos::as_geos_geometry()
    start_point <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'start'",
        quiet = TRUE
      ) |>
      geos::as_geos_geometry()
    end_point <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'end'",
        quiet = TRUE
      ) |>
      geos::as_geos_geometry()

    # Find polygon's skeleton
    skeleton <- cnt_skeleton(polygon, keep = 1)

    # Two starting points
    result <-
      cnt_path(
        skeleton,
        start_point,
        end_point
      )

    # One starting point
    result_one <-
      cnt_path(
        skeleton,
        start_point[1],
        end_point
      )

    # Test Output Structure
    expect_equal(length(result), 2)
    expect_equal(length(result_one), 1)
    expect_contains(get_geom_type(result), "linestring")
    expect_contains(get_geom_type(result_one), "linestring")
    # Class, CRS and attributes are inherited
    expect_true(inherits(result, c("geos_geometry")))
    expect_true(inherits(result_one, c("geos_geometry")))
    expect_identical(wk::wk_crs(result), wk::wk_crs(polygon))
    expect_identical(wk::wk_crs(result_one), wk::wk_crs(polygon))

    # Invalid inputs
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = skeleton,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = polygon,
        start_point = end_point,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = end_point,
        end_point = start_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = "not a skeleton",
        start_point = start_point,
        end_point = end_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = "start_point",
        end_point = end_point
      )
    )
    expect_error(
      cnt_path(
        skeleton = skeleton,
        start_point = "start_point",
        end_point = "end_point"
      )
    )
  }
)

test_that(
  "cnt_path errors on incorrect input classes",
  {
    expect_error(
      cnt_path(
        skeleton = "not a skeleton",
        start_point = "not a start_point",
        end_point = "not an end_point"
      )
    )
  }
)
