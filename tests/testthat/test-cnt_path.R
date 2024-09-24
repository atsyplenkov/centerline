library(sf)
library(terra)
library(geos)

# Test Input Validation
test_that("cnt_path errors on incorrect input classes", {
  expect_error(
    cnt_path(
      skeleton = "not a skeleton",
      start_point = "not a start_point",
      end_point = "not an end_point"
    )
  )
})

test_that("cnt_path errors on multiple end points", {
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
  skeleton <- cnt_skeleton(polygon)

  expect_error(
    cnt_path(skeleton, end_point, start_point)
  )
})

test_that(
  "cnt_path handles sf, sfc, geos_geometry and
SpatVector inputs equivalently",
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
    skeleton <-
      cnt_skeleton(polygon)

    result_sf <-
      cnt_path(skeleton, start_point, end_point)
    result_sfc <-
      cnt_path(
        sf::st_as_sfc(skeleton),
        sf::st_as_sfc(start_point),
        sf::st_as_sfc(end_point)
      )
    result_spat <-
      cnt_path(
        terra::vect(skeleton),
        terra::vect(start_point),
        terra::vect(end_point)
      )
    result_geos <-
      cnt_path(
        geos::as_geos_geometry(skeleton),
        geos::as_geos_geometry(start_point),
        geos::as_geos_geometry(end_point)
      )

    # Compare results
    expect_length(result_spat, 2)
    expect_length(result_geos, 2)
    expect_length(result_sf, 2)
    expect_length(result_sfc, 2)

    # Compare output classes
    expect_true(inherits(result_sf, c("sf")))
    expect_true(inherits(result_sfc, c("sfc")))
    expect_true(inherits(result_spat, c("SpatVector")))
    expect_true(inherits(result_geos, c("geos_geometry")))

    # Check geometry types
    expect_contains(get_geom_type(result_sf), "LINESTRING")
    expect_contains(get_geom_type(result_sfc), "LINESTRING")
    expect_contains(get_geom_type(result_spat), "lines")
    expect_contains(get_geom_type(result_geos), "linestring")
  }
)

# Test Output Structure
test_that(
  "cnt_path returns a correct class objects with LINESTRING geometry
  when there are two or more starting points",
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
    skeleton <-
      cnt_skeleton(polygon)

    result <- cnt_path(skeleton, start_point, end_point)
    expect_true(is.data.frame(result))
    expect_true(inherits(result, c("sf", "sfc", "SpatVector")))
    # Check for LINESTRING geometry;
    expect_true(all(
      sf::st_geometry_type(result) == "LINESTRING"
    ))

    # Simple geometry
    expect_true(all(
      sf::st_is_simple(result)
    ))
  }
)

test_that(
  "cnt_path returns a correct class objects with LINESTRING geometry
  when there is only one starting point",
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
      )[1, ]

    end_point <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        query = "SELECT * FROM polygon_points WHERE type IS 'end'",
        quiet = TRUE
      )

    # Find polygon's skeleton
    skeleton <-
      cnt_skeleton(polygon)

    result <- cnt_path(skeleton, start_point, end_point)
    expect_true(is.data.frame(result))
    expect_true(inherits(result, c("sf", "sfc", "SpatVector")))
    # Check for LINESTRING geometry
    expect_true(all(
      sf::st_geometry_type(result) == "LINESTRING"
    ))

    # Simple geometry
    expect_true(all(
      sf::st_is_simple(result)
    ))
  }
)
