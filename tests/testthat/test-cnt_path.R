library(testthat)
library(sf)
library(terra)
# library(centerline)
library(sfnetworks)

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


test_that("cnt_path handles sf and SpatVector inputs equivalently", {
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
  result_spat <-
    cnt_path(
      terra::vect(skeleton),
      terra::vect(start_point),
      terra::vect(end_point)
    )
  # Compare results
  expect_equal(length(result_spat), length(result_sf))
})

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
