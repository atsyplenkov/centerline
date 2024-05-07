library(testthat)
library(sf)
library(terra)
library(centerline)
library(sfnetworks)

# Test Input Validation
test_that("cnt_path errors on incorrect input classes", {
  expect_error(cnt_path("not a skeleton", "not a start_point", "not an end_point"))
})

test_that("cnt_path errors on multiple end points", {
  polygon <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      layer = "polygon"
    )

  start_point <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      query = "SELECT * FROM polygon_points WHERE type IS 'start'"
    )

  end_point <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      query = "SELECT * FROM polygon_points WHERE type IS 'end'"
    )

  # Find polygon's skeleton
  skeleton <-
    cnt_skeleton(polygon)

  expect_error(cnt_path(skeleton, end_point, start_point))
})


test_that("cnt_path handles sf and SpatVector inputs equivalently", {
  polygon <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      layer = "polygon"
    )

  start_point <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      query = "SELECT * FROM polygon_points WHERE type IS 'start'"
    )

  end_point <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      query = "SELECT * FROM polygon_points WHERE type IS 'end'"
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
  # Compare results; this might involve converting one result to the other's format for direct comparison
  expect_equal(length(result_spat), length(result_sf))
})

# Test Output Structure
test_that("cnt_path returns a list of correct class objects with LINESTRING geometry", {
  polygon <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      layer = "polygon"
    )

  start_point <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      query = "SELECT * FROM polygon_points WHERE type IS 'start'"
    )

  end_point <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      query = "SELECT * FROM polygon_points WHERE type IS 'end'"
    )

  # Find polygon's skeleton
  skeleton <-
    cnt_skeleton(polygon)

  result <- cnt_path(skeleton, start_point, end_point)
  expect_true(is.list(result))
  expect_true(all(sapply(result, function(x) {
    inherits(
      x, c("sf", "sfc", "SpatVector")
    )
  })))
  # Check for LINESTRING geometry; this check might need to be adjusted based on the object class
  expect_true(all(
    sapply(result, function(x) {
      sf::st_geometry_type(x) == "LINESTRING"
    })
  ))

  # Simple geometry
  expect_true(all(
    sapply(result, function(x) {
      sf::st_is_simple(x)
    })
  ))

  # One line per centerline
  expect_true(all(
    sapply(result, function(x) {
      nrow(x) == 1L
    })
  ))
})
