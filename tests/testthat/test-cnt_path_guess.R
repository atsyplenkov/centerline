library(testthat)
library(sf)
library(terra)
library(centerline)
library(sfnetworks)

test_that("cnt_path_guess handles sf and SpatVector inputs equivalently", {
  polygon <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      layer = "polygon"
    )

  # Find polygon's skeleton
  result_sf <-
    cnt_path_guess(polygon)
  result_spat <-
    cnt_path_guess(terra::vect(polygon))
  # Compare results; this might involve converting one result to the other's format for direct comparison
  expect_equal(nrow(result_spat), nrow(result_sf))
})

# Test Output Structure
test_that("cnt_path_guess returns a list of correct class objects with LINESTRING geometry", {
  polygon <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      layer = "polygon"
    )

  # Find polygon's skeleton
  result <- cnt_path_guess(polygon)

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
})
