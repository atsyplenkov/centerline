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
  # Compare results
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

# Test simplification/densification
test_that("Path guessing works with any keep parameter", {
  polygon <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      layer = "shapes"
    )
  polygon$id <- seq_len(nrow(polygon))
  polygon21 <- subset(polygon, id == 21)
  polygon_path <- cnt_path_guess(polygon21, keep = 1.6)

  # Test that all paths are created without errors
  # With keep parameter varying from 0 to 2
  test_list <-
    lapply(seq(0, 2, by = 0.1), function(x) {
      tryCatch(
        cnt_path_guess(polygon21, keep = x),
        error = \(e) NA
      )
    })

  # Check that all paths are not NA
  expect_true(all(!is.na(test_list)))
})
