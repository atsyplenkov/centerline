library(testthat)
library(sf)
library(geos)
library(terra)

test_that("cnt_skeleton returns LINESTRING geometry", {
  polygon <-
    sf::st_sfc(sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
      ncol = 2, byrow = TRUE
    ))))
  result <- cnt_skeleton(polygon, keep = 1)
  expect_s3_class(result, c("sf", "sfc"))
  expect_true(all(sf::st_is(result, "LINESTRING")))
})

test_that("cnt_skeleton returns same class as input", {
  polygon_sfc <-
    sf::st_sfc(sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
      ncol = 2, byrow = TRUE
    ))))

  polygon_sf <- sf::st_as_sf(polygon_sfc)
  polygon_terra <- terra::vect(polygon_sf)
  polygon_geos <- geos::as_geos_geometry(polygon_sf)

  result_sfc <- cnt_skeleton(polygon_sfc, keep = 1)
  result_sf <- cnt_skeleton(polygon_sf, keep = 1)
  result_terra <- cnt_skeleton(polygon_terra, keep = 1)
  result_geos <- cnt_skeleton(polygon_geos, keep = 1)

  expect_s3_class(result_sfc, c("sfc"))
  expect_s3_class(result_sf, c("sf"))
  expect_s4_class(result_terra, c("SpatVector"))
  expect_s3_class(result_geos, c("geos_geometry"))
})

test_that("'keep' parameter affects the output", {
  polygon <-
    sf::st_read(
      system.file("extdata/example.gpkg", package = "centerline"),
      layer = "polygon",
      quiet = TRUE
    )

  result_simplified <- cnt_skeleton(polygon, keep = 0.1)
  result_not_simplified <- cnt_skeleton(polygon, keep = 1)
  result_densified <- cnt_skeleton(polygon, keep = 1.1)

  num_points_simplified <-
    sum(sapply(sf::st_geometry(result_simplified), function(x) {
      length(x[[1]][[1]])
    }))
  num_points_not_simplified <-
    sum(sapply(sf::st_geometry(result_not_simplified), function(x) {
      length(x[[1]][[1]])
    }))
  num_points_densified <-
    sum(sapply(sf::st_geometry(result_densified), function(x) {
      length(x[[1]][[1]])
    }))

  expect_true(num_points_simplified < num_points_not_simplified)
  expect_true(num_points_not_simplified < num_points_densified)
  expect_true(num_points_simplified < num_points_densified)
})

test_that("cnt_skeleton errors on incorrect input types", {
  expect_error(
    cnt_skeleton("not an sf object")
  )
})
