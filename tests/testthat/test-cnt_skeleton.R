library(testthat)
library(sf)
library(rmapshaper)
library(terra)

test_that("cnt_skeleton returns LINESTRING geometry", {
  polygon <-
    sf::st_sfc(sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE
    ))))
  result <- cnt_skeleton(polygon, keep = 1)
  expect_s3_class(result, c("sf", "sfc"))
  expect_true(all(sf::st_is(result, "LINESTRING")))
})

test_that("cnt_skeleton returns same class as input", {
  polygon_sfc <-
    sf::st_sfc(sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE
    ))))

  polygon_sf <- sf::st_as_sf(polygon_sfc)
  polygon_terra <- terra::vect(polygon_sf)

  result_sfc <- cnt_skeleton(polygon_sfc, keep = 1)
  result_sf <- cnt_skeleton(polygon_sf, keep = 1)
  result_terra <- cnt_skeleton(polygon_terra, keep = 1)

  expect_s3_class(result_sfc, c("sfc"))
  expect_s3_class(result_sf, c("sf"))
  expect_s4_class(result_terra, c("SpatVector"))
})

test_that("simplify parameter affects the output", {
  polygon <-
    sf::st_sfc(sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE
    ))))

  result_simplified <- cnt_skeleton(polygon, keep = 0.1)
  result_not_simplified <- cnt_skeleton(polygon, keep = 1)

  num_points_simplified <-
    sum(sapply(sf::st_geometry(result_simplified), function(x)
      length(x[[1]][[1]])))
  num_points_not_simplified <-
    sum(sapply(sf::st_geometry(result_not_simplified), function(x)
      length(x[[1]][[1]])))

  expect_true(num_points_simplified < num_points_not_simplified)
})

test_that("cnt_skeleton errors on incorrect input types", {
  expect_error(
    cnt_skeleton("not an sf object"),
    "Input is not of class 'SpatVector'"
  )
})
