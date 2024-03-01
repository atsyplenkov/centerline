library(testthat)
library(sf)
library(rmapshaper)

test_that("cent_skeleton returns LINESTRING geometry", {
  polygon <- sf::st_sfc(sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))))
  result <- cent_skeleton(polygon, simplify = F)
  expect_s3_class(result, c("sf", "sfc"))
  expect_true(all(sf::st_is(result, "LINESTRING")))
})

test_that("simplify parameter affects the output", {
  polygon <- sf::st_sfc(sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))))
  result_simplified <- cent_skeleton(polygon, simplify = TRUE)
  result_not_simplified <- cent_skeleton(polygon, simplify = FALSE)

  num_points_simplified <- sum(sapply(sf::st_geometry(result_simplified), function(x) length(x[[1]][[1]])))
  num_points_not_simplified <- sum(sapply(sf::st_geometry(result_not_simplified), function(x) length(x[[1]][[1]])))

  expect_true(num_points_simplified < num_points_not_simplified)
})

test_that("cent_skeleton errors on incorrect input types", {
  expect_error(cent_skeleton("not an sf object"), "Input is not of class 'sf' or 'sfc'")
})
