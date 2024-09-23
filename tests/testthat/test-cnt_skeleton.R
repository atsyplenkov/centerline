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

  # Check class
  expect_s3_class(result_sfc, c("sfc"))
  expect_s3_class(result_sf, c("sf"))
  expect_s4_class(result_terra, c("SpatVector"))
  expect_s3_class(result_geos, c("geos_geometry"))

  # Check geometry types
  expect_contains(get_geom_type(result_sf), "LINESTRING")
  expect_contains(get_geom_type(result_sfc), "LINESTRING")
  expect_contains(get_geom_type(result_terra), "lines")
  expect_contains(get_geom_type(result_geos), "linestring")
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
    sum(
      vapply(
        sf::st_geometry(result_simplified),
        function(x) length(x[[1]][[1]]),
        FUN.VALUE = integer(1)
      )
    )
  num_points_not_simplified <-
    sum(
      vapply(
        sf::st_geometry(result_not_simplified),
        function(x) length(x[[1]][[1]]),
        FUN.VALUE = integer(1)
      )
    )
  num_points_densified <-
    sum(
      vapply(
        sf::st_geometry(result_densified),
        function(x) length(x[[1]][[1]]),
        FUN.VALUE = integer(1)
      )
    )

  expect_true(num_points_simplified < num_points_not_simplified)
  expect_true(num_points_not_simplified < num_points_densified)
  expect_true(num_points_simplified < num_points_densified)
})

test_that("cnt_skeleton errors on incorrect input types", {
  expect_error(
    cnt_skeleton("not an sf object")
  )
})

# Test simplification/densification
test_that(
  "cnt_skeleton works with any 'keep' parameter",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "shapes",
        quiet = TRUE
      )
    polygon$id <- seq_len(nrow(polygon))
    polygon21 <- subset(polygon, id == 21)

    # Test that all paths are created without errors
    # With keep parameter varying from 0 to 2
    test_list <-
      lapply(seq(0.1, 2, by = 0.1), function(x) {
        tryCatch(
          cnt_skeleton(polygon21, keep = x),
          error = \(e) NA
        )
      })

    # Check that all paths are not NA
    expect_true(all(!is.na(test_list)))
  }
)
