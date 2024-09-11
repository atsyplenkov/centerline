library(testthat)
library(sf)
library(terra)
library(centerline)
library(sfnetworks)

test_that(
  "cnt_path_guess handles sf, SpatVector and geos_geometry
  inputs equivalently",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      )

    # Find polygon's skeleton
    result_sf <-
      cnt_path_guess(polygon)
    result_spat <-
      cnt_path_guess(terra::vect(polygon))
    result_geos <-
      cnt_path_guess(geos::as_geos_geometry(polygon))

    # Find centerline lengths
    result_length <-
      list(result_geos, result_sf, result_spat) |>
      lapply(sf::st_as_sf) |>
      lapply(geos::as_geos_geometry) |>
      lapply(geos::geos_length) |>
      unlist()

    # Compare lengths
    expect_true(
      round(mean(result_length), 3) == round(result_length[1], 3)
    )
    # Check class
    expect_true(
      inherits(result_geos, c("geos_geometry"))
    )
    expect_true(
      inherits(result_sf, c("sf"))
    )
    expect_true(
      inherits(result_spat, c("SpatVector"))
    )
  }
)

# Test Output Structure
test_that(
  "cnt_path_guess returns a list of correct class objects
  with LINESTRING geometry",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      )

    # Find polygon's skeleton
    result <- cnt_path_guess(polygon)

    expect_true(is.list(result))
    expect_true(all(vapply(result, function(x) {
      inherits(
        x, c("sf", "sfc", "SpatVector")
      )
    }, logical(1))))
    # Check for LINESTRING geometry
    expect_true(all(
      vapply(result, function(x) {
        sf::st_geometry_type(x) == "LINESTRING"
      }, logical(1))
    ))
  }
)

# Test simplification/densification
test_that(
  "Path guessing works with any keep parameter",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "shapes",
        quiet = TRUE
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
  }
)
