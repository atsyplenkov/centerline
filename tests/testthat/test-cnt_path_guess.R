library(sf)
library(terra)
library(sfnetworks)

test_that(
  "cnt_path_guess handles sf, SpatVector and geos_geometry
  inputs equivalently, saving CRS",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      )
    polygon_terra <- terra::vect(polygon)
    polygon_geos <- geos::as_geos_geometry(polygon)

    # Find polygon's longest path
    result_sf <-
      cnt_path_guess(polygon)
    result_spat <-
      cnt_path_guess(polygon_terra)
    result_geos <-
      cnt_path_guess(polygon_geos)

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
    # Check CRS
    expect_true(
      wk::wk_crs(result_geos) == wk::wk_crs(polygon_geos)
    )
    expect_true(
      sf::st_crs(result_sf) == sf::st_crs(polygon)
    )
    expect_true(
      terra::crs(result_spat) == terra::crs(polygon_terra)
    )
  }
)

test_that(
  "cnt_path_guess can have inputs of various classes",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      )
    skeleton <- cnt_skeleton(polygon)
    skeleton_geos <- geos::as_geos_geometry(skeleton)
    skeleton_terra <- terra::vect(skeleton)

    # Find polygon's longest path
    result_sf <-
      cnt_path_guess(polygon, skeleton)
    result_spat <-
      cnt_path_guess(polygon, skeleton_terra)
    result_geos <-
      cnt_path_guess(polygon, skeleton_geos)

    expect_equal(
      result_sf,
      result_geos
    )
    expect_equal(
      result_sf,
      result_spat
    )
  }
)

# Test Output Structure
test_that(
  "cnt_path_guess returns a LINESTRING geometry",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      )
    polygon_sfc <- sf::st_as_sfc(polygon)
    polygon_geos <- geos::as_geos_geometry(polygon)
    polygon_terra <- terra::vect(polygon)

    # Find polygon's skeleton
    result_sf <- cnt_path_guess(polygon)
    result_sfc <- cnt_path_guess(polygon_sfc)
    result_geos <- cnt_path_guess(polygon_geos)
    result_spat <- cnt_path_guess(polygon_terra)

    # Compare results
    expect_length(result_spat, 1)
    expect_length(result_geos, 1)
    expect_length(result_sf, 1)
    expect_length(result_sfc, 1)

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

# Test simplification/densification
test_that(
  "Path guessing works with any 'keep' parameter",
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
          cnt_path_guess(polygon21, keep = x),
          error = \(e) NA
        )
      })

    # Estimate lengths
    test_lengths <-
      vapply(
        test_list, sf::st_length,
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

    # Check that all paths are not NA nor NULL nor zero
    expect_true(all(!is.na(test_list)))
    expect_true(all(!is.null(test_lengths)))
    expect_true(all(test_lengths > 0))
    expect_vector(test_lengths, ptype = double(), size = 20)
    expect_gt(length(unique(test_lengths)), 1)
  }
)
