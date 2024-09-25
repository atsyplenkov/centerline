library(sf)
library(terra)
library(geos)

polygon_sf <-
  sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon",
    quiet = TRUE
  )
polygon_sfc <- sf::st_as_sfc(polygon_sf)
polygon_geos <- geos::as_geos_geometry(polygon_sf)
polygon_terra <- terra::vect(polygon_sf)

skeleton_sf <- cnt_skeleton(polygon_sf)
skeleton_sfc <- sf::st_as_sfc(skeleton_sf)
skeleton_geos <- geos::as_geos_geometry(skeleton_sf)
skeleton_terra <- terra::vect(skeleton_sf)

test_that("cnt_path_guess errors on incorrect input classes", {
  # sf
  expect_error(
    cnt_path_guess(
      input = polygon_sf,
      skeleton = polygon_sf
    )
  )
  expect_error(
    cnt_path_guess(
      input = skeleton_sf
    )
  )

  # sfc
  expect_error(
    cnt_path_guess(
      input = polygon_sfc,
      skeleton = polygon_sfc
    )
  )
  expect_error(
    cnt_path_guess(
      input = skeleton_sfc
    )
  )

  # terra
  expect_error(
    cnt_path_guess(
      input = polygon_terra,
      skeleton = polygon_terra
    )
  )
  expect_error(
    cnt_path_guess(
      input = skeleton_terra
    )
  )

  # geos
  expect_error(
    cnt_path_guess(
      input = polygon_geos,
      skeleton = polygon_geos
    )
  )
  expect_error(
    cnt_path_guess(
      input = skeleton_geos
    )
  )
})

test_that(
  "cnt_path_guess handles sf, SpatVector and geos_geometry
  inputs equivalently, saving CRS",
  {
    # Find polygon's longest path
    result_sf <-
      cnt_path_guess(polygon_sf)
    result_terra <-
      cnt_path_guess(polygon_terra)
    result_geos <-
      cnt_path_guess(polygon_geos)
    result_sfc <-
      cnt_path_guess(polygon_sfc)

    # Find centerline lengths
    result_length <-
      list(result_geos, result_sf, result_terra, result_sfc) |>
      lapply(sf::st_as_sf) |>
      lapply(geos::as_geos_geometry) |>
      lapply(geos::geos_length) |>
      unlist()

    # Compare lengths
    expect_true(all(result_length >= result_length[1]))
    # Check class
    expect_s3_class(result_sfc, c("sfc"))
    expect_s3_class(result_sf, c("sf"))
    expect_s4_class(result_terra, c("SpatVector"))
    expect_s3_class(result_geos, c("geos_geometry"))
    # Check CRS
    expect_true(
      wk::wk_crs(result_geos) == wk::wk_crs(polygon_geos)
    )
    expect_true(
      sf::st_crs(result_sf) == sf::st_crs(polygon_sf)
    )
    expect_true(
      sf::st_crs(result_sfc) == sf::st_crs(polygon_sfc)
    )
    expect_true(
      terra::crs(result_terra) == terra::crs(polygon_terra)
    )
  }
)

test_that(
  "cnt_path_guess can have inputs of various classes",
  {
    # Find polygon's longest path
    result_sf <-
      cnt_path_guess(polygon_sf, skeleton_sf)
    result_sfc <-
      cnt_path_guess(polygon_sf, skeleton_sfc)
    result_terra <-
      cnt_path_guess(polygon_sf, skeleton_terra)
    result_geos <-
      cnt_path_guess(polygon_sf, skeleton_geos)

    expect_equal(
      result_sf,
      result_geos
    )
    expect_equal(
      result_sf,
      result_terra
    )
    expect_equal(
      result_sf,
      result_sfc
    )
  }
)

test_that(
  "cnt_path_guess returns a LINESTRING geometry",
  {
    # Find polygon's skeleton
    result_sf <- cnt_path_guess(polygon_sf)
    result_sfc <- cnt_path_guess(polygon_sfc)
    result_geos <- cnt_path_guess(polygon_geos)
    result_terra <- cnt_path_guess(polygon_terra)

    # Compare results
    expect_length(result_terra, 1)
    expect_length(result_geos, 1)
    expect_length(result_sf, 1)
    expect_length(result_sfc, 1)

    # Compare output classes
    expect_true(inherits(result_sf, c("sf")))
    expect_true(inherits(result_sfc, c("sfc")))
    expect_true(inherits(result_terra, c("SpatVector")))
    expect_true(inherits(result_geos, c("geos_geometry")))

    # Check geometry types
    expect_contains(get_geom_type(result_sf), "LINESTRING")
    expect_contains(get_geom_type(result_sfc), "LINESTRING")
    expect_contains(get_geom_type(result_terra), "lines")
    expect_contains(get_geom_type(result_geos), "linestring")
  }
)

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

test_that(
  "cnt_path_guess handles several POLYGON objects correctly and saves the
  attribute table",
  {
    polygon_sf <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "shapes",
        quiet = TRUE
      )
    polygon_sf$id <- seq_len(nrow(polygon_sf))

    polygon_sfc <- sf::st_as_sfc(polygon_sf)
    polygon_geos <- geos::as_geos_geometry(polygon_sf)
    polygon_terra <- terra::vect(polygon_sf)

    centerline_sf <- cnt_path_guess(polygon_sf)
    centerline_sfc <- cnt_path_guess(polygon_sfc)
    centerline_geos <- cnt_path_guess(polygon_geos)
    centerline_terra <- cnt_path_guess(polygon_terra)

    # Check length of geometries
    expect_equal(
      nrow(centerline_sf),
      nrow(polygon_sf)
    )
    expect_equal(
      length(centerline_sfc),
      length(polygon_sfc)
    )
    expect_equal(
      nrow(centerline_terra),
      nrow(polygon_terra)
    )
    expect_equal(
      length(centerline_geos),
      length(polygon_geos)
    )

    # Compare attribute tables
    expect_identical(
      sf::st_drop_geometry(centerline_sf),
      sf::st_drop_geometry(polygon_sf)
    )
    expect_identical(
      terra::as.data.frame(centerline_terra),
      terra::as.data.frame(polygon_terra)
    )
  }
)


test_that(
  "cnt_path_guess handles several MULTIPOLYGON objects correctly and saves the
  attribute table",
  {
    # One MULTIPOLYGON
    multipolygon_sf <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "lake_island",
        quiet = TRUE
      )
    multipolygon_sfc <- st_as_sfc(multipolygon_sf)
    multipolygon_terra <- terra::vect(multipolygon_sf)
    multipolygon_geos <- multipolygon_sf |>
      geos::as_geos_geometry()

    number_of_geometries <-
      multipolygon_geos |>
      geos::geos_num_geometries()

    result_sfc <- cnt_path_guess(multipolygon_sfc)
    result_sf <- cnt_path_guess(multipolygon_sf)
    result_terra <- cnt_path_guess(multipolygon_terra)
    result_geos <- cnt_path_guess(multipolygon_geos)

    # Check class
    expect_s3_class(result_sfc, c("sfc"))
    expect_s3_class(result_sf, c("sf"))
    expect_s4_class(result_terra, c("SpatVector"))
    expect_s3_class(result_geos, c("geos_geometry"))

    # Check length of geometries
    expect_equal(
      length(result_sfc),
      number_of_geometries
    )
    expect_equal(
      nrow(result_sf),
      number_of_geometries
    )
    expect_equal(
      nrow(result_terra),
      number_of_geometries
    )
    expect_equal(
      length(result_geos),
      number_of_geometries
    )

    # Check attribute tables
    expect_identical(
      sf::st_drop_geometry(result_sf)[1, ],
      sf::st_drop_geometry(multipolygon_sf)
    )
    expect_identical(
      terra::as.data.frame(result_terra)[1, ],
      terra::as.data.frame(multipolygon_terra)
    )
  }
)
