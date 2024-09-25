library(sf)
library(geos)
library(terra)

test_that(
  "cnt_skeleton returns same class as input with the same CRS and
  geometry type MULTILINESTRING",
  {
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
    expect_contains(get_geom_type(result_sf), "MULTILINESTRING")
    expect_contains(get_geom_type(result_sfc), "MULTILINESTRING")
    expect_contains(get_geom_type(result_terra), "lines")
    expect_contains(get_geom_type(result_geos), "multilinestring")

    # Check CRS
    expect_equal(sf::st_crs(result_sf), sf::st_crs(polygon_sf))
    expect_equal(sf::st_crs(result_sfc), sf::st_crs(polygon_sfc))
    expect_equal(terra::crs(result_terra), terra::crs(polygon_terra))
    expect_equal(wk::wk_crs(result_geos), wk::wk_crs(polygon_geos))
  }
)

test_that(
  "'keep' parameter affects the output",
  {
    polygon <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "polygon",
        quiet = TRUE
      ) |>
      geos::as_geos_geometry()

    result_simplified <- cnt_skeleton(polygon, keep = 0.1)
    result_not_simplified <- cnt_skeleton(polygon, keep = 1)
    result_densified <- cnt_skeleton(polygon, keep = 1.1)

    num_points_simplified <-
      geos::geos_num_coordinates(result_simplified)
    num_points_not_simplified <-
      geos::geos_num_coordinates(result_not_simplified)
    num_points_densified <-
      geos::geos_num_coordinates(result_densified)

    expect_true(num_points_simplified < num_points_not_simplified)
    expect_true(num_points_not_simplified < num_points_densified)
    expect_true(num_points_simplified < num_points_densified)
  }
)

test_that(
  "cnt_skeleton errors on incorrect input types",
  {
    expect_error(
      cnt_skeleton("not an sf object")
    )
  }
)

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

test_that(
  "cnt_skeleton handles MULTIPOLYGON objects correctly and saves the
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

    result_sfc <- cnt_skeleton(multipolygon_sfc, keep = 1)
    result_sf <- cnt_skeleton(multipolygon_sf, keep = 1)
    result_terra <- cnt_skeleton(multipolygon_terra, keep = 1)
    result_geos <- cnt_skeleton(multipolygon_geos, keep = 1)

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

test_that(
  "cnt_skeleton returns the same amount of 'MULTILINESTRING' geometries
  as 'POLYGON' geometries in the input",
  {
    shapes <-
      sf::st_read(
        system.file("extdata/example.gpkg", package = "centerline"),
        layer = "shapes",
        quiet = TRUE
      )
    shapes$id <- seq_len(nrow(shapes))

    shapes_sfc <- sf::st_as_sfc(shapes)
    shapes_terra <- terra::vect(shapes)
    shapes_geos <- shapes |>
      geos::as_geos_geometry()

    result_sf <- cnt_skeleton(shapes, keep = 1)
    result_sfc <- cnt_skeleton(shapes_sfc, keep = 1)
    result_terra <- cnt_skeleton(shapes_terra, keep = 1)
    result_geos <- cnt_skeleton(shapes_geos, keep = 1)

    # Check length of geometries
    expect_equal(
      nrow(result_sf),
      nrow(shapes)
    )
    expect_equal(
      length(result_sfc),
      length(shapes_sfc)
    )
    expect_equal(
      nrow(result_terra),
      nrow(shapes_terra)
    )
    expect_equal(
      length(result_geos),
      length(shapes_geos)
    )

    # Compare attribute tables
    expect_identical(
      sf::st_drop_geometry(result_sf),
      sf::st_drop_geometry(shapes)
    )
    expect_identical(
      terra::as.data.frame(result_terra),
      terra::as.data.frame(shapes_terra)
    )
  }
)
