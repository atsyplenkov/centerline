test_that("cnt_path handles 'terra' geometries", {
  skip_if_not_installed("terra")

  polygon <- terra::vect(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon"
  )
  start_point <- terra::vect(
    system.file("extdata/example.gpkg", package = "centerline"),
    query = "SELECT * FROM polygon_points WHERE type IS 'start'"
  )
  end_point <- terra::vect(
    system.file("extdata/example.gpkg", package = "centerline"),
    query = "SELECT * FROM polygon_points WHERE type IS 'end'"
  )

  # Find polygon's skeleton
  skeleton <- cnt_skeleton(polygon, keep = 1)

  # Two starting points
  result <- cnt_path(skeleton, start_point, end_point)

  # One starting point
  result_one <- cnt_path(skeleton, start_point[1, ], end_point)

  # Test Output Structure
  expect_length(result, 2)
  expect_length(result_one, 1)
  expect_contains(get_geom_type(result), "lines")
  expect_contains(get_geom_type(result_one), "lines")
  # Class, CRS and attributes are inherited
  expect_true(inherits(result, c("SpatVector")))
  expect_true(inherits(result_one, c("SpatVector")))
  expect_equal(
    gsub("_", " ", terra::crs(result)),
    gsub("_", " ", terra::crs(polygon))
  )
  expect_equal(
    gsub("_", " ", terra::crs(result_one)),
    gsub("_", " ", terra::crs(polygon))
  )
  expect_equal(as.data.frame(result), as.data.frame(start_point))
  expect_equal(as.data.frame(result_one), as.data.frame(start_point[1, ]))

  # Invalid inputs
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = skeleton,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = polygon,
    start_point = end_point,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = end_point,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = "not a skeleton",
    start_point = start_point,
    end_point = end_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = "start_point",
    end_point = end_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = "start_point",
    end_point = "end_point"
  ))
})

test_that("cnt_path handles 'sf' geometries", {
  polygon <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon",
    quiet = TRUE
  )
  start_point <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    query = "SELECT * FROM polygon_points WHERE type IS 'start'",
    quiet = TRUE
  )
  end_point <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    query = "SELECT * FROM polygon_points WHERE type IS 'end'",
    quiet = TRUE
  )

  # Find polygon's skeleton
  skeleton <- cnt_skeleton(polygon, keep = 1)

  # Two starting points
  result <- cnt_path(skeleton, start_point, end_point)

  # One starting point
  result_one <- cnt_path(skeleton, start_point[1, ], end_point)

  # Test Output Structure
  expect_equal(nrow(result), 2)
  expect_equal(nrow(result_one), 1)
  expect_contains(get_geom_type(result), "LINESTRING")
  expect_contains(get_geom_type(result_one), "LINESTRING")
  # Class, CRS and attributes are inherited
  expect_true(inherits(result, c("sf")))
  expect_true(inherits(result_one, c("sf")))
  expect_equal(sf::st_crs(result), sf::st_crs(polygon))
  expect_equal(sf::st_crs(result_one), sf::st_crs(polygon))
  expect_equal(sf::st_drop_geometry(result), sf::st_drop_geometry(start_point))
  expect_equal(
    sf::st_drop_geometry(result_one),
    sf::st_drop_geometry(start_point[1, ])
  )

  # Invalid inputs
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = skeleton,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = polygon,
    start_point = end_point,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = end_point,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = "not a skeleton",
    start_point = start_point,
    end_point = end_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = "start_point",
    end_point = end_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = "start_point",
    end_point = "end_point"
  ))
})


test_that("cnt_path handles 'sfc' geometries", {
  polygon <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon",
    quiet = TRUE
  ) |>
    sf::st_as_sfc()
  start_point <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    query = "SELECT * FROM polygon_points WHERE type IS 'start'",
    quiet = TRUE
  ) |>
    sf::st_as_sfc()
  end_point <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    query = "SELECT * FROM polygon_points WHERE type IS 'end'",
    quiet = TRUE
  ) |>
    sf::st_as_sfc()

  # Find polygon's skeleton
  skeleton <- cnt_skeleton(polygon, keep = 1)

  # Two starting points
  result <- cnt_path(skeleton, start_point, end_point)

  # One starting point
  result_one <- cnt_path(skeleton, start_point[1], end_point)

  # Test Output Structure
  expect_equal(length(result), 2)
  expect_equal(length(result_one), 1)
  expect_contains(get_geom_type(result), "LINESTRING")
  expect_contains(get_geom_type(result_one), "LINESTRING")
  # Class, CRS and attributes are inherited
  expect_true(inherits(result, c("sfc")))
  expect_true(inherits(result_one, c("sfc")))
  expect_equal(sf::st_crs(result), sf::st_crs(polygon))
  expect_equal(sf::st_crs(result_one), sf::st_crs(polygon))

  # Invalid inputs
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = skeleton,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = polygon,
    start_point = end_point,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = end_point,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = "not a skeleton",
    start_point = start_point,
    end_point = end_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = "start_point",
    end_point = end_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = "start_point",
    end_point = "end_point"
  ))
})


test_that("cnt_path handles 'geos' geometries", {
  polygon <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon",
    quiet = TRUE
  ) |>
    geos::as_geos_geometry()
  start_point <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    query = "SELECT * FROM polygon_points WHERE type IS 'start'",
    quiet = TRUE
  ) |>
    geos::as_geos_geometry()
  end_point <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    query = "SELECT * FROM polygon_points WHERE type IS 'end'",
    quiet = TRUE
  ) |>
    geos::as_geos_geometry()

  # Find polygon's skeleton
  skeleton <- cnt_skeleton(polygon, keep = 1)

  # Two starting points
  result <- cnt_path(skeleton, start_point, end_point)

  # One starting point
  result_one <- cnt_path(skeleton, start_point[1], end_point)

  # Test Output Structure
  expect_equal(length(result), 2)
  expect_equal(length(result_one), 1)
  expect_contains(get_geom_type(result), "linestring")
  expect_contains(get_geom_type(result_one), "linestring")
  # Class, CRS and attributes are inherited
  expect_true(inherits(result, c("geos_geometry")))
  expect_true(inherits(result_one, c("geos_geometry")))
  expect_identical(wk::wk_crs(result), wk::wk_crs(polygon))
  expect_identical(wk::wk_crs(result_one), wk::wk_crs(polygon))

  # Invalid inputs
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = skeleton,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = polygon,
    start_point = end_point,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = end_point,
    end_point = start_point
  ))
  expect_error(cnt_path(
    skeleton = "not a skeleton",
    start_point = start_point,
    end_point = end_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = "start_point",
    end_point = end_point
  ))
  expect_error(cnt_path(
    skeleton = skeleton,
    start_point = "start_point",
    end_point = "end_point"
  ))
})

test_that("cnt_path errors on incorrect input classes", {
  expect_error(cnt_path(
    skeleton = "not a skeleton",
    start_point = "not a start_point",
    end_point = "not an end_point"
  ))
})

test_that("cnt_path reports unreachable skeleton paths", {
  skeleton <- geos::as_geos_geometry("MULTILINESTRING ((0 0, 2 0), (0 1, 2 1))")
  start <- geos::as_geos_geometry("POINT (0 0)")
  end <- geos::as_geos_geometry("POINT (0 1)")

  error <- expect_no_warning(tryCatch(
    cnt_path(skeleton, start, end),
    error = identity
  ))
  expect_s3_class(error, "error")
  expect_identical(
    conditionMessage(error),
    paste0(
      "Start and end points are not connected by the skeleton graph ",
      "(nearest nodes lie in different connected components or path is empty)."
    )
  )

  starts <- c(
    geos::as_geos_geometry("POINT (2 0)"),
    geos::as_geos_geometry("POINT (0 1)")
  )
  multi_error <- expect_no_warning(tryCatch(
    cnt_path(skeleton, starts, start),
    error = identity
  ))
  expect_s3_class(multi_error, "error")
  expect_identical(
    conditionMessage(multi_error),
    paste0(
      "Start and end points are not connected by the skeleton graph ",
      "(nearest nodes lie in different connected components or path is empty).",
      " Failed start indices: 2."
    )
  )
})

test_that("cnt_path returns an oriented linestring on a connected skeleton", {
  skeleton <- geos::as_geos_geometry(
    "MULTILINESTRING ((0 0, 1 0), (1 0, 1 1), (1 1, 0 1))"
  )
  start <- geos::as_geos_geometry("POINT (0 0)")
  end <- geos::as_geos_geometry("POINT (0 1)")

  path <- expect_no_warning(cnt_path(skeleton, start, end))
  expect_identical(geos::geos_type(path), "linestring")
  expect_equal(geos::geos_distance(geos::geos_point_start(path), start), 0)
  expect_equal(geos::geos_distance(geos::geos_point_end(path), end), 0)
})

test_that("cnt_path returns exact ordered endpoints for anchored skeletons", {
  polygon_sf <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon",
    quiet = TRUE
  )
  points_sf <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon_points",
    quiet = TRUE
  )
  boundary_sf <- sf::st_boundary(polygon_sf)
  anchors_sf <- points_sf[
    as.numeric(sf::st_distance(points_sf, boundary_sf)) == 0,
  ]
  anchors_sf <- anchors_sf[1:2, ]

  # geos
  polygon_geos <- geos::as_geos_geometry(polygon_sf)
  anchors_geos <- geos::as_geos_geometry(anchors_sf)
  sk_geos <- cnt_skeleton(polygon_geos, keep = 1, anchors = anchors_geos)
  expect_s3_class(sk_geos, "geos_geometry")
  expect_contains(get_geom_type(sk_geos), "multilinestring")
  expect_identical(wk::wk_crs(sk_geos), wk::wk_crs(polygon_geos))
  path_geos <- expect_no_warning(cnt_path(
    sk_geos,
    anchors_geos[1],
    anchors_geos[2]
  ))
  expect_equal(
    geos::geos_distance(geos::geos_point_start(path_geos), anchors_geos[1]),
    0
  )
  expect_equal(
    geos::geos_distance(geos::geos_point_end(path_geos), anchors_geos[2]),
    0
  )

  # sf
  sk_sf <- cnt_skeleton(polygon_sf, keep = 1, anchors = anchors_sf)
  expect_s3_class(sk_sf, "sf")
  expect_contains(get_geom_type(sk_sf), "MULTILINESTRING")
  expect_equal(sf::st_crs(sk_sf), sf::st_crs(polygon_sf))
  expect_identical(
    sf::st_drop_geometry(sk_sf),
    sf::st_drop_geometry(polygon_sf)
  )
  path_sf <- expect_no_warning(cnt_path(
    sk_sf,
    anchors_sf[1, ],
    anchors_sf[2, ]
  ))
  path_sf_geos <- geos::as_geos_geometry(path_sf)
  expect_equal(
    geos::geos_distance(
      geos::geos_point_start(path_sf_geos),
      geos::as_geos_geometry(anchors_sf[1, ])
    ),
    0
  )
  expect_equal(
    geos::geos_distance(
      geos::geos_point_end(path_sf_geos),
      geos::as_geos_geometry(anchors_sf[2, ])
    ),
    0
  )
  expect_equal(
    as.data.frame(sf::st_drop_geometry(path_sf)),
    as.data.frame(sf::st_drop_geometry(anchors_sf[1, ])),
    ignore_attr = "row.names"
  )

  # sfc
  polygon_sfc <- sf::st_as_sfc(polygon_sf)
  anchors_sfc <- sf::st_as_sfc(anchors_sf)
  sk_sfc <- cnt_skeleton(polygon_sfc, keep = 1, anchors = anchors_sfc)
  expect_s3_class(sk_sfc, "sfc")
  expect_contains(get_geom_type(sk_sfc), "MULTILINESTRING")
  expect_equal(sf::st_crs(sk_sfc), sf::st_crs(polygon_sfc))
  path_sfc <- expect_no_warning(cnt_path(
    sk_sfc,
    anchors_sfc[1],
    anchors_sfc[2]
  ))
  path_sfc_geos <- geos::as_geos_geometry(path_sfc)
  expect_equal(
    geos::geos_distance(
      geos::geos_point_start(path_sfc_geos),
      geos::as_geos_geometry(anchors_sfc[1])
    ),
    0
  )
  expect_equal(
    geos::geos_distance(
      geos::geos_point_end(path_sfc_geos),
      geos::as_geos_geometry(anchors_sfc[2])
    ),
    0
  )

  # SpatVector
  skip_if_not_installed("terra")
  polygon_terra <- terra::vect(polygon_sf)
  anchors_terra <- terra::vect(anchors_sf)
  sk_terra <- cnt_skeleton(polygon_terra, keep = 1, anchors = anchors_terra)
  expect_s4_class(sk_terra, "SpatVector")
  expect_contains(get_geom_type(sk_terra), "lines")
  expect_equal(
    gsub("_", " ", terra::crs(sk_terra)),
    gsub("_", " ", terra::crs(polygon_terra))
  )
  expect_equal(as.data.frame(sk_terra), as.data.frame(polygon_terra))
  path_terra <- expect_no_warning(cnt_path(
    sk_terra,
    anchors_terra[1],
    anchors_terra[2]
  ))
  path_terra_geos <- geos::as_geos_geometry(sf::st_as_sf(path_terra))
  anchors_terra_geos <- geos::as_geos_geometry(sf::st_as_sf(anchors_terra))
  expect_equal(
    geos::geos_distance(
      geos::geos_point_start(path_terra_geos),
      anchors_terra_geos[1]
    ),
    0
  )
  expect_equal(
    geos::geos_distance(
      geos::geos_point_end(path_terra_geos),
      anchors_terra_geos[2]
    ),
    0
  )
  expect_equal(as.data.frame(path_terra), as.data.frame(anchors_terra[1]))
})



migration_geom_close <- function(actual, baseline_wkb, baseline_length) {
  tol <- max(1e-3, 1e-6 * baseline_length)
  actual_len <- as.numeric(geos::geos_length(actual))
  expect_equal(actual_len, baseline_length, tolerance = tol)
  baseline <- geos::as_geos_geometry(baseline_wkb)
  equal_exact <- tryCatch(
    isTRUE(geos::geos_equals_exact(actual, baseline, tol)),
    error = function(e) FALSE
  )
  if (!equal_exact) {
    # Allow reverse orientation of the whole path
    equal_rev <- tryCatch(
      isTRUE(geos::geos_equals_exact(actual, geos::geos_reverse(baseline), tol)),
      error = function(e) FALSE
    )
    if (!equal_rev) {
      hd <- as.numeric(geos::geos_distance_hausdorff(actual, baseline))
      expect_lte(hd, tol)
    }
  }
  invisible(TRUE)
}

test_that("cnt_path free and anchored routes match graph-migration baseline", {
  baseline <- readRDS(test_path("fixtures/graph-migration-baseline.rds"))
  f <- system.file("extdata/example.gpkg", package = "centerline")
  p <- geos::as_geos_geometry(sf::st_read(f, layer = "polygon", quiet = TRUE))
  pts <- geos::as_geos_geometry(
    sf::st_read(f, layer = "polygon_points", quiet = TRUE)
  )
  b <- geos::geos_boundary(p)
  a <- pts[geos::geos_equals(geos::geos_intersection(b, pts), pts)][1:2]

  ordinary <- cnt_skeleton(p, keep = 1)
  free1 <- cnt_path(ordinary, pts[2], pts[1])
  free2 <- cnt_path(ordinary, pts[3], pts[1])
  migration_geom_close(
    free1,
    baseline$free_paths_wkb[1],
    baseline$lengths[["free_path_1"]]
  )
  migration_geom_close(
    free2,
    baseline$free_paths_wkb[2],
    baseline$lengths[["free_path_2"]]
  )

  anchored <- cnt_skeleton(p, keep = 1, anchors = a)
  apath <- cnt_path(anchored, a[1], a[2])
  migration_geom_close(
    apath,
    baseline$anchored_path_wkb,
    baseline$lengths[["anchored_path"]]
  )
})

