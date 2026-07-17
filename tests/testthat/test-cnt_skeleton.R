test_that("cnt_skeleton works with 'terra' objects", {
  skip_if_not_installed("terra")

  polygon_sfc <- sf::st_sfc(sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  ))))
  polygon_terra <- terra::vect(polygon_sfc)
  result_terra <- cnt_skeleton(polygon_terra, keep = 1)

  # Check classes and CRS
  expect_s4_class(result_terra, c("SpatVector"))
  expect_contains(get_geom_type(result_terra), "lines")
  expect_equal(terra::crs(result_terra), terra::crs(polygon_terra))
})

test_that("cnt_skeleton works with 'terra' MULTIPOLYGON geometries", {
  skip_if_not_installed("terra")
  # One MULTIPOLYGON
  multipolygon_terra <- terra::vect(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "lake_island"
  )

  result_terra <- cnt_skeleton(multipolygon_terra, keep = 1)

  expect_s4_class(result_terra, c("SpatVector"))
  expect_equal(nrow(result_terra), 8)
  expect_identical(
    terra::as.data.frame(result_terra)[1, ],
    terra::as.data.frame(multipolygon_terra)
  )
})

test_that("cnt_skeleton works with 'terra' multiple POLYGON geometries", {
  skip_if_not_installed("terra")
  # One MULTIPOLYGON
  shapes_terra <- terra::vect(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "shapes"
  )
  shapes_terra$id <- seq_len(nrow(shapes_terra))

  result_terra <- cnt_skeleton(shapes_terra, keep = 1)

  expect_equal(nrow(result_terra), nrow(shapes_terra))
  expect_identical(
    terra::as.data.frame(result_terra),
    terra::as.data.frame(shapes_terra)
  )
})

test_that("cnt_skeleton returns same class as input with the same CRS and
  geometry type MULTILINESTRING", {
  polygon_sfc <- sf::st_sfc(sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  ))))

  polygon_sf <- sf::st_as_sf(polygon_sfc)
  polygon_geos <- geos::as_geos_geometry(polygon_sf)

  result_sfc <- cnt_skeleton(polygon_sfc, keep = 1)
  result_sf <- cnt_skeleton(polygon_sf, keep = 1)
  result_geos <- cnt_skeleton(polygon_geos, keep = 1)

  # Check class
  expect_s3_class(result_sfc, c("sfc"))
  expect_s3_class(result_sf, c("sf"))
  expect_s3_class(result_geos, c("geos_geometry"))

  # Check geometry types
  expect_contains(get_geom_type(result_sf), "MULTILINESTRING")
  expect_contains(get_geom_type(result_sfc), "MULTILINESTRING")
  expect_contains(get_geom_type(result_geos), "multilinestring")

  # Check CRS
  expect_equal(sf::st_crs(result_sf), sf::st_crs(polygon_sf))
  expect_equal(sf::st_crs(result_sfc), sf::st_crs(polygon_sfc))
  expect_equal(wk::wk_crs(result_geos), wk::wk_crs(polygon_geos))

  # Check type errors
  expect_error(cnt_skeleton(polygon_geos, keep = "a"))
  expect_error(cnt_skeleton(polygon_geos, keep = -10))
  expect_error(cnt_skeleton(polygon_geos, keep = 10))
  expect_error(cnt_skeleton(polygon_geos, method = "a"))
})

test_that("'keep' parameter affects the output", {
  polygon <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon",
    quiet = TRUE
  ) |>
    geos::as_geos_geometry()

  result_simplified <- cnt_skeleton(polygon, keep = 0.1)
  result_not_simplified <- cnt_skeleton(polygon, keep = 1)
  result_densified <- cnt_skeleton(polygon, keep = 1.1)

  num_points_simplified <- geos::geos_num_coordinates(result_simplified)
  num_points_not_simplified <- geos::geos_num_coordinates(result_not_simplified)
  num_points_densified <- geos::geos_num_coordinates(result_densified)

  expect_true(num_points_simplified < num_points_not_simplified)
  expect_true(num_points_not_simplified < num_points_densified)
  expect_true(num_points_simplified < num_points_densified)
})

test_that("cnt_skeleton errors on incorrect input types", {
  expect_error(cnt_skeleton("not an sf object"))
})

test_that("cnt_skeleton works with any 'keep' parameter", {
  polygon <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "shapes",
    quiet = TRUE
  )
  polygon$id <- seq_len(nrow(polygon))
  polygon21 <- subset(polygon, id == 21)

  # Test that all paths are created without errors
  # With keep parameter varying from 0 to 2
  test_list <- lapply(seq(0.1, 2, by = 0.1), function(x) {
    tryCatch(cnt_skeleton(polygon21, keep = x), error = \(e) NA)
  })

  # Check that all paths are not NA
  expect_true(!anyNA(test_list))
})

test_that("cnt_skeleton handles MULTIPOLYGON objects correctly and saves the
  attribute table", {
  # One MULTIPOLYGON
  multipolygon_sf <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "lake_island",
    quiet = TRUE
  )
  multipolygon_sfc <- sf::st_as_sfc(multipolygon_sf)
  multipolygon_geos <- multipolygon_sf |> geos::as_geos_geometry()

  number_of_geometries <- multipolygon_geos |> geos::geos_num_geometries()

  result_sfc <- cnt_skeleton(multipolygon_sfc, keep = 1)
  result_sf <- cnt_skeleton(multipolygon_sf, keep = 1)
  result_geos <- cnt_skeleton(multipolygon_geos, keep = 1)

  # Check class
  expect_s3_class(result_sfc, c("sfc"))
  expect_s3_class(result_sf, c("sf"))
  expect_s3_class(result_geos, c("geos_geometry"))

  # Check length of geometries
  expect_equal(length(result_sfc), number_of_geometries)
  expect_equal(nrow(result_sf), number_of_geometries)
  expect_equal(length(result_geos), number_of_geometries)

  # Check attribute tables
  expect_identical(
    sf::st_drop_geometry(result_sf)[1, ],
    sf::st_drop_geometry(multipolygon_sf)
  )
})

test_that("cnt_skeleton returns the same amount of 'MULTILINESTRING' geometries
  as 'POLYGON' geometries in the input", {
  shapes <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "shapes",
    quiet = TRUE
  )
  shapes$id <- seq_len(nrow(shapes))

  shapes_sfc <- sf::st_as_sfc(shapes)
  shapes_geos <- geos::as_geos_geometry(shapes)

  result_sf <- cnt_skeleton(shapes, keep = 1)
  result_sfc <- cnt_skeleton(shapes_sfc, keep = 1)
  result_geos <- cnt_skeleton(shapes_geos, keep = 1)

  # Check length of geometries
  expect_equal(nrow(result_sf), nrow(shapes))
  expect_equal(length(result_sfc), length(shapes_sfc))
  expect_equal(length(result_geos), length(shapes_geos))

  # Compare attribute tables
  expect_identical(
    sf::st_drop_geometry(result_sf),
    sf::st_drop_geometry(shapes)
  )
})

test_that("cnt_skeleton generates straight skeletons", {
  skip_if_not_installed("raybevel")

  shapes <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "shapes",
    quiet = TRUE
  ) |>
    geos::as_geos_geometry()

  shape_no_hole <- shapes[1]
  shape_w_hole <- shapes[89]

  result_no_hole <- cnt_skeleton(shape_no_hole, method = "straight")
  result_w_hole <- cnt_skeleton(shape_w_hole, method = "straight")

  # Check class
  expect_s3_class(result_no_hole, c("geos_geometry"))
  expect_s3_class(result_w_hole, c("geos_geometry"))

  # Check crs
  expect_identical(sf::st_crs(result_no_hole), sf::st_crs(shape_no_hole))
  expect_identical(sf::st_crs(result_w_hole), sf::st_crs(shape_w_hole))

  # Test that all skeletons are created without errors
  # With keep parameter varying from 0 to 1
  list_no_hole <- lapply(seq(0.1, 1, by = 0.1), function(x) {
    tryCatch(
      cnt_skeleton(shape_no_hole, keep = x, method = "straight"),
      error = \(e) NA
    )
  })
  list_w_hole <- lapply(seq(0.1, 1, by = 0.1), function(x) {
    tryCatch(
      cnt_skeleton(shape_w_hole, keep = x, method = "straight"),
      error = \(e) NA
    )
  })

  # Estimate lengths
  lengths_w_hole <- vapply(
    list_w_hole,
    geos::geos_length,
    FUN.VALUE = numeric(1)
  )
  lengths_no_hole <- vapply(
    list_no_hole,
    geos::geos_length,
    FUN.VALUE = numeric(1)
  )

  # Check that all paths are not NA
  expect_true(!anyNA(list_no_hole))
  expect_true(!anyNA(list_w_hole))

  # Check that first length is smaller than the median and last is larger
  expect_true(lengths_no_hole[1] < median(lengths_no_hole))
  expect_true(lengths_w_hole[1] < median(lengths_w_hole))

  # Expect warning, when keep > 1
  expect_warning(cnt_skeleton(shape_w_hole, keep = 1.1, method = "straight"))
  expect_warning(cnt_skeleton(shape_no_hole, keep = 1.1, method = "straight"))
})

# Boundary anchors --------------------------------------------------------

example_boundary_anchors_geos <- function() {
  polygon <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon",
    quiet = TRUE
  ) |>
    geos::as_geos_geometry()
  points <- sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "polygon_points",
    quiet = TRUE
  ) |>
    geos::as_geos_geometry()
  boundary <- geos::geos_boundary(polygon)
  anchors <- points[geos::geos_equals(
    geos::geos_intersection(boundary, points),
    points
  )]
  list(polygon = polygon, anchors = anchors[1:2])
}

anchor_grid_size <- function(polygon) {
  bbox <- unclass(wk::wk_bbox(polygon))
  sqrt((bbox$xmax - bbox$xmin)^2 + (bbox$ymax - bbox$ymin)^2) * 1e-7
}

skeleton_graph_info <- function(skeleton) {
  g <- skeleton_graph_build(skeleton, crs = wk::wk_crs(skeleton))
  list(
    graph = g,
    nodes = skeleton_graph_node_points(g),
    degree = skeleton_graph_degree(g),
    xy = cbind(g$nodes$x, g$nodes$y)
  )
}

test_that("boundary anchors attach approved junctions on example.gpkg", {
  data <- example_boundary_anchors_geos()
  polygon <- data$polygon
  anchors <- data$anchors
  ordinary <- cnt_skeleton(polygon, keep = 1)
  ordinary_net <- skeleton_graph_info(ordinary)
  grid_size <- anchor_grid_size(polygon)

  # Pre-augmentation: anchors are not ordinary skeleton nodes.
  for (i in seq_along(anchors)) {
    d <- as.numeric(geos::geos_distance(anchors[i], ordinary_net$nodes))
    expect_true(all(d > 0))
  }

  anchored <- cnt_skeleton(polygon, keep = 1, anchors = anchors)
  expect_true(geos::geos_covers(anchored, ordinary))

  aug <- skeleton_graph_info(anchored)
  approved <- list(
    c(1830873.1875, 5453788.018333333),
    c(1830873.699999998, 5453778.95)
  )
  expected_lengths <- c(0.489655, 0.489126)
  boundary <- geos::geos_boundary(polygon)

  for (i in seq_along(anchors)) {
    d <- as.numeric(geos::geos_distance(anchors[i], aug$nodes))
    zero_ids <- which(d == 0)
    expect_equal(length(zero_ids), 1L)
    expect_equal(unname(aug$degree[[zero_ids[[1]]]]), 1L)

    nbrs <- skeleton_graph_neighbors(aug$graph, zero_ids[[1]])
    expect_equal(length(nbrs), 1L)
    expect_true(aug$degree[[nbrs[[1]]]] >= 3L)
    target_xy <- aug$xy[nbrs[[1]], 1:2]
    expect_lt(sqrt(sum((target_xy - approved[[i]])^2)), grid_size)

    axy <- as.numeric(wk::wk_coords(anchors[i])[c("x", "y")])
    expect_equal(
      sqrt(sum((target_xy - axy)^2)),
      expected_lengths[[i]],
      tolerance = 1e-3
    )

    connector <- geos::geos_make_linestring(
      x = c(axy[[1]], target_xy[[1]]),
      y = c(axy[[2]], target_xy[[2]]),
      crs = wk::wk_crs(polygon)
    )
    raw_pt <- geos::geos_make_point(
      target_xy[[1]],
      target_xy[[2]],
      crs = wk::wk_crs(polygon)
    )
    expect_true(geos::geos_covered_by(connector, polygon))
    expect_true(geos::geos_equals(
      geos::geos_intersection(connector, boundary),
      anchors[i]
    ))
    expect_true(geos::geos_equals(
      geos::geos_intersection(connector, ordinary),
      raw_pt
    ))
  }
})

test_that("boundary anchors scale with relative precision grid", {
  data <- example_boundary_anchors_geos()
  polygon0 <- data$polygon
  anchors0 <- data$anchors
  bbox <- unclass(wk::wk_bbox(polygon0))
  origin <- c(bbox$xmin, bbox$ymin)

  shift_geom <- function(geom, origin, scale) {
    coords <- wk::wk_coords(geom)
    x <- (coords$x - origin[[1]]) * scale
    y <- (coords$y - origin[[2]]) * scale
    types <- geos::geos_type(geom)
    out <- lapply(seq_along(geom), function(i) {
      idx <- coords$feature_id == i
      if (types[[i]] == "point") {
        geos::geos_make_point(x[idx][[1]], y[idx][[1]])
      } else if (types[[i]] %in% c("polygon", "multipolygon")) {
        # rebuild via WKT translation of shifted coords
        gsf <- sf::st_as_sf(geom[i])
        mat <- sf::st_coordinates(gsf)
        mat[, 1] <- (mat[, 1] - origin[[1]]) * scale
        mat[, 2] <- (mat[, 2] - origin[[2]]) * scale
        # drop ring/id columns and rebuild polygon
        ring <- mat[, 1:2]
        sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(ring)))) |>
          geos::as_geos_geometry()
      } else {
        stop("unsupported type")
      }
    })
    do.call(c, out)
  }

  # Use affine on sf for robust polygon scaling
  scale_pair <- function(scale) {
    pol_sf <- sf::st_as_sf(polygon0)
    anc_sf <- sf::st_as_sf(anchors0)
    shift <- function(g) {
      sf::st_geometry(g) <- (sf::st_geometry(g) - origin) * scale
      sf::st_crs(g) <- NA
      g
    }
    list(
      polygon = geos::as_geos_geometry(shift(pol_sf)),
      anchors = geos::as_geos_geometry(shift(anc_sf))
    )
  }

  for (scale in c(1e-6, 1e6)) {
    pair <- scale_pair(scale)
    ordinary <- cnt_skeleton(pair$polygon, keep = 1)
    ordinary_net <- skeleton_graph_info(ordinary)
    for (i in seq_along(pair$anchors)) {
      d <- as.numeric(geos::geos_distance(pair$anchors[i], ordinary_net$nodes))
      expect_true(all(d > 0))
    }
    anchored <- cnt_skeleton(pair$polygon, keep = 1, anchors = pair$anchors)
    aug <- skeleton_graph_info(anchored)
    targets <- list()
    for (i in seq_along(pair$anchors)) {
      d <- as.numeric(geos::geos_distance(pair$anchors[i], aug$nodes))
      zero_ids <- which(d == 0)
      expect_equal(length(zero_ids), 1L)
      expect_equal(unname(aug$degree[[zero_ids[[1]]]]), 1L)
      nbrs <- skeleton_graph_neighbors(aug$graph, zero_ids[[1]])
      targets[[i]] <- aug$xy[nbrs[[1]], 1:2]
    }
    # normalized junction choice: same relative offset order
    axy1 <- as.numeric(wk::wk_coords(pair$anchors[1])[c("x", "y")])
    axy2 <- as.numeric(wk::wk_coords(pair$anchors[2])[c("x", "y")])
    # route endpoints are exact
    path <- expect_no_warning(cnt_path(
      anchored,
      pair$anchors[1],
      pair$anchors[2]
    ))
    expect_equal(
      geos::geos_distance(geos::geos_point_start(path), pair$anchors[1]),
      0
    )
    expect_equal(
      geos::geos_distance(geos::geos_point_end(path), pair$anchors[2]),
      0
    )
  }
})

test_that("check_anchors validates class, geometry, CRS, and duplicates", {
  polygon <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
      ncol = 2,
      byrow = TRUE
    ))),
    crs = 2193
  )
  anchors <- sf::st_sfc(
    sf::st_point(c(0.5, 0)),
    sf::st_point(c(1, 0.5)),
    crs = 2193
  )

  expect_true(check_anchors(polygon, NULL))
  expect_true(check_anchors(polygon, anchors[0]))

  expect_error(
    check_anchors(polygon, geos::as_geos_geometry(anchors)),
    "anchors must use the same spatial class as input"
  )
  expect_error(
    check_anchors(
      polygon,
      sf::st_sfc(
        sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        crs = 2193
      )
    ),
    "anchors must contain non-empty POINT geometries"
  )
  expect_error(
    check_anchors(polygon, sf::st_sfc(sf::st_point(c(0.5, 0)), crs = 4326)),
    "anchors and input must use the same CRS"
  )
  expect_error(
    check_anchors(
      polygon,
      sf::st_sfc(sf::st_point(c(0.5, 0)), sf::st_point(c(0.5, 0)), crs = 2193)
    ),
    "anchors must not contain duplicate points"
  )
})

test_that("anchors require unique polygon-part boundary membership", {
  poly <- geos::as_geos_geometry("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  expect_error(
    cnt_skeleton(
      poly,
      keep = 1,
      anchors = geos::as_geos_geometry("POINT (0.5 0.5)")
    ),
    "Each anchor must lie on exactly one polygon-part boundary"
  )

  # Adjacent unit squares sharing x = 1
  left <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  right <- sf::st_polygon(list(matrix(
    c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
    ncol = 2,
    byrow = TRUE
  )))
  parts <- geos::as_geos_geometry(sf::st_sfc(left, right))
  shared <- geos::as_geos_geometry("POINT (1 0.5)")
  expect_error(
    cnt_skeleton(parts, keep = 1, anchors = shared),
    "Each anchor must lie on exactly one polygon-part boundary"
  )
})

test_that("add_boundary_anchors fails without degree-three junctions", {
  polygon <- geos::as_geos_geometry("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  skeleton <- geos::as_geos_geometry("LINESTRING (0.5 0, 0.5 1)")
  anchor <- geos::as_geos_geometry("POINT (0.5 0)")
  expect_error(
    add_boundary_anchors(skeleton, polygon, anchor),
    "No valid interior junction connector"
  )
})

test_that("add_boundary_anchors rejects concavity-crossing chords", {
  polygon <- geos::as_geos_geometry(
    "POLYGON ((0 0, 4 0, 4 1, 1 1, 1 3, 4 3, 4 4, 0 4, 0 0))"
  )
  # Hand-built ordinary network with two degree-three junctions.
  skeleton <- c(
    geos::as_geos_geometry("LINESTRING (0.5 0.5, 0.5 2)"),
    geos::as_geos_geometry("LINESTRING (0.5 0.5, 0.2 0.2)"),
    geos::as_geos_geometry("LINESTRING (0.5 0.5, 0.8 0.2)"),
    geos::as_geos_geometry("LINESTRING (2 3.5, 3.5 3.5)"),
    geos::as_geos_geometry("LINESTRING (2 3.5, 2 3.2)"),
    geos::as_geos_geometry("LINESTRING (2 3.5, 2 3.8)")
  ) |>
    geos::geos_make_collection() |>
    geos::geos_line_merge()
  anchor <- geos::as_geos_geometry("POINT (1 2)")
  augmented <- add_boundary_anchors(skeleton, polygon, anchor)

  expect_true(geos::geos_covers(augmented, skeleton))
  boundary <- geos::geos_boundary(polygon)
  # Selected connector is to (0.5, 0.5), not the notch-crossing (2, 3.5)
  connector <- geos::geos_make_linestring(x = c(1, 0.5), y = c(2, 0.5))
  expect_true(geos::geos_covered_by(connector, polygon))
  expect_true(geos::geos_equals(
    geos::geos_intersection(connector, boundary),
    anchor
  ))

  aug <- skeleton_graph_info(augmented)
  d <- as.numeric(geos::geos_distance(anchor, aug$nodes))
  zero_ids <- which(d == 0)
  expect_equal(length(zero_ids), 1L)
  nbrs <- skeleton_graph_neighbors(aug$graph, zero_ids[[1]])
  target <- aug$xy[nbrs[[1]], 1:2]
  expect_equal(target[[1]], 0.5, tolerance = 1e-9)
  expect_equal(target[[2]], 0.5, tolerance = 1e-9)
})

test_that("add_boundary_anchors rejects early skeleton crossings", {
  polygon <- geos::as_geos_geometry("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
  skeleton <- c(
    geos::as_geos_geometry("LINESTRING (2 5, 2 4)"),
    geos::as_geos_geometry("LINESTRING (2 5, 1.5 4.5)"),
    geos::as_geos_geometry("LINESTRING (2 5, 3 5)"),
    geos::as_geos_geometry("LINESTRING (1 4.8, 1 5.2)"),
    geos::as_geos_geometry("LINESTRING (8 8, 8 9)"),
    geos::as_geos_geometry("LINESTRING (8 8, 7 8)"),
    geos::as_geos_geometry("LINESTRING (8 8, 9 8)")
  ) |>
    geos::geos_make_collection() |>
    geos::geos_line_merge()
  anchor <- geos::as_geos_geometry("POINT (0 5)")
  augmented <- add_boundary_anchors(skeleton, polygon, anchor)

  aug <- skeleton_graph_info(augmented)
  d <- as.numeric(geos::geos_distance(anchor, aug$nodes))
  zero_ids <- which(d == 0)
  nbrs <- skeleton_graph_neighbors(aug$graph, zero_ids[[1]])
  target <- aug$xy[nbrs[[1]], 1:2]
  expect_equal(target[[1]], 8, tolerance = 1e-9)
  expect_equal(target[[2]], 8, tolerance = 1e-9)

  # Direct connector has no intermediate node at (1, 5)
  connector <- geos::geos_make_linestring(x = c(0, 8), y = c(5, 8))
  inter <- geos::geos_intersection(connector, skeleton)
  # Rejected low-cost junction would include (1,5) as extra intersection
  bad <- geos::geos_make_linestring(x = c(0, 2), y = c(5, 5))
  bad_inter <- geos::geos_intersection(bad, skeleton)
  expect_false(isTRUE(geos::geos_equals(
    bad_inter,
    geos::as_geos_geometry("POINT (2 5)")
  )))
})

test_that("add_boundary_anchors falls back to first skeleton hit", {
  # Only one junction cluster; the direct chord crosses a blocking segment, so
  # a full junction connector is invalid and first-hit must attach mid-edge.
  polygon <- geos::as_geos_geometry("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
  skeleton <- c(
    geos::as_geos_geometry("LINESTRING (2 5, 2 4)"),
    geos::as_geos_geometry("LINESTRING (2 5, 1.5 4.5)"),
    geos::as_geos_geometry("LINESTRING (2 5, 3 5)"),
    geos::as_geos_geometry("LINESTRING (1 4.8, 1 5.2)")
  ) |>
    geos::geos_make_collection() |>
    geos::geos_line_merge()
  anchor <- geos::as_geos_geometry("POINT (0 5)")
  augmented <- add_boundary_anchors(skeleton, polygon, anchor)

  expect_true(geos::geos_covers(augmented, skeleton))
  aug <- skeleton_graph_info(augmented)
  d <- as.numeric(geos::geos_distance(anchor, aug$nodes))
  zero_ids <- which(d == 0)
  expect_equal(length(zero_ids), 1L)
  expect_equal(unname(aug$degree[[zero_ids[[1]]]]), 1L)
  nbrs <- skeleton_graph_neighbors(aug$graph, zero_ids[[1]])
  expect_equal(length(nbrs), 1L)
  target <- aug$xy[nbrs[[1]], 1:2]
  # First hit is the blocking segment at x = 1, y = 5.
  expect_equal(target[[1]], 1, tolerance = 1e-9)
  expect_equal(target[[2]], 5, tolerance = 1e-9)
})

test_that("anchors attach per unnested polygon part for multi inputs", {
  left <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  right <- sf::st_polygon(list(matrix(
    c(3, 0, 4, 0, 4, 1, 3, 1, 3, 0),
    ncol = 2,
    byrow = TRUE
  )))
  polys_sf <- sf::st_sf(id = c("L", "R"), geometry = sf::st_sfc(left, right))
  multi_sf <- sf::st_sf(
    id = "M",
    geometry = sf::st_sfc(sf::st_multipolygon(list(
      list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)),
      list(matrix(c(3, 0, 4, 0, 4, 1, 3, 1, 3, 0), ncol = 2, byrow = TRUE))
    )))
  )
  anchors_sf <- sf::st_sf(
    name = c("a", "b"),
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0)), sf::st_point(c(3.5, 0)))
  )

  # Multirow POLYGON object
  sk_poly <- cnt_skeleton(polys_sf, keep = 1, anchors = anchors_sf)
  expect_equal(nrow(sk_poly), 2)
  expect_identical(
    sf::st_drop_geometry(sk_poly),
    sf::st_drop_geometry(polys_sf)
  )

  # One-row MULTIPOLYGON
  sk_multi <- cnt_skeleton(multi_sf, keep = 1, anchors = anchors_sf)
  expect_equal(nrow(sk_multi), 2)
  expect_identical(
    sf::st_drop_geometry(sk_multi)[1, , drop = FALSE],
    sf::st_drop_geometry(multi_sf)
  )
})

test_that("straight skeleton accepts the same anchors contract", {
  skip_if_not_installed("raybevel")
  polygon <- geos::as_geos_geometry("POLYGON ((0 0, 4 0, 4 2, 0 2, 0 0))")
  anchors <- geos::as_geos_geometry(c("POINT (0 1)", "POINT (4 1)"))
  sk <- cnt_skeleton(polygon, keep = 1, method = "straight", anchors = anchors)
  path <- expect_no_warning(cnt_path(sk, anchors[1], anchors[2]))
  expect_equal(geos::geos_distance(geos::geos_point_start(path), anchors[1]), 0)
  expect_equal(geos::geos_distance(geos::geos_point_end(path), anchors[2]), 0)
  aug <- skeleton_graph_info(sk)
  for (i in seq_along(anchors)) {
    d <- as.numeric(geos::geos_distance(anchors[i], aug$nodes))
    expect_equal(unname(aug$degree[[which(d == 0)[[1]]]]), 1L)
  }
})

# Protected Voronoi preparation ------------------------------------------

protected_voronoi_fixture <- function(scale = 1) {
  #fmt: skip
  exterior <- matrix(
    c(
      0, 0, 2, 0, 4, 0, 6, 0, 8, 0, 10, 0,
      10, 2, 10, 4, 10, 6,
      8, 6, 6, 6, 4, 6, 2, 6, 0, 6,
      0, 4, 0, 2, 0, 0
    ) * scale,
    ncol = 2,
    byrow = TRUE
  )
  hole <- matrix(
    c(3, 2, 3, 4, 7, 4, 7, 2, 3, 2) * scale,
    ncol = 2,
    byrow = TRUE
  )
  polygon <- geos::as_geos_geometry(sf::st_polygon(list(exterior, hole)))
  anchors <- c(
    geos::geos_make_point(4 * scale, 0),
    geos::geos_make_point(4.0000005 * scale, 0),
    geos::geos_make_point(5 * scale, 0),
    geos::geos_make_point(6.5 * scale, 0),
    geos::geos_make_point(7.5 * scale, 0),
    geos::geos_make_point(5 * scale, 2 * scale)
  )
  list(polygon = polygon, anchors = anchors, scale = scale)
}

expect_exact_ring_membership <- function(prepared, anchors) {
  pc <- wk::wk_coords(prepared)
  ac <- wk::wk_coords(anchors)
  for (i in seq_len(nrow(ac))) {
    expect_true(any(pc$x == ac$x[[i]] & pc$y == ac$y[[i]]))
  }
}

test_that("prepare_polygon_for_skeleton matches unprotected branches exactly", {
  data <- protected_voronoi_fixture()
  polygon <- data$polygon
  anchors <- data$anchors

  expect_identical(
    unclass(wk::as_wkb(prepare_polygon_for_skeleton(polygon, keep = 0.5))),
    unclass(wk::as_wkb(geos_ms_simplify(polygon, keep = 0.5)))
  )
  expect_identical(
    unclass(wk::as_wkb(prepare_polygon_for_skeleton(
      polygon,
      keep = 1,
      protect = anchors
    ))),
    unclass(wk::as_wkb(polygon))
  )
  expect_identical(
    unclass(wk::as_wkb(prepare_polygon_for_skeleton(
      polygon,
      keep = 1.5,
      protect = anchors
    ))),
    unclass(wk::as_wkb(geos_ms_densify(polygon, keep = 1.5)))
  )

  # Coordinator cutover must not change ordinary no-anchor Voronoi output.
  expect_identical(
    unclass(wk::as_wkb(cnt_skeleton(polygon, keep = 0.5))),
    unclass(wk::as_wkb(cnt_skeleton_voronoi(geos_ms_simplify(
      polygon,
      keep = 0.5
    ))))
  )
})

test_that("protected simplification keeps exact exterior and hole anchors", {
  data <- protected_voronoi_fixture()
  polygon <- data$polygon
  anchors <- data$anchors

  prepared <- prepare_polygon_for_skeleton(
    polygon,
    keep = 0.3,
    protect = anchors
  )

  expect_true(geos::geos_is_valid(prepared))
  expect_identical(geos::geos_type(prepared), "polygon")
  expect_exact_ring_membership(prepared, anchors)

  pc <- wk::wk_coords(prepared)
  ac <- wk::wk_coords(anchors)
  # Existing exterior vertex, snap case, mid-edge, ordered inserts, hole.
  expect_true(any(pc$x == ac$x[[1]] & pc$y == ac$y[[1]] & pc$ring_id == 1L))
  expect_true(any(pc$x == ac$x[[2]] & pc$y == ac$y[[2]] & pc$ring_id == 1L))
  expect_true(any(pc$x == ac$x[[3]] & pc$y == ac$y[[3]] & pc$ring_id == 1L))
  expect_true(any(pc$x == ac$x[[4]] & pc$y == ac$y[[4]] & pc$ring_id == 1L))
  expect_true(any(pc$x == ac$x[[5]] & pc$y == ac$y[[5]] & pc$ring_id == 1L))
  expect_true(any(pc$x == ac$x[[6]] & pc$y == ac$y[[6]] & pc$ring_id == 2L))

  for (i in seq_along(anchors)) {
    expect_true(isTRUE(geos::geos_equals(
      geos::geos_intersection(geos::geos_boundary(prepared), anchors[i]),
      anchors[i]
    )))
  }

  # Unchanged anchor coordinates and every injected site participates.
  expect_identical(unclass(wk::as_wkb(anchors)), unclass(wk::as_wkb(anchors)))
  expect_true(all(geos::geos_covers(
    geos::geos_unique_points(prepared),
    anchors
  )))
})

test_that("protected preparation isolates anchors per polygon part", {
  left <- protected_voronoi_fixture()$polygon
  #fmt: skip
  right_exterior <- matrix(
    c(
      20, 0, 22, 0, 24, 0, 26, 0, 28, 0, 30, 0,
      30, 2, 30, 4, 30, 6,
      28, 6, 26, 6, 24, 6, 22, 6, 20, 6,
      20, 4, 20, 2, 20, 0
    ),
    ncol = 2,
    byrow = TRUE
  )
  right_hole <- matrix(
    c(23, 2, 23, 4, 27, 4, 27, 2, 23, 2),
    ncol = 2,
    byrow = TRUE
  )
  right <- geos::as_geos_geometry(sf::st_polygon(list(
    right_exterior,
    right_hole
  )))
  left_anchor <- geos::geos_make_point(5, 0)
  right_anchor <- geos::geos_make_point(25, 0)

  prep_left <- prepare_polygon_for_skeleton(
    left,
    keep = 0.3,
    protect = left_anchor
  )
  prep_right <- prepare_polygon_for_skeleton(
    right,
    keep = 0.3,
    protect = right_anchor
  )

  expect_exact_ring_membership(prep_left, left_anchor)
  expect_exact_ring_membership(prep_right, right_anchor)

  lc <- wk::wk_coords(prep_left)
  rc <- wk::wk_coords(prep_right)
  expect_false(any(lc$x == 25 & lc$y == 0))
  expect_false(any(rc$x == 5 & rc$y == 0))
})

test_that("protected preparation respects relative precision grid scales", {
  for (scale in c(1e-6, 1e6)) {
    data <- protected_voronoi_fixture(scale = scale)
    prepared <- prepare_polygon_for_skeleton(
      data$polygon,
      keep = 0.3,
      protect = data$anchors
    )
    expect_true(geos::geos_is_valid(prepared))
    expect_exact_ring_membership(prepared, data$anchors)
    expect_true(all(geos::geos_covers(
      geos::geos_unique_points(prepared),
      data$anchors
    )))
  }
})

test_that("voronoi anchors at keep < 1 yield zero-distance cnt_path ends", {
  data <- example_boundary_anchors_geos()
  polygon <- data$polygon
  anchors <- data$anchors

  prepared <- prepare_polygon_for_skeleton(
    polygon,
    keep = 0.3,
    protect = anchors
  )
  expect_exact_ring_membership(prepared, anchors)

  skeleton <- cnt_skeleton(polygon, keep = 0.3, anchors = anchors)
  path <- expect_no_warning(cnt_path(skeleton, anchors[1], anchors[2]))
  expect_equal(geos::geos_distance(geos::geos_point_start(path), anchors[1]), 0)
  expect_equal(geos::geos_distance(geos::geos_point_end(path), anchors[2]), 0)
})


test_that("ordinary and anchored skeletons match graph-migration baseline", {
  baseline <- readRDS(test_path("fixtures/graph-migration-baseline.rds"))
  f <- system.file("extdata/example.gpkg", package = "centerline")
  p <- geos::as_geos_geometry(sf::st_read(f, layer = "polygon", quiet = TRUE))
  pts <- geos::as_geos_geometry(
    sf::st_read(f, layer = "polygon_points", quiet = TRUE)
  )
  b <- geos::geos_boundary(p)
  a <- pts[geos::geos_equals(geos::geos_intersection(b, pts), pts)][1:2]

  ordinary <- cnt_skeleton(p, keep = 1)
  anchored <- cnt_skeleton(p, keep = 1, anchors = a)

  for (item in list(
    list(ordinary, baseline$ordinary_skeleton_wkb, baseline$lengths[["ordinary_skeleton"]]),
    list(anchored, baseline$anchored_skeleton_wkb, baseline$lengths[["anchored_skeleton"]])
  )) {
    actual <- item[[1]]
    base_len <- item[[3]]
    tol <- max(1e-3, 1e-6 * base_len)
    expect_equal(as.numeric(geos::geos_length(actual)), base_len, tolerance = tol)
    base_geom <- geos::as_geos_geometry(item[[2]])
    equal_exact <- tryCatch(
      isTRUE(geos::geos_equals_exact(actual, base_geom, tol)),
      error = function(e) FALSE
    )
    if (!equal_exact) {
      hd <- as.numeric(geos::geos_distance_hausdorff(actual, base_geom))
      expect_lte(hd, tol)
    }
  }
})
