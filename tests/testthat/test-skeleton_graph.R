test_that("triangle of three edges has correct topology and weights", {
  lines <- geos::as_geos_geometry(c(
    "LINESTRING(0 0, 1 0)",
    "LINESTRING(1 0, 1 1)",
    "LINESTRING(1 1, 0 0)"
  ))
  g <- skeleton_graph_build(lines)

  expect_s3_class(g, "cnt_skeleton_graph")
  expect_equal(length(g$nodes$x), 3L)
  expect_equal(length(g$edges$weight), 3L)
  expect_equal(as.integer(igraph::vcount(g$graph)), 3L)
  expect_equal(as.integer(igraph::ecount(g$graph)), 3L)
  expect_equal(sort(skeleton_graph_degree(g)), c(2L, 2L, 2L))
  expect_equal(g$edges$weight, as.numeric(geos::geos_length(g$edges$geom)))
  expect_true(g$meta$noded)
  expect_true(is.na(g$meta$grid_size))
})

test_that("two disconnected segments stay disconnected", {
  lines <- geos::as_geos_geometry(c(
    "LINESTRING(0 0, 1 0)",
    "LINESTRING(0 1, 1 1)"
  ))
  g <- skeleton_graph_build(lines)

  expect_equal(as.integer(igraph::vcount(g$graph)), 4L)
  expect_equal(as.integer(igraph::ecount(g$graph)), 2L)
  expect_equal(sort(skeleton_graph_degree(g)), c(1L, 1L, 1L, 1L))

  from <- skeleton_graph_nearest_nodes(
    g,
    geos::as_geos_geometry("POINT(0 0)")
  )
  to <- skeleton_graph_nearest_nodes(
    g,
    geos::as_geos_geometry("POINT(0 1)")
  )
  epath <- skeleton_graph_paths(g, from, to)[[1]]
  expect_equal(length(epath), 0L)
})

test_that("four separately supplied ring edges node into a closed ring", {
  lines <- geos::as_geos_geometry(c(
    "LINESTRING(0 0, 1 0)",
    "LINESTRING(1 0, 1 1)",
    "LINESTRING(1 1, 0 1)",
    "LINESTRING(0 1, 0 0)"
  ))
  g <- skeleton_graph_build(lines)

  expect_equal(as.integer(igraph::vcount(g$graph)), 4L)
  expect_equal(as.integer(igraph::ecount(g$graph)), 4L)
  expect_equal(sort(skeleton_graph_degree(g)), c(2L, 2L, 2L, 2L))
})

test_that("crossing X yields five nodes and center degree four", {
  lines <- geos::as_geos_geometry(c(
    "LINESTRING(0 0, 1 1)",
    "LINESTRING(0 1, 1 0)"
  ))
  g <- skeleton_graph_build(lines)

  expect_equal(as.integer(igraph::vcount(g$graph)), 5L)
  expect_equal(as.integer(igraph::ecount(g$graph)), 4L)
  expect_equal(sort(skeleton_graph_degree(g)), c(1L, 1L, 1L, 1L, 4L))

  center <- skeleton_graph_nearest_nodes(
    g,
    geos::as_geos_geometry("POINT(0.5 0.5)")
  )
  expect_equal(skeleton_graph_degree(g)[[center]], 4L)
  expect_equal(length(skeleton_graph_neighbors(g, center)), 4L)
})

test_that("exact endpoints share a node; near-touch needs grid_size", {
  exact <- geos::as_geos_geometry(c(
    "LINESTRING(0 0, 1 0)",
    "LINESTRING(1 0, 2 0)"
  ))
  g_exact <- skeleton_graph_build(exact)
  expect_equal(as.integer(igraph::vcount(g_exact$graph)), 3L)
  expect_equal(as.integer(igraph::ecount(g_exact$graph)), 2L)

  eps <- 1e-12
  near <- geos::as_geos_geometry(c(
    "LINESTRING(0 0, 1 0)",
    sprintf("LINESTRING(%.16f 0, 2 0)", 1 + eps)
  ))
  g_near <- skeleton_graph_build(near)
  # Distinct IEEE doubles keep separate nodes without precision.
  expect_equal(as.integer(igraph::vcount(g_near$graph)), 4L)

  g_snap <- skeleton_graph_build(near, grid_size = 1e-9)
  expect_equal(as.integer(igraph::vcount(g_snap$graph)), 3L)
  expect_equal(g_snap$meta$grid_size, 1e-9)
})

test_that("edge weights equal GEOS lengths", {
  lines <- geos::as_geos_geometry(c(
    "LINESTRING(0 0, 3 0)",
    "LINESTRING(0 0, 0 4)"
  ))
  g <- skeleton_graph_build(lines)
  expect_equal(sort(g$edges$weight), c(3, 4))
  expect_equal(g$edges$weight, as.numeric(geos::geos_length(g$edges$geom)))
})

test_that("parallel edges keep both geometries and select shorter epath", {
  short <- geos::as_geos_geometry("LINESTRING(0 0, 1 0)")
  long <- geos::as_geos_geometry("LINESTRING(0 0, 0 1, 1 1, 1 0)")
  lines <- c(short, long)
  g <- skeleton_graph_build(lines)

  expect_equal(as.integer(igraph::ecount(g$graph)), 2L)
  expect_equal(as.integer(igraph::vcount(g$graph)), 2L)
  expect_equal(sort(g$edges$weight), sort(as.numeric(geos::geos_length(lines))))

  from <- skeleton_graph_nearest_nodes(
    g,
    geos::as_geos_geometry("POINT(0 0)")
  )
  to <- skeleton_graph_nearest_nodes(
    g,
    geos::as_geos_geometry("POINT(1 0)")
  )
  epath <- skeleton_graph_paths(g, from, to)[[1]]
  expect_equal(length(epath), 1L)

  path_line <- skeleton_graph_path_line(g, epath)
  expect_equal(
    as.numeric(geos::geos_length(path_line)),
    min(g$edges$weight),
    tolerance = 1e-12
  )
  # Edge-ID order matches edges$geom: selected edge is the short segment.
  expect_true(
    isTRUE(geos::geos_equals_exact(path_line, short, 1e-9)) ||
      isTRUE(geos::geos_equals_exact(
        path_line,
        geos::geos_reverse(short),
        1e-9
      ))
  )
})

test_that("path_line rejects empty edge paths", {
  lines <- geos::as_geos_geometry("LINESTRING(0 0, 1 0)")
  g <- skeleton_graph_build(lines)
  expect_error(skeleton_graph_path_line(g, integer()), "empty edge path")
})

test_that("node_points returns stored node geometries", {
  lines <- geos::as_geos_geometry("LINESTRING(0 0, 1 0)")
  g <- skeleton_graph_build(lines)
  pts <- skeleton_graph_node_points(g)
  expect_equal(length(pts), 2L)
  expect_identical(pts, g$nodes$geom)
})

test_that("builder validates grid_size and empty input", {
  lines <- geos::as_geos_geometry("LINESTRING(0 0, 1 0)")
  expect_error(skeleton_graph_build(lines, grid_size = 0), "positive finite")
  expect_error(skeleton_graph_build(lines, grid_size = c(1, 2)), "positive finite")
  expect_error(
    skeleton_graph_build(geos::as_geos_geometry("POINT(0 0)")),
    "line geometries"
  )
})
