# Planar line graph from GEOS-noded skeleton segments --------------------
# Undirected weighted graph with edge geometry and node strtree. Used by
# cnt_path(), cnt_path_guess(), and boundary-anchor topology queries.

skeleton_graph_build <- function(lines, crs = wk::wk_crs(lines), grid_size = NULL) {
  if (!inherits(lines, "geos_geometry")) {
    stop("lines must be a geos_geometry object")
  }

  line_types <- geos::geos_type(lines)
  if (
    !all(
      line_types %in%
        c("linestring", "multilinestring", "linearring")
    )
  ) {
    stop("lines must contain only line geometries")
  }

  if (any(geos::geos_is_empty(lines))) {
    lines <- lines[!geos::geos_is_empty(lines)]
  }

  if (length(lines) == 0L) {
    stop("no non-empty edges after noding")
  }

  gs <- NA_real_
  if (!is.null(grid_size)) {
    if (
      length(grid_size) != 1L ||
        !is.finite(grid_size) ||
        grid_size <= 0
    ) {
      stop("grid_size must be one positive finite number")
    }
    gs <- as.numeric(grid_size)
    lines <- geos::geos_set_precision(
      lines,
      gs,
      preserve_topology = TRUE,
      keep_collapsed = FALSE
    )
  }

  # One collection so intersections across separate input features node together.
  noded <- geos::geos_node(geos::geos_make_collection(lines))
  edges <- geos::geos_unnest(noded, keep_multi = FALSE)
  edges <- edges[!geos::geos_is_empty(edges)]
  edge_types <- geos::geos_type(edges)
  edges <- edges[edge_types %in% c("linestring", "linearring")]

  if (length(edges) == 0L) {
    stop("no non-empty edges after noding")
  }

  weight <- as.numeric(geos::geos_length(edges))
  if (any(!is.finite(weight) | weight <= 0)) {
    stop("edge lengths must be positive and finite")
  }

  start_coords <- wk::wk_coords(geos::geos_point_start(edges))
  end_coords <- wk::wk_coords(geos::geos_point_end(edges))

  sx <- start_coords$x
  sy <- start_coords$y
  ex <- end_coords$x
  ey <- end_coords$y

  # Interleave start/end in edge order for deterministic first-occurrence IDs.
  xs <- as.numeric(rbind(sx, ex))
  ys <- as.numeric(rbind(sy, ey))
  # Normalize signed zero so %a keys match.
  xs[xs == 0] <- 0
  ys[ys == 0] <- 0

  keys_all <- paste(sprintf("%a", xs), sprintf("%a", ys), sep = "|")
  key_unique <- unique(keys_all)
  endpoint_ids <- match(keys_all, key_unique)

  from <- as.integer(endpoint_ids[c(TRUE, FALSE)])
  to <- as.integer(endpoint_ids[c(FALSE, TRUE)])

  n_nodes <- length(key_unique)
  # First-occurrence coordinates for each unique key.
  first_idx <- match(key_unique, keys_all)
  node_x <- xs[first_idx]
  node_y <- ys[first_idx]

  graph <- igraph::graph_from_edgelist(
    cbind(from, to),
    directed = FALSE
  )
  # Ensure isolated-node-free graphs still report full vertex count when
  # edgelist max id equals n_nodes (always true here).
  if (igraph::vcount(graph) < n_nodes) {
    graph <- igraph::add_vertices(graph, n_nodes - igraph::vcount(graph))
  }

  igraph::E(graph)$weight <- weight
  igraph::E(graph)$edge_id <- seq_along(weight)

  node_geom <- geos::geos_make_point(node_x, node_y, crs = crs)
  node_tree <- geos::geos_strtree(node_geom)

  if (!is.null(crs)) {
    wk::wk_crs(edges) <- crs
  }

  structure(
    list(
      nodes = list(
        x = node_x,
        y = node_y,
        key = key_unique,
        geom = node_geom,
        tree = node_tree
      ),
      edges = list(
        from = from,
        to = to,
        weight = weight,
        geom = edges
      ),
      graph = graph,
      crs = crs,
      meta = list(
        noded = TRUE,
        grid_size = gs
      )
    ),
    class = "cnt_skeleton_graph"
  )
}

skeleton_graph_degree <- function(g) {
  as.integer(igraph::degree(g$graph))
}

skeleton_graph_neighbors <- function(g, id) {
  as.integer(igraph::neighbors(g$graph, id, mode = "all"))
}

skeleton_graph_nearest_nodes <- function(g, points) {
  as.integer(geos::geos_nearest(points, g$nodes$tree))
}

skeleton_graph_paths <- function(g, from, to) {
  if (length(from) != 1L) {
    stop("paths() requires exactly one source node")
  }

  withCallingHandlers(
    {
      sp <- igraph::shortest_paths(
        g$graph,
        from = from,
        to = to,
        mode = "all",
        weights = g$edges$weight,
        output = "epath"
      )
    },
    warning = function(w) {
      if (grepl("Couldn't reach some vertices", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  lapply(sp$epath, function(e) {
    if (length(e) == 0L) {
      integer()
    } else {
      as.integer(igraph::E(g$graph)$edge_id[e])
    }
  })
}

skeleton_graph_path_line <- function(g, epath) {
  if (length(epath) == 0L) {
    stop("empty edge path")
  }
  line <- g$edges$geom[epath] |>
    geos::geos_make_collection() |>
    geos::geos_line_merge()
  if (!is.null(g$crs)) {
    wk::wk_crs(line) <- g$crs
  }
  line
}

skeleton_graph_node_points <- function(g) {
  g$nodes$geom
}
