# Graph construction and shortest-path utilities --------------------------
# Pure R + geos replacements for sfnetworks / igraph pathfinding

#' Build an undirected edge-weighted graph from geos linestrings
#'
#' Endpoints are deduplicated using exact geometry equality.
#' @noRd
build_graph_geos <- function(lines) {
  starts <- geos::geos_point_start(lines)
  ends   <- geos::geos_point_end(lines)
  all_points <- c(starts, ends)
  n_edges <- length(lines)

  # Find unique nodes via exact geometry equality
  eq_mat <- geos::geos_equals_matrix(all_points, all_points)

  node_ids <- integer(length(all_points))
  current_id <- 1L
  visited <- logical(length(all_points))

  for (i in seq_along(all_points)) {
    if (!visited[i]) {
      group <- eq_mat[[i]]
      node_ids[group] <- current_id
      visited[group] <- TRUE
      current_id <- current_id + 1L
    }
  }

  n_nodes <- current_id - 1L

  from <- node_ids[seq_len(n_edges)]
  to   <- node_ids[seq_len(n_edges) + n_edges]
  weight <- as.numeric(geos::geos_length(lines))

  # Adjacency list: for each node, store incident edge indices
  adj <- vector("list", n_nodes)
  for (e in seq_len(n_edges)) {
    u <- from[e]
    v <- to[e]
    adj[[u]] <- c(adj[[u]], e)
    adj[[v]] <- c(adj[[v]], e)
  }

  # One representative point per unique node
  first_idx <- match(seq_len(n_nodes), node_ids)
  node_points <- all_points[first_idx]

  list(
    n_nodes     = n_nodes,
    n_edges     = n_edges,
    from        = from,
    to          = to,
    weight      = weight,
    geometry    = lines,
    node_points = node_points,
    adj         = adj
  )
}

#' Dijkstra shortest path on a graph built by build_graph_geos()
#'
#' @param graph List as returned by `build_graph_geos()`.
#' @param from Integer scalar, source node ID.
#' @param to Integer scalar, target node ID. If `NULL`, distances to all
#'   nodes are returned.
#' @return If `to` is given, a list with `nodes` (integer vector),
#'   `edges` (integer vector), and `length` (numeric scalar), or `NULL` if no
#'   path exists. If `to` is `NULL`, a list with `dist` (numeric vector),
#'   `prev_node`, and `prev_edge`.
#' @noRd
dijkstra <- function(graph, from, to = NULL) {
  n <- graph$n_nodes
  dist <- rep(Inf, n)
  prev_node <- rep(NA_integer_, n)
  prev_edge <- rep(NA_integer_, n)
  dist[from] <- 0

  unvisited <- seq_len(n)

  while (length(unvisited) > 0) {
    u <- unvisited[which.min(dist[unvisited])]
    if (is.infinite(dist[u])) break
    unvisited <- setdiff(unvisited, u)

    for (e in graph$adj[[u]]) {
      v <- if (graph$from[e] == u) graph$to[e] else graph$from[e]
      alt <- dist[u] + graph$weight[e]
      if (alt < dist[v]) {
        dist[v] <- alt
        prev_node[v] <- u
        prev_edge[v] <- e
      }
    }
  }

  if (!is.null(to)) {
    if (is.infinite(dist[to])) return(NULL)

    path_nodes <- integer(0)
    path_edges <- integer(0)
    u <- to
    while (!is.na(u)) {
      path_nodes <- c(u, path_nodes)
      pe <- prev_edge[u]
      if (!is.na(pe)) path_edges <- c(pe, path_edges)
      u <- prev_node[u]
    }

    list(
      nodes  = path_nodes,
      edges  = path_edges,
      length = dist[to]
    )
  } else {
    list(
      dist      = dist,
      prev_node = prev_node,
      prev_edge = prev_edge
    )
  }
}
