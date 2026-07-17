# Boundary-anchor matching and skeleton augmentation ---------------------

# Match each anchor to exactly one unnested polygon-part boundary.
# Returns a list of one-based anchor indices per polygon part.
match_boundary_anchors <- function(polygons, anchors) {
  n_parts <- length(polygons)
  membership <- vector("list", n_parts)
  for (i in seq_len(n_parts)) {
    membership[[i]] <- integer()
  }

  if (is.null(anchors) || length(anchors) == 0L) {
    return(membership)
  }

  part_hits <- vector("list", length(anchors))
  for (a in seq_along(anchors)) {
    hits <- integer()
    for (i in seq_len(n_parts)) {
      boundary <- geos::geos_boundary(polygons[i])
      intersection <- geos::geos_intersection(boundary, anchors[a])
      if (isTRUE(geos::geos_equals(intersection, anchors[a]))) {
        hits <- c(hits, i)
      }
    }
    part_hits[[a]] <- hits
  }

  bad <- which(lengths(part_hits) != 1L)
  if (length(bad) > 0L) {
    stop(
      "Each anchor must lie on exactly one polygon-part boundary; ",
      "failed anchor indices: ",
      paste(bad, collapse = ", ")
    )
  }

  for (a in seq_along(anchors)) {
    part <- part_hits[[a]]
    membership[[part]] <- c(membership[[part]], a)
  }

  membership
}

# Default number of nearest junctions scored before expanding.
anchor_candidate_k <- 32L

point_xy <- function(geom) {
  coords <- wk::wk_coords(geom)
  c(coords$x[[1]], coords$y[[1]])
}

continuation_angle <- function(anchor_xy, junction_xy, neighbor_xy) {
  # Directed continuation: angle between anchor->junction and
  # junction->neighbor. Straight continuation is 0; a U-turn is 180.
  # The fixture-validated ranking uses the smaller supplement so both
  # approved junctions retain their observed 21.2 / 67.9 scores.
  v1 <- junction_xy - anchor_xy
  v2 <- neighbor_xy - junction_xy
  n1 <- sqrt(sum(v1 * v1))
  n2 <- sqrt(sum(v2 * v2))
  if (n1 == 0 || n2 == 0) {
    return(NA_real_)
  }
  cosine <- sum(v1 * v2) / (n1 * n2)
  cosine <- max(-1, min(1, cosine))
  angle <- acos(cosine) * 180 / pi
  min(angle, 180 - angle)
}

# Collect point locations from a skeleton/connector intersection geometry.
intersection_hit_points <- function(geom) {
  if (geos::geos_is_empty(geom)) {
    return(NULL)
  }

  type <- geos::geos_type(geom)
  if (identical(type, "point")) {
    return(geom)
  }
  if (identical(type, "multipoint")) {
    return(geos::geos_unnest(geom, keep_multi = FALSE))
  }

  # linestring / multilinestring / geometrycollection: unique vertices
  pts <- geos::geos_unique_points(geom)
  if (geos::geos_is_empty(pts)) {
    return(NULL)
  }
  if (identical(geos::geos_type(pts), "point")) {
    return(pts)
  }
  geos::geos_unnest(pts, keep_multi = FALSE)
}

# Augment one ordinary skeleton with direct connectors from boundary anchors
# to valid interior junctions. Anchors never become Voronoi sites.
#
# Candidate search scores only the K nearest cleaned junctions (expanding K
# until success or all junctions are tried). Connectors prefer a full chord
# to a degree-at-least-three junction; if every full chord fails, fall back to
# the first skeleton hit along the same ranked rays (mid-edge T-junction).
add_boundary_anchors <- function(skeleton, polygon, anchors) {
  if (is.null(anchors) || length(anchors) == 0L) {
    return(skeleton)
  }

  crs <- wk::wk_crs(polygon)
  grid_size <- boundary_grid_size(polygon)

  ordinary_skeleton <- skeleton
  boundary <- geos::geos_boundary(polygon)

  # Raw unrounded graph used for connector endpoints and branch coverage.
  raw_lines <- ordinary_skeleton |>
    geos::geos_node() |>
    geos::geos_unnest(keep_multi = FALSE) |>
    sf::st_as_sf()
  raw_network <- sfnetworks::as_sfnetwork(
    raw_lines,
    directed = FALSE,
    edges_as_lines = TRUE
  )
  raw_nodes <- sfnetworks::activate(raw_network, "nodes") |> sf::st_as_sf()
  raw_degree <- igraph::degree(raw_network)
  raw_xy <- sf::st_coordinates(raw_nodes)

  # Cleaned analysis graph used only for candidate discovery and scoring.
  cleaned_lines <- tryCatch(
    {
      ordinary_skeleton |>
        geos::geos_set_precision(
          grid_size,
          preserve_topology = TRUE,
          keep_collapsed = FALSE
        ) |>
        geos::geos_node() |>
        geos::geos_unnest(keep_multi = FALSE) |>
        sf::st_as_sf() |>
        sf::st_set_precision(0)
    },
    error = function(e) {
      # Extreme scales can make precision-rounded noding fail to converge.
      # Fall back to the unrounded node set for analysis only; mapping still
      # uses grid_size against raw junctions.
      raw_lines
    }
  )
  cleaned_network <- sfnetworks::as_sfnetwork(
    cleaned_lines,
    directed = FALSE,
    edges_as_lines = TRUE
  )
  cleaned_nodes <- sfnetworks::activate(cleaned_network, "nodes") |>
    sf::st_as_sf()
  cleaned_degree <- igraph::degree(cleaned_network)
  cleaned_xy <- sf::st_coordinates(cleaned_nodes)
  cleaned_boundary_dist <- as.numeric(sf::st_distance(
    cleaned_nodes,
    sf::st_as_sf(boundary)
  ))

  cleaned_junction_ids <- which(cleaned_degree >= 3L)
  raw_junction_ids <- which(raw_degree >= 3L)

  if (length(cleaned_junction_ids) == 0L || length(raw_junction_ids) == 0L) {
    stop(
      "No valid interior junction connector; ",
      "ordinary skeleton has no degree-at-least-three junction"
    )
  }

  map_cleaned_to_raw <- function(cleaned_id) {
    cxy <- cleaned_xy[cleaned_id, 1:2, drop = FALSE]
    d <- sqrt(rowSums(
      (raw_xy[raw_junction_ids, 1:2, drop = FALSE] -
        matrix(cxy, nrow = length(raw_junction_ids), ncol = 2, byrow = TRUE))^2
    ))
    within <- which(d <= grid_size)
    if (length(within) == 0L) {
      return(NA_integer_)
    }
    cand_raw <- raw_junction_ids[within]
    cand_d <- d[within]
    cand_xy <- raw_xy[cand_raw, 1:2, drop = FALSE]
    ord <- order(cand_d, cand_xy[, 1], cand_xy[, 2])
    cand_raw[[ord[[1]]]]
  }

  mapped_raw <- vapply(
    cleaned_junction_ids,
    map_cleaned_to_raw,
    integer(1),
    USE.NAMES = FALSE
  )
  keep_map <- !is.na(mapped_raw)
  cleaned_junction_ids <- cleaned_junction_ids[keep_map]
  mapped_raw <- mapped_raw[keep_map]

  if (length(cleaned_junction_ids) == 0L) {
    stop(
      "No valid interior junction connector; ",
      "no cleaned junction maps to a raw degree-at-least-three node"
    )
  }

  n_junctions <- length(cleaned_junction_ids)
  # Junction coordinate matrix and raw targets (numeric hot path).
  j_xy <- cleaned_xy[cleaned_junction_ids, 1:2, drop = FALSE]
  j_raw_id <- as.integer(mapped_raw)

  # Adjacency once; avoids igraph::neighbors per junction per anchor.
  cleaned_adj <- vector("list", nrow(cleaned_xy))
  for (cid in cleaned_junction_ids) {
    cleaned_adj[[cid]] <- as.integer(igraph::neighbors(cleaned_network, cid))
  }

  select_inward_neighbor <- function(cleaned_id, anchor_xy) {
    neighbours <- cleaned_adj[[cleaned_id]]
    if (length(neighbours) == 0L) {
      return(NA_integer_)
    }
    dists <- cleaned_boundary_dist[neighbours]
    max_d <- max(dists)
    candidates <- neighbours[dists >= (max_d - grid_size)]
    if (length(candidates) == 1L) {
      return(candidates[[1]])
    }
    jxy <- cleaned_xy[cleaned_id, 1:2]
    angles <- vapply(
      candidates,
      function(nid) {
        continuation_angle(anchor_xy, jxy, cleaned_xy[nid, 1:2])
      },
      numeric(1),
      USE.NAMES = FALSE
    )
    candidates[[which.min(angles)]]
  }

  connector_valid_for_anchor <- function(connector, anchor, target_pt) {
    covered <- isTRUE(geos::geos_covered_by(connector, polygon))
    boundary_ok <- isTRUE(geos::geos_equals(
      geos::geos_intersection(connector, boundary),
      anchor
    ))
    skeleton_ok <- isTRUE(geos::geos_equals(
      geos::geos_intersection(connector, ordinary_skeleton),
      target_pt
    ))
    list(
      covered = covered,
      boundary = boundary_ok,
      skeleton = skeleton_ok,
      ok = covered && boundary_ok && skeleton_ok
    )
  }

  # Score a subset of junction indices (1..n_junctions) for one anchor.
  # Returns a matrix sorted by cost with columns:
  # j_idx, raw_id, cleaned_x, cleaned_y, distance, turn_angle, cost
  score_junction_subset <- function(axy, j_idx) {
    if (length(j_idx) == 0L) {
      return(matrix(numeric(), nrow = 0L, ncol = 7L))
    }

    n <- length(j_idx)
    out_j <- integer(n)
    out_raw <- integer(n)
    out_x <- numeric(n)
    out_y <- numeric(n)
    out_dist <- numeric(n)
    out_ang <- numeric(n)
    out_cost <- numeric(n)
    keep <- logical(n)

    for (i in seq_len(n)) {
      j <- j_idx[[i]]
      cid <- cleaned_junction_ids[[j]]
      cxy <- j_xy[j, ]
      nid <- select_inward_neighbor(cid, axy)
      if (is.na(nid)) {
        next
      }
      ang <- continuation_angle(axy, cxy, cleaned_xy[nid, 1:2])
      if (is.na(ang)) {
        next
      }
      dist <- sqrt((cxy[[1]] - axy[[1]])^2 + (cxy[[2]] - axy[[2]])^2)
      keep[[i]] <- TRUE
      out_j[[i]] <- j
      out_raw[[i]] <- j_raw_id[[j]]
      out_x[[i]] <- cxy[[1]]
      out_y[[i]] <- cxy[[2]]
      out_dist[[i]] <- dist
      out_ang[[i]] <- ang
      out_cost[[i]] <- dist * (1 + ang / 90)
    }

    if (!any(keep)) {
      return(matrix(numeric(), nrow = 0L, ncol = 7L))
    }

    mat <- cbind(
      out_j[keep],
      out_raw[keep],
      out_x[keep],
      out_y[keep],
      out_dist[keep],
      out_ang[keep],
      out_cost[keep]
    )
    # Rank by cost, distance, turn angle, then cleaned coordinates.
    ord <- order(mat[, 7], mat[, 5], mat[, 6], mat[, 3], mat[, 4])
    mat <- mat[ord, , drop = FALSE]
    # When several cleaned candidates map to one raw junction, keep best.
    mat <- mat[!duplicated(mat[, 2]), , drop = FALSE]
    mat <- mat[
      order(mat[, 7], mat[, 5], mat[, 6], mat[, 3], mat[, 4]),
      ,
      drop = FALSE
    ]
    mat
  }

  # First skeleton hit along anchor -> target_xy (excluding the anchor).
  first_hit_connector <- function(axy, anchor, target_xy) {
    full <- geos::geos_make_linestring(
      x = c(axy[[1]], target_xy[[1]]),
      y = c(axy[[2]], target_xy[[2]]),
      crs = crs
    )
    s_int <- geos::geos_intersection(full, ordinary_skeleton)
    hits <- intersection_hit_points(s_int)
    if (is.null(hits) || length(hits) == 0L) {
      return(NULL)
    }

    d <- as.numeric(geos::geos_distance(hits, anchor))
    # Hits at the anchor itself mean the skeleton already reaches the boundary.
    keep <- is.finite(d) & d > grid_size
    if (!any(keep)) {
      return(list(on_skeleton = TRUE))
    }
    hits <- hits[keep]
    d <- d[keep]
    foot <- hits[which.min(d)]
    fxy <- point_xy(foot)
    connector <- geos::geos_make_linestring(
      x = c(axy[[1]], fxy[[1]]),
      y = c(axy[[2]], fxy[[2]]),
      crs = crs
    )
    checks <- connector_valid_for_anchor(connector, anchor, foot)
    list(
      on_skeleton = FALSE,
      connector = connector,
      foot = foot,
      checks = checks,
      distance = sqrt((fxy[[1]] - axy[[1]])^2 + (fxy[[2]] - axy[[2]])^2)
    )
  }

  already_terminal_on_skeleton <- function(anchor) {
    d <- as.numeric(sf::st_distance(sf::st_as_sf(anchor), raw_nodes))
    zero_ids <- which(d == 0)
    if (length(zero_ids) != 1L) {
      return(FALSE)
    }
    id <- zero_ids[[1]]
    if (raw_degree[[id]] != 1L) {
      return(FALSE)
    }
    neighbours <- as.integer(igraph::neighbors(raw_network, id))
    length(neighbours) == 1L && raw_degree[[neighbours[[1]]]] >= 3L
  }

  selected_connectors <- vector("list", length(anchors))
  selected_raw_pts <- vector("list", length(anchors))
  selected_meta <- vector("list", length(anchors))

  for (ai in seq_along(anchors)) {
    anchor <- anchors[ai]
    if (already_terminal_on_skeleton(anchor)) {
      selected_connectors[[ai]] <- NULL
      selected_raw_pts[[ai]] <- NULL
      selected_meta[[ai]] <- list(noop = TRUE)
      next
    }

    # Anchor on a non-terminal skeleton node cannot become a terminal
    # without pruning branches.
    d_raw <- as.numeric(sf::st_distance(sf::st_as_sf(anchor), raw_nodes))
    if (any(d_raw == 0)) {
      stop(
        "No valid interior junction connector for anchor ",
        ai,
        "; anchor lies on the ordinary skeleton but is not a terminal ",
        "node with a unique degree-at-least-three neighbor"
      )
    }

    axy <- point_xy(anchor)

    # Squared distances to all junctions (vectorized); used for K-nearest.
    d2 <- (j_xy[, 1L] - axy[[1]])^2 + (j_xy[, 2L] - axy[[2]])^2
    dist_order <- order(d2)

    chosen <- NULL
    chosen_raw_pt <- NULL
    best_checks <- NULL
    k <- min(as.integer(anchor_candidate_k), n_junctions)

    repeat {
      j_idx <- dist_order[seq_len(k)]
      score_mat <- score_junction_subset(axy, j_idx)
      if (nrow(score_mat) == 0L) {
        if (k >= n_junctions) {
          break
        }
        k <- min(n_junctions, max(k * 2L, k + 1L))
        next
      }

      # Phase A: full chords to degree-at-least-three junctions.
      for (row in seq_len(nrow(score_mat))) {
        rid <- as.integer(score_mat[row, 2L])
        rxy <- raw_xy[rid, 1:2]
        connector <- geos::geos_make_linestring(
          x = c(axy[[1]], rxy[[1]]),
          y = c(axy[[2]], rxy[[2]]),
          crs = crs
        )
        raw_pt <- geos::geos_make_point(rxy[[1]], rxy[[2]], crs = crs)
        checks <- connector_valid_for_anchor(connector, anchor, raw_pt)
        if (
          is.null(best_checks) ||
            sum(unlist(checks[1:3])) > sum(unlist(best_checks[1:3]))
        ) {
          best_checks <- checks
        }
        if (isTRUE(checks$ok)) {
          chosen <- connector
          chosen_raw_pt <- raw_pt
          selected_meta[[ai]] <- list(
            noop = FALSE,
            first_hit = FALSE,
            raw_id = rid,
            distance = score_mat[row, 5L],
            turn_angle = score_mat[row, 6L],
            cost = score_mat[row, 7L]
          )
          break
        }
      }

      if (!is.null(chosen)) {
        break
      }

      # Phase B: first skeleton hit along the same ranked rays.
      for (row in seq_len(nrow(score_mat))) {
        rid <- as.integer(score_mat[row, 2L])
        rxy <- raw_xy[rid, 1:2]
        fh <- first_hit_connector(axy, anchor, rxy)
        if (is.null(fh)) {
          next
        }
        if (isTRUE(fh$on_skeleton)) {
          # Skeleton already reaches this anchor; treat as noop terminal.
          chosen <- NULL
          chosen_raw_pt <- NULL
          selected_meta[[ai]] <- list(noop = TRUE)
          # Mark with a sentinel so the outer code accepts noop.
          chosen <- "noop"
          break
        }
        checks <- fh$checks
        if (
          is.null(best_checks) ||
            sum(unlist(checks[1:3])) > sum(unlist(best_checks[1:3]))
        ) {
          best_checks <- checks
        }
        if (isTRUE(checks$ok)) {
          chosen <- fh$connector
          chosen_raw_pt <- fh$foot
          selected_meta[[ai]] <- list(
            noop = FALSE,
            first_hit = TRUE,
            raw_id = rid,
            distance = fh$distance,
            turn_angle = score_mat[row, 6L],
            cost = score_mat[row, 7L]
          )
          break
        }
      }

      if (!is.null(chosen)) {
        break
      }

      if (k >= n_junctions) {
        break
      }
      k <- min(n_junctions, max(k * 2L, k + 1L))
    }

    if (identical(chosen, "noop")) {
      selected_connectors[[ai]] <- NULL
      selected_raw_pts[[ai]] <- NULL
      next
    }

    if (is.null(chosen)) {
      if (is.null(best_checks)) {
        stop("No valid interior junction connector for anchor ", ai)
      }
      stop(
        "No valid interior junction connector for anchor ",
        ai,
        "; covered_by=",
        best_checks$covered,
        ", boundary_equals_anchor=",
        best_checks$boundary,
        ", skeleton_equals_junction=",
        best_checks$skeleton
      )
    }

    selected_connectors[[ai]] <- chosen
    selected_raw_pts[[ai]] <- chosen_raw_pt
  }

  active_idx <- which(vapply(
    selected_connectors,
    Negate(is.null),
    logical(1),
    USE.NAMES = FALSE
  ))
  connectors <- selected_connectors[active_idx]
  raw_targets <- selected_raw_pts[active_idx]

  if (length(connectors) >= 2L) {
    for (i in seq_len(length(connectors) - 1L)) {
      for (j in seq.int(i + 1L, length(connectors))) {
        inter <- geos::geos_intersection(connectors[[i]], connectors[[j]])
        if (isTRUE(geos::geos_is_empty(inter))) {
          next
        }
        same_target <- isTRUE(geos::geos_equals(
          raw_targets[[i]],
          raw_targets[[j]]
        )) &&
          isTRUE(geos::geos_equals(inter, raw_targets[[i]]))
        if (!same_target) {
          stop("Anchor connectors intersect before their destination junctions")
        }
      }
    }
  }

  if (length(connectors) == 0L) {
    augmented <- ordinary_skeleton
  } else {
    connector_geoms <- do.call(c, connectors)
    augmented <- c(ordinary_skeleton, connector_geoms) |>
      geos::geos_make_collection() |>
      geos::geos_node()
  }
  wk::wk_crs(augmented) <- crs

  # Postconditions: anchors are degree-one terminals; ordinary branches remain.
  aug_edges <- augmented |> geos::geos_unnest(keep_multi = FALSE)
  aug_lines <- sf::st_as_sf(aug_edges)
  aug_network <- sfnetworks::as_sfnetwork(
    aug_lines,
    directed = FALSE,
    edges_as_lines = TRUE
  )
  aug_nodes <- sfnetworks::activate(aug_network, "nodes") |> sf::st_as_sf()
  aug_degree <- igraph::degree(aug_network)

  for (ai in seq_along(anchors)) {
    if (isTRUE(selected_meta[[ai]]$noop)) {
      next
    }
    d <- as.numeric(sf::st_distance(sf::st_as_sf(anchors[ai]), aug_nodes))
    zero_ids <- which(d == 0)
    if (length(zero_ids) != 1L || aug_degree[[zero_ids[[1]]]] != 1L) {
      stop(
        "Postcondition failed: anchor ",
        ai,
        " is not a unique degree-one graph node"
      )
    }

    if (isTRUE(selected_meta[[ai]]$first_hit)) {
      # Mid-edge foot must lie on the ordinary skeleton.
      foot <- selected_raw_pts[[ai]]
      on_sk <- isTRUE(geos::geos_covered_by(foot, ordinary_skeleton)) ||
        isTRUE(geos::geos_intersects(foot, ordinary_skeleton)) ||
        isTRUE(
          as.numeric(geos::geos_distance(foot, ordinary_skeleton)) <= grid_size
        )
      if (!on_sk) {
        stop(
          "Postcondition failed: first-hit foot for anchor ",
          ai,
          " does not lie on the ordinary skeleton"
        )
      }
    } else {
      # Full-chord target was a pre-existing raw junction.
      rid <- selected_meta[[ai]]$raw_id
      if (is.null(rid) || raw_degree[[rid]] < 3L) {
        stop(
          "Postcondition failed: connector target for anchor ",
          ai,
          " was not a pre-existing degree-at-least-three junction"
        )
      }
    }
  }

  if (!isTRUE(geos::geos_covers(augmented, ordinary_skeleton))) {
    stop("Postcondition failed: ordinary skeleton branches were not preserved")
  }

  # Return one MULTILINESTRING per polygon part without line_merge, which can
  # erase explicit junction/terminal noding.
  if (geos::geos_type(augmented) != "multilinestring") {
    augmented <- geos::geos_make_collection(
      aug_edges,
      type_id = "multilinestring"
    )
    wk::wk_crs(augmented) <- crs
  }

  augmented
}

# Precision helpers and protected polygon preparation --------------------

# Relative bbox-diagonal precision used for connector scoring and for
# snap/insert decisions when protecting boundary anchors.
boundary_grid_size <- function(geom) {
  bbox <- unclass(wk::wk_bbox(geom))
  grid_size <- sqrt((bbox$xmax - bbox$xmin)^2 + (bbox$ymax - bbox$ymin)^2) *
    1e-7
  if (!is.finite(grid_size) || grid_size == 0) {
    stop("Unable to derive a positive precision grid from the polygon bbox")
  }
  grid_size
}

# Euclidean distance from a point to a finite segment, plus the clamped
# projection parameter t in [0, 1].
point_to_segment <- function(px, py, x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  len2 <- dx * dx + dy * dy
  if (len2 == 0) {
    return(list(dist = sqrt((px - x1)^2 + (py - y1)^2), t = 0, length = 0))
  }
  t <- ((px - x1) * dx + (py - y1) * dy) / len2
  t_clamped <- max(0, min(1, t))
  qx <- x1 + t_clamped * dx
  qy <- y1 + t_clamped * dy
  list(
    dist = sqrt((px - qx)^2 + (py - qy)^2),
    t = t_clamped,
    length = sqrt(len2)
  )
}

# Insert exact boundary points onto one unnested POLYGON as ring vertices.
# Points are processed in input order. Exact matches are retained; near
# matches snap by replacing the nearest unclaimed vertex; otherwise the
# point is inserted on the nearest eligible segment. Distinct protected
# points are never merged.
insert_points_on_boundary <- function(
  geom,
  points,
  grid_size = boundary_grid_size(geom)
) {
  if (is.null(points) || length(points) == 0L) {
    return(geom)
  }

  crs <- wk::wk_crs(geom)
  coords <- wk::wk_coords(geom)
  ring_ids <- sort(unique(coords$ring_id))

  # Open rings (closing duplicate removed) plus claimed-vertex flags.
  rings <- lapply(ring_ids, function(rid) {
    idx <- which(coords$ring_id == rid)
    x <- coords$x[idx]
    y <- coords$y[idx]
    n <- length(x)
    # Drop exact closing coordinate; ring is later closed on rebuild.
    if (n >= 2L && x[[1]] == x[[n]] && y[[1]] == y[[n]]) {
      x <- x[-n]
      y <- y[-n]
    }
    list(x = x, y = y, claimed = rep(FALSE, length(x)))
  })
  names(rings) <- as.character(ring_ids)

  # Pending inserts: list of data.frames keyed by ring, with columns
  # segment, t, order, x, y. Applied after all points are assigned so
  # multi-inserts on one segment can be sorted by t then input order.
  pending <- lapply(ring_ids, function(rid) {
    data.frame(
      segment = integer(),
      t = numeric(),
      order = integer(),
      x = numeric(),
      y = numeric()
    )
  })
  names(pending) <- as.character(ring_ids)

  pt_coords <- wk::wk_coords(points)

  for (pi in seq_len(nrow(pt_coords))) {
    px <- pt_coords$x[[pi]]
    py <- pt_coords$y[[pi]]

    # 1) Exact existing ring coordinate.
    exact_hit <- FALSE
    for (r in seq_along(rings)) {
      rx <- rings[[r]]$x
      ry <- rings[[r]]$y
      hit <- which(rx == px & ry == py)
      if (length(hit) > 0L) {
        rings[[r]]$claimed[[hit[[1]]]] <- TRUE
        exact_hit <- TRUE
        break
      }
    }
    if (exact_hit) {
      next
    }

    # 2) Snap to nearest unclaimed vertex within grid_size.
    best_vertex_dist <- Inf
    best_vertex <- NULL
    for (r in seq_along(rings)) {
      rx <- rings[[r]]$x
      ry <- rings[[r]]$y
      claimed <- rings[[r]]$claimed
      for (vi in seq_along(rx)) {
        if (claimed[[vi]]) {
          next
        }
        d <- sqrt((rx[[vi]] - px)^2 + (ry[[vi]] - py)^2)
        if (d <= grid_size && d < best_vertex_dist) {
          best_vertex_dist <- d
          best_vertex <- list(ring = r, vertex = vi)
        } else if (
          d <= grid_size && d == best_vertex_dist && !is.null(best_vertex)
        ) {
          # Tie-break: earlier ring, then earlier vertex.
          if (
            r < best_vertex$ring ||
              (r == best_vertex$ring && vi < best_vertex$vertex)
          ) {
            best_vertex <- list(ring = r, vertex = vi)
          }
        }
      }
    }

    if (!is.null(best_vertex)) {
      r <- best_vertex$ring
      vi <- best_vertex$vertex
      rings[[r]]$x[[vi]] <- px
      rings[[r]]$y[[vi]] <- py
      rings[[r]]$claimed[[vi]] <- TRUE
      next
    }

    # 3) Insert on nearest non-zero-length segment within grid_size.
    best_seg_dist <- Inf
    best_seg <- NULL
    for (r in seq_along(rings)) {
      rx <- rings[[r]]$x
      ry <- rings[[r]]$y
      n <- length(rx)
      for (si in seq_len(n)) {
        sj <- if (si < n) si + 1L else 1L
        hit <- point_to_segment(px, py, rx[[si]], ry[[si]], rx[[sj]], ry[[sj]])
        if (hit$length == 0) {
          next
        }
        if (hit$dist <= grid_size && hit$dist < best_seg_dist) {
          best_seg_dist <- hit$dist
          best_seg <- list(ring = r, segment = si, t = hit$t)
        } else if (
          hit$dist <= grid_size &&
            hit$dist == best_seg_dist &&
            !is.null(best_seg)
        ) {
          if (
            r < best_seg$ring || (r == best_seg$ring && si < best_seg$segment)
          ) {
            best_seg <- list(ring = r, segment = si, t = hit$t)
          }
        }
      }
    }

    if (is.null(best_seg)) {
      stop("Protected point is not on the polygon boundary within tolerance")
    }

    rkey <- names(rings)[[best_seg$ring]]
    pending[[rkey]] <- rbind(
      pending[[rkey]],
      data.frame(
        segment = best_seg$segment,
        t = best_seg$t,
        order = pi,
        x = px,
        y = py
      )
    )
  }

  # Apply pending inserts per ring, sorted by segment, then t, then order.
  for (r in seq_along(rings)) {
    rkey <- names(rings)[[r]]
    ins <- pending[[rkey]]
    if (nrow(ins) == 0L) {
      next
    }
    ins <- ins[order(ins$segment, ins$t, ins$order), , drop = FALSE]

    rx <- rings[[r]]$x
    ry <- rings[[r]]$y
    claimed <- rings[[r]]$claimed
    n <- length(rx)

    new_x <- numeric()
    new_y <- numeric()
    new_claimed <- logical()

    for (si in seq_len(n)) {
      new_x <- c(new_x, rx[[si]])
      new_y <- c(new_y, ry[[si]])
      new_claimed <- c(new_claimed, claimed[[si]])

      seg_ins <- ins[ins$segment == si, , drop = FALSE]
      if (nrow(seg_ins) > 0L) {
        new_x <- c(new_x, seg_ins$x)
        new_y <- c(new_y, seg_ins$y)
        new_claimed <- c(new_claimed, rep(TRUE, nrow(seg_ins)))
      }
    }

    rings[[r]]$x <- new_x
    rings[[r]]$y <- new_y
    rings[[r]]$claimed <- new_claimed
  }

  # Rebuild closed polygon with exterior then holes, original order.
  out_x <- numeric()
  out_y <- numeric()
  out_ring <- integer()
  for (r in seq_along(rings)) {
    rx <- rings[[r]]$x
    ry <- rings[[r]]$y
    rid <- ring_ids[[r]]
    # Restore exact closing coordinate.
    out_x <- c(out_x, rx, rx[[1]])
    out_y <- c(out_y, ry, ry[[1]])
    out_ring <- c(out_ring, rep(rid, length(rx) + 1L))
  }

  geos::geos_make_polygon(out_x, out_y, ring_id = out_ring, crs = crs)
}

# Deterministic three-vertex non-collinear stability set for one open ring.
# First vertex, first farthest vertex from it, then first vertex farthest
# from that line. Guarantees simplification cannot collapse the ring below
# a valid triangle.
ring_stability_vertices <- function(x, y) {
  n <- length(x)
  if (n == 0L) {
    return(integer())
  }
  v1 <- 1L
  if (n == 1L) {
    return(v1)
  }

  d1 <- (x - x[[v1]])^2 + (y - y[[v1]])^2
  v2 <- which.max(d1)
  # which.max returns first max on ties
  if (n == 2L || d1[[v2]] == 0) {
    return(unique(c(v1, v2)))
  }

  # Distance from each point to the infinite line through v1-v2.
  ax <- x[[v1]]
  ay <- y[[v1]]
  bx <- x[[v2]]
  by <- y[[v2]]
  dx <- bx - ax
  dy <- by - ay
  denom <- sqrt(dx * dx + dy * dy)
  if (denom == 0) {
    return(unique(c(v1, v2)))
  }
  cross <- abs((x - ax) * dy - (y - ay) * dx) / denom
  v3 <- which.max(cross)
  unique(c(v1, v2, v3))
}

# Remove consecutive segments shorter than grid_size when the removable
# endpoint is an ordinary free vertex. Never remove protected or stability
# vertices. When both endpoints are ordinary, remove the later one in ring
# order.
prune_short_free_segments <- function(x, y, protected, grid_size) {
  if (length(x) < 3L) {
    return(list(x = x, y = y, protected = protected))
  }

  changed <- TRUE
  while (changed && length(x) > 3L) {
    changed <- FALSE
    n <- length(x)
    remove_idx <- NA_integer_
    for (i in seq_len(n)) {
      j <- if (i < n) i + 1L else 1L
      d <- sqrt((x[[j]] - x[[i]])^2 + (y[[j]] - y[[i]])^2)
      if (d >= grid_size || d == 0) {
        next
      }
      # Prefer removing a free endpoint; never a protected one.
      if (!protected[[i]] && !protected[[j]]) {
        # Both ordinary: remove the later one in ring order.
        # For the wrap-around segment (n, 1), "later" is 1 only if we
        # treat ring order cyclically; plan says later in ring order,
        # so for (n,1) the later is 1? Ring order: after n comes 1.
        # "Later" means higher index for non-wrap, and for wrap the
        # second endpoint in traversal (j).
        remove_idx <- j
      } else if (!protected[[j]]) {
        remove_idx <- j
      } else if (!protected[[i]]) {
        remove_idx <- i
      } else {
        next
      }
      break
    }
    if (!is.na(remove_idx)) {
      x <- x[-remove_idx]
      y <- y[-remove_idx]
      protected <- protected[-remove_idx]
      changed <- TRUE
    }
  }

  list(x = x, y = y, protected = protected)
}

# Simplify one open ring chain between fixed endpoints using GEOS DP.
# Endpoints are not duplicated in the result.
simplify_open_chain <- function(x, y, tolerance) {
  n <- length(x)
  if (n <= 2L) {
    return(list(x = x, y = y))
  }
  line <- geos::geos_make_linestring(x, y)
  simple <- geos::geos_simplify(line, tolerance = tolerance)
  sc <- wk::wk_coords(simple)
  list(x = sc$x, y = sc$y)
}

# Protected Douglas-Peucker simplification that keeps every protected
# coordinate as an exact ring vertex of the rebuilt polygon.
geos_simplify_preserve_points <- function(geom, protect, keep) {
  if (is.null(protect) || length(protect) == 0L) {
    return(geos_ms_simplify(geom, keep = max(keep, 0.05)))
  }

  # Tolerance from the original part before insertion (same rule as
  # geos_ms_simplify, with the internal max(keep, 0.05) floor).
  perimeter_length <- geos::geos_length(geom)
  point_count <- geos::geos_num_coordinates(geom)
  tolerance <- perimeter_length / point_count / (max(keep, 0.05) * 7)

  grid_size <- boundary_grid_size(geom)
  crs <- wk::wk_crs(geom)
  protect_pts <- protect

  inserted <- insert_points_on_boundary(geom, protect_pts, grid_size)
  coords <- wk::wk_coords(inserted)
  ring_ids <- sort(unique(coords$ring_id))
  ac <- wk::wk_coords(protect_pts)

  out_x <- numeric()
  out_y <- numeric()
  out_ring <- integer()

  for (rid in ring_ids) {
    idx <- which(coords$ring_id == rid)
    x <- coords$x[idx]
    y <- coords$y[idx]
    n <- length(x)
    if (n >= 2L && x[[1]] == x[[n]] && y[[1]] == y[[n]]) {
      x <- x[-n]
      y <- y[-n]
    }
    n <- length(x)

    # Exact protected-coordinate matches on this ring.
    is_protect <- vapply(
      seq_len(n),
      function(i) {
        any(ac$x == x[[i]] & ac$y == y[[i]])
      },
      logical(1),
      USE.NAMES = FALSE
    )

    stability <- ring_stability_vertices(x, y)
    is_fixed <- is_protect
    is_fixed[stability] <- TRUE

    fixed_idx <- which(is_fixed)
    if (length(fixed_idx) == 0L) {
      # Should not happen (stability always picks at least vertex 1), but
      # fall back to whole-ring DP.
      closed_x <- c(x, x[[1]])
      closed_y <- c(y, y[[1]])
      chain <- simplify_open_chain(closed_x, closed_y, tolerance)
      # Drop closing duplicate if present.
      cx <- chain$x
      cy <- chain$y
      cn <- length(cx)
      if (cn >= 2L && cx[[1]] == cx[[cn]] && cy[[1]] == cy[[cn]]) {
        cx <- cx[-cn]
        cy <- cy[-cn]
      }
      x <- cx
      y <- cy
      is_fixed <- rep(FALSE, length(x))
      if (length(x) > 0L) {
        is_fixed[[1]] <- TRUE
      }
    } else {
      # Walk cyclic chains between consecutive fixed vertices.
      # fixed_idx is already in ring order.
      fixed_idx <- sort(unique(fixed_idx))
      n_fixed <- length(fixed_idx)
      chain_x <- numeric()
      chain_y <- numeric()
      chain_fixed <- logical()

      for (fi in seq_len(n_fixed)) {
        start <- fixed_idx[[fi]]
        end <- fixed_idx[[if (fi < n_fixed) fi + 1L else 1L]]

        if (fi < n_fixed) {
          path_idx <- start:end
        } else {
          # Wrap-around chain from last fixed to first fixed.
          if (start <= n) {
            path_idx <- c(start:n, seq_len(end))
          } else {
            path_idx <- seq_len(end)
          }
          # When start == end and only one fixed vertex, path is full cycle.
          if (n_fixed == 1L) {
            path_idx <- c(start:n, seq_len(start))
          }
        }

        px <- x[path_idx]
        py <- y[path_idx]
        simplified <- simplify_open_chain(px, py, tolerance)
        sx <- simplified$x
        sy <- simplified$y

        # Drop the end vertex for every chain; it is the start of the next
        # (or the final close). Keep the start vertex.
        if (length(sx) >= 1L) {
          # Exclude last point (shared with next chain start / close).
          keep_n <- length(sx) - 1L
          if (keep_n >= 1L) {
            chain_x <- c(chain_x, sx[seq_len(keep_n)])
            chain_y <- c(chain_y, sy[seq_len(keep_n)])
            # First of each chain is fixed; intermediates are free.
            cf <- rep(FALSE, keep_n)
            cf[[1]] <- TRUE
            chain_fixed <- c(chain_fixed, cf)
          }
        }
      }

      x <- chain_x
      y <- chain_y
      is_fixed <- chain_fixed
    }

    # Prune short free segments only.
    pruned <- prune_short_free_segments(x, y, is_fixed, grid_size)
    x <- pruned$x
    y <- pruned$y

    if (length(x) < 3L) {
      stop("Failed to build a valid polygon while preserving boundary anchors")
    }

    out_x <- c(out_x, x, x[[1]])
    out_y <- c(out_y, y, y[[1]])
    out_ring <- c(out_ring, rep(rid, length(x) + 1L))
  }

  result <- geos::geos_make_polygon(out_x, out_y, ring_id = out_ring, crs = crs)

  # Validate; repair only on failure, and only if protected coords survive.
  if (!isTRUE(geos::geos_is_valid(result))) {
    repaired <- geos::geos_make_valid(result)
    repaired_parts <- if (
      geos::geos_type(repaired) %in% c("multipolygon", "geometrycollection")
    ) {
      geos::geos_unnest(repaired, keep_multi = FALSE)
    } else {
      repaired
    }
    ok_type <- length(repaired_parts) == 1L &&
      identical(geos::geos_type(repaired_parts), "polygon") &&
      isTRUE(geos::geos_is_valid(repaired_parts))
    if (!ok_type) {
      stop("Failed to build a valid polygon while preserving boundary anchors")
    }
    rc <- wk::wk_coords(repaired_parts)
    protect_ok <- all(vapply(
      seq_len(nrow(ac)),
      function(i) {
        any(rc$x == ac$x[[i]] & rc$y == ac$y[[i]])
      },
      logical(1),
      USE.NAMES = FALSE
    ))
    if (!protect_ok) {
      stop("Failed to build a valid polygon while preserving boundary anchors")
    }
    result <- repaired_parts
    wk::wk_crs(result) <- crs
  }

  # Final invariants: exact membership + boundary equality of protect set.
  rc <- wk::wk_coords(result)
  exact_ok <- all(vapply(
    seq_len(nrow(ac)),
    function(i) {
      any(rc$x == ac$x[[i]] & rc$y == ac$y[[i]])
    },
    logical(1),
    USE.NAMES = FALSE
  ))
  boundary_eq <- all(vapply(
    seq_along(protect_pts),
    function(i) {
      isTRUE(geos::geos_equals(
        geos::geos_intersection(geos::geos_boundary(result), protect_pts[i]),
        protect_pts[i]
      ))
    },
    logical(1),
    USE.NAMES = FALSE
  ))
  if (!exact_ok || !boundary_eq) {
    stop(
      "Prepared polygon does not preserve every anchor as an exact ring vertex"
    )
  }

  result
}
