# Polygon simplifications ------------------------------------------------
# Fast simplification, similiar to {mapshaper} ms_simplify
geos_ms_simplify <-
  function(geom, keep) {
    perimeter_length <-
      geos::geos_length(geom)

    point_count <-
      geom |>
      geos::geos_num_coordinates()

    point_density <-
      perimeter_length / point_count

    geos::geos_simplify(
      geom,
      tolerance = point_density / (keep * 7)
    )
  }

# Fast densification, similar behavior to {mapshaper} ms_simplify
geos_ms_densify <-
  function(geom, keep) {
    perimeter_length <-
      geos::geos_length(geom)

    point_count <-
      geom |>
      geos::geos_num_coordinates()

    point_density <-
      perimeter_length / point_count

    geos::geos_densify(
      geom,
      tolerance = point_density / (keep)
    )
  }

geos_ksmooth <-
  function(input) {
    do.call(c, lapply(input, geos_ksmooth_master))
  }

geos_ksmooth_master <-
  function(input) {
    check_package("smoothr")
    checkmate::assert_class(input, "geos_geometry")

    crs <- wk::wk_crs(input)

    num_coords <-
      geos::geos_num_coordinates(input)
    cent_length <-
      geos::geos_length(input)
    simpl_tolerance <- cent_length / num_coords

    m <- input |>
      geos::geos_simplify(tolerance = simpl_tolerance) |>
      geos_to_matrix()

    m <- smoothr::smooth_ksmooth(m, wrap = FALSE)

    geos::geos_make_linestring(m[, 1], m[, 2], crs = crs)
  }

# Reverse lines if needed ------------------------------------------------
# Check if we need to reverse the lines
reverse_lines_if_needed <-
  function(lines_list_geos, end_point) {
    start_centerline <- geos::geos_point_start(lines_list_geos[[1]])
    end_centerline <- geos::geos_point_end(lines_list_geos[[1]])
    end_geos <- geos::as_geos_geometry(end_point)

    start_tail <- geos::geos_distance(end_geos, start_centerline)
    end_tail <- geos::geos_distance(end_geos, end_centerline)

    if (start_tail < end_tail) {
      lines_list_geos |>
        lapply(geos::geos_reverse) |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge)
    } else {
      lines_list_geos |>
        lapply(geos::geos_make_collection) |>
        lapply(geos::geos_line_merge)
    }
  }

# Outer nodes of the skeleton --------------------------------------------
# Faster alternative to igraph::centr_betw()
find_outer_nodes <-
  function(skeleton_geos) {
    all_index <- geos::geos_strtree(skeleton_geos)

    start_points <- geos::geos_point_start(skeleton_geos)
    end_points <- geos::geos_point_end(skeleton_geos)

    start_intersects <- geos::geos_intersects_matrix(start_points, all_index)
    end_intersects <- geos::geos_intersects_matrix(end_points, all_index)

    lonely_start <- start_points[
      vapply(
        start_intersects,
        FUN = function(x) length(x) == 1,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
      )
    ]
    lonely_end <- end_points[
      vapply(
        end_intersects,
        FUN = function(x) length(x) == 1,
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
      )
    ]

    c(lonely_end, lonely_start)
  }

find_closest_nodes <-
  function(sf_graph, nodes_geos) {
    geos_graph <-
      sfnetworks::activate(sf_graph, "nodes") |>
      sf::st_as_sf() |>
      geos::as_geos_geometry() |>
      geos::geos_strtree()

    geos::geos_nearest(nodes_geos, geos_graph)
  }

# Straight skeleton helpers ----------------------------------------------
# Extract ring from polygon
# If the i == 1, it returns the polygon itself
get_geos_ring <-
  function(geos_obj, i) {
    geos_obj |>
      geos::geos_ring_n(i) |>
      geos::geos_polygonize() |>
      geos::geos_unnest()
  }

# Get list of inner rings coordinates in a form of a list of matrices
list_geos_inner_rings <-
  function(geos_obj, num_rings) {
    num_iter <- seq(from = 2, to = num_rings + 1)

    lapply(num_iter, function(i) {
      get_geos_ring(geos_obj, i)
    }) |>
      lapply(geos_to_matrix)
  }

# Convert raybevel object to geos_geometry object
raybevel_to_geos <-
  function(rayskeleton, crs = NULL) {
    # Keep only inner links
    sk_links <- rayskeleton$links[!rayskeleton$links$edge, ]
    # Create a data.frame of source nodes
    source_nodes <- rayskeleton$nodes[c("id", "x", "y")]
    names(source_nodes) <- c("source", "start_x", "start_y")
    # Create a data.frame of destination nodes
    destination_nodes <- rayskeleton$nodes[c("id", "x", "y")]
    names(destination_nodes) <- c("destination", "end_x", "end_y")

    # Build a linestring geometry
    sk_new <-
      merge(x = sk_links, y = source_nodes, by = "source", all.x = TRUE) |>
      merge(y = destination_nodes, by = "destination", all.x = TRUE)

    sk_geometry <-
      sprintf(
        "LINESTRING(%s %s, %s %s)",
        sk_new$start_x,
        sk_new$start_y,
        sk_new$end_x,
        sk_new$end_y
      )

    geos::as_geos_geometry(sk_geometry, crs = crs) |>
      geos::geos_make_collection() |>
      geos::geos_line_merge()
  }
