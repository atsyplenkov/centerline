#' @noRd

# DRAFT
# cnt_path_guess <-
#   function(
#     input,
#     skeleton = NULL,
#     ...
#   ){
#
#     # Save CRS
#     crs <- terra::crs(input)
#
#
#     # Transform to GEOS geometry
#     input_geos <-
#       input |>
#       terra::geom(wkt = T) |>
#       geos::as_geos_geometry()
#
#     input <-
#       centerline:::terra_to_sf(input)
#
#     if (is.null(skeleton)) {
#
#       skeleton <-
#         cnt_skeleton(input = input, ...)
#
#     } else {
#
#       skeleton <-
#         centerline:::terra_to_sf(skeleton)
#
#     }
#
#     # Convert skeleton to sfnetworks
#     pol_network <-
#       skeleton |>
#       sfnetworks::as_sfnetwork(directed = F)
#
#     # Find the most distant nodes from the polygon center
#     end_node <-
#       pol_network |>
#       sfnetworks::activate("nodes") |>
#       dplyr::mutate(bc_undir = tidygraph::centrality_closeness()) |>
#       dplyr::filter(bc_undir == min(bc_undir)) |>
#       # Keep only one point
#       sf::st_as_sf() |>
#       head(n = 1)
#
#     # Find main points of the polygon
#     # main_points <-
#     #   input |>
#     #   rmapshaper::ms_simplify(
#     #     keep = 0.01,
#     #     method = "dp",
#     #     keep_shapes = T
#     #   ) |>
#     #   sf::st_cast("POINT")
#
#     perimeter_length <-
#       geos::geos_length(input_geos)
#
#     point_count <-
#       input_geos |>
#       geos::geos_unique_points() |>
#       geos::geos_num_coordinates()
#
#     point_density <-
#       perimeter_length / point_count
#
#     main_points <-
#       geos::geos_simplify_preserve_topology(
#         input_geos,
#         tolerance = point_density / 0.1
#       ) |>
#       geos::geos_unique_points() |>
#       sf::st_as_sf() |>
#       sf::st_cast("POINT")
#
#     # Find closest nodes to the above found points
#     closest_points <-
#       main_points |>
#       sf::st_nearest_feature(pol_network, check_crs = F)
#
#     closest_end_points <-
#       end_node |>
#       sf::st_nearest_feature(pol_network, check_crs = F)
#
#     # Estimate distance between key points and end point
#     net <-
#       pol_network |>
#       sfnetworks::activate("edges") |>
#       dplyr::mutate(weight = sfnetworks::edge_length())
#
#     paths  <-
#       sfnetworks::st_network_paths(
#         net,
#         to = closest_points,
#         from = rep(closest_end_points, length(closest_points)),
#         weights = "weight"
#       )
#
#     # Remove paths of zero length
#     true_paths <-
#       paths$edge_paths[sapply(paths$edge_paths, length) > 0]
#
#     # Transform paths to sf objects
#     true_paths_sf <-
#       true_paths |>
#       lapply(function(.x) dplyr::slice(sfnetworks::activate(net, "edges"), .x)) |>
#       lapply(st_as_sf)
#
#     # Find total lengths of an object
#     paths_length <-
#       sapply(
#         true_paths_sf,
#         function(i) geos::as_geos_geometry(i) |>
#           geos::geos_length() |>
#           sum()
#         )
#
#     true_paths_sf[[which.max(paths_length)]] |>
#       geos::as_geos_geometry() |>
#       geos::geos_line_merge() |>
#       wk::as_wkt() |>
#       as.character() |>
#       terra::vect(crs = crs)
#   }

# lake <-
#   terra::vect("inst/extdata/example.gpkg", layer = "lake")
#
# lake_skeleton <-
#   cnt_skeleton(lake, keep = 0.1)
#
# plot(lake)
# lake_skeleton |>
#   plot(col = "blue", add = T)
#
# cnt_path_guess(lake, keep = 0.1) |>
#   plot(add = T, col = "firebrick", lwd = 2)
#
