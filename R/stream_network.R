end

lg <-
  cnt_skeleton(lake, keep = 0.1) |>
  st_as_sf()

input <- lg

base_order <- 1L

input_connected <-
  st_intersects(input, sparse = T)

input_touches <-
  sapply(input_connected, unique) |>
  sapply(length)

input$id <- 1:nrow(input)
input$order <- ifelse(
  input_touches %in% c(0:3),
  base_order,
  NA_integer_
)

input <- filter_small(input, order = 1, quantile = 0.5)

input_connected <-
  st_intersects(input, sparse = T)

input_touches <-
  sapply(input_connected, unique) |>
  sapply(length)

input$id <- 1:nrow(input)
input$order <- ifelse(
  input_touches %in% c(0:3),
  base_order,
  NA_integer_
)

input <- filter_small(input, order = 1, quantile = 0.5)

while(sum(is.na(input$order)) >= 1){

  base_order <- max(input$order, na.rm = TRUE) + 1
  input_trim <- subset(input, is.na(input$order))

  trim_touches <-
    st_touches(input_trim, sparse = T) |>
    sapply(unique) |>
    sapply(length)

  id_order <-
    input_trim[which(trim_touches %in% c(0:3)), ]$id

  input[which(input$id %in% id_order), ]$order <- base_order


}

terra::plot(terra::vect(lake))
input |>
  terra::vect() |>
  terra::plot("order", add = T)


filter_small <-
  function(
    obj,
    order,
    quantile = 0.05
  ){

    obj_trim <-
      subset(obj, obj$order == order)

    obj_length <-
      obj_trim |>
      st_length() |>
      as.numeric()

    threshold <-
      quantile(obj_length, quantile)

    filtered_id <-
      obj_trim$id[obj_length <= threshold]

    subset(obj, !obj$id %in% filtered_id)

  }
