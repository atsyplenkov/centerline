lake <-
  sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "lake",
    quiet = TRUE
  )

shapes <- 
  sf::st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "shapes",
    quiet = TRUE
  )

test_that(
  "The geom_cnt_*() creates ggproto objects",
  {
    skip_if_not_installed("geomtextpath")
    skip_if_not_installed("geomtextpath")
    skip_if_not_installed("ggplot2")

    ln <- geom_cnt(data = lake)
    txt <- geom_cnt_text(data = lake, label = "label")
    lbl <- geom_cnt_label(data = lake, label = "label")

    expect_s3_class(ln[[1]], "LayerInstance")
    expect_s3_class(ln[[2]], "CoordSf")
    expect_s3_class(txt[[1]], "LayerInstance")
    expect_s3_class(txt[[2]], "CoordSf")
    expect_s3_class(lbl[[1]], "LayerInstance")
    expect_s3_class(lbl[[2]], "CoordSf")
  }
)

test_that(
  "geom_cnt_*() create correct types",
  {
    skip_if_not_installed("geomtextpath")
    skip_if_not_installed("ggplot2")

    ln <-
      ggplot2::ggplot(lake) +
      geom_cnt(data = lake)
    txt <-
      ggplot2::ggplot(lake) +
      geom_cnt_text(data = lake, label = "label")
    lbl <-
      ggplot2::ggplot(lake) +
      geom_cnt_label(data = lake, label = "label")

    ln_grobs <- ggplot2::layer_grob(ln)[[1]]
    txt_grobs <- ggplot2::layer_grob(txt)[[1]]
    lbl_grobs <- ggplot2::layer_grob(lbl)[[1]]

    expect_s3_class(ln[[2]][[1]], "LayerSf")
    expect_s3_class(txt[[2]][[1]], "LayerSf")
    expect_s3_class(lbl[[2]][[1]], "LayerSf")
    expect_s3_class(ln[[7]], "CoordSf")
    expect_s3_class(txt[[7]], "CoordSf")
    expect_s3_class(lbl[[7]], "CoordSf")
    expect_s3_class(ln_grobs, "polyline")
    expect_s3_class(txt_grobs, "gTree")
    expect_s3_class(lbl_grobs, "gTree")
  }
)

test_that(
  "geom_cnt_*() smiplification works",
  {
    skip_if_not_installed("geomtextpath")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("smoothr")

    ln <-
      ggplot2::ggplot(lake) +
      geom_cnt(data = lake, simplify = TRUE)
    txt <-
      ggplot2::ggplot(lake) +
      geom_cnt_text(data = lake, label = "label", simplify = TRUE)
    lbl <-
      ggplot2::ggplot(lake) +
      geom_cnt_label(data = lake, label = "label", simplify = TRUE)

    ln_grobs <- ggplot2::layer_grob(ln)[[1]]
    txt_grobs <- ggplot2::layer_grob(txt)[[1]]
    lbl_grobs <- ggplot2::layer_grob(lbl)[[1]]

    expect_s3_class(ln[[2]][[1]], "LayerSf")
    expect_s3_class(txt[[2]][[1]], "LayerSf")
    expect_s3_class(lbl[[2]][[1]], "LayerSf")
    expect_s3_class(ln[[7]], "CoordSf")
    expect_s3_class(txt[[7]], "CoordSf")
    expect_s3_class(lbl[[7]], "CoordSf")
    expect_s3_class(ln_grobs, "polyline")
    expect_s3_class(txt_grobs, "gTree")
    expect_s3_class(lbl_grobs, "gTree")
  }
)

test_that(
  "geom_cnt_*() smiplification works with mutiple geometries",
  {
    skip_if_not_installed("geomtextpath")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("smoothr")

    ln <-
      ggplot2::ggplot(shapes) +
      geom_cnt(data = shapes, simplify = TRUE)
    txt <-
      ggplot2::ggplot(shapes) +
      geom_cnt_text(data = shapes, label = "label", simplify = TRUE)
    lbl <-
      ggplot2::ggplot(shapes) +
      geom_cnt_label(data = shapes, label = "label", simplify = TRUE)

    ln_grobs <- ggplot2::layer_grob(ln)[[1]]
    txt_grobs <- ggplot2::layer_grob(txt)[[1]]
    lbl_grobs <- ggplot2::layer_grob(lbl)[[1]]

    expect_s3_class(ln[[2]][[1]], "LayerSf")
    expect_s3_class(txt[[2]][[1]], "LayerSf")
    expect_s3_class(lbl[[2]][[1]], "LayerSf")
    expect_s3_class(ln[[7]], "CoordSf")
    expect_s3_class(txt[[7]], "CoordSf")
    expect_s3_class(lbl[[7]], "CoordSf")
    expect_s3_class(ln_grobs, "polyline")
    expect_s3_class(txt_grobs, "gTree")
    expect_s3_class(lbl_grobs, "gTree")
  }
)