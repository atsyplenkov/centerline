
<!-- README.md is generated from README.Rmd. Please edit that file -->

# centerline

<!-- badges: start -->

[![R-CMD-check](https://github.com/atsyplenkov/centerline/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/atsyplenkov/centerline/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/centerline)](https://CRAN.R-project.org/package=centerline)
![GitHub R package
version](https://img.shields.io/github/r-package/v/atsyplenkov/centerline?label=github)
![GitHub last
commit](https://img.shields.io/github/last-commit/atsyplenkov/centerline)
<!-- badges: end -->

The `centerline` R package simplifies the extraction of linear features
from complex polygons, such as roads or rivers, by computing their
centerlines (or median-axis) using Voronoi diagrams. It uses the
super-fast [`geos`](https://paleolimbot.github.io/geos/index.html) and
[`rmapshaper`](http://andyteucher.ca/rmapshaper/index.html) libraries in
the background.

## Installation

You can install the development version of `centerline` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atsyplenkov/centerline")

# OR

# install.packages("pak")
pak::pak("atsyplenkov/centerline")
```

## Examples for closed geometries

At the heart of this package is the `cnt_skeleton` function, which
efficiently computes the skeleton of closed 2D polygonal geometries. The
function uses
[`rmapshaper::ms_simplify()`](http://andyteucher.ca/rmapshaper/reference/ms_simplify.html)
by default to keep the most important nodes and reduce noise from the
beginning.

``` r
library(centerline)
library(terra)
#> terra 1.7.65

# Load Polygon Of Interest (POI)
polygon <- 
  terra::vect(
    system.file(
      "extdata/example.gpkg", package = "centerline"
    ), 
    layer = "polygon"
  )

# Find POI's skeleton
pol_skeleton <- 
  cnt_skeleton(polygon, simplify = F) 

# Simplified POI's skeleton
pol_skeleton_simplify <- 
  cnt_skeleton(polygon, simplify = T, keep = .05) 

# Plot
par(mfrow = c(1, 2))

# Raw
plot(polygon,
     border = "grey20",
     main = "Raw")
plot(pol_skeleton,
     col = "dodgerblue3",
     add = T)

# Simplified
plot(polygon,
     border = "grey20",
     main = "Simplified")
plot(pol_skeleton_simplify,
     col = "dodgerblue3",
     add = T)
```

<img src="man/figures/README-example-1.png" width="100%" />

However, the above-generated lines are not exactly a centerline of a
polygon. One way to find the centerline of a closed polygon is to define
both `start` and `end` points. For example, in the case of landslides,
it could be the landslide initiation point and landslide terminus.

``` r
# Load points data
points <- 
  terra::vect(
    system.file(
      "extdata/example.gpkg", package = "centerline"
    ),
    layer = "polygon_points"
  )

# Connect points
pol_path <-
  cnt_path(
    skeleton = pol_skeleton,
    start_point = terra::subset(points, points$type == "start"),
    end_point = terra::subset(points, points$type != "start")
  )

# Plot
plot(polygon, border = "grey20", main = "Path connecting starting and ending points")
plot(pol_skeleton, col = "dodgerblue3", add = T)
plot(points[1,], col = "coral2",  add = T)
plot(points[2,], col = "green4",  add = T)
plot(pol_path[[1]], lwd = 3, add = T)
```

<img src="man/figures/README-example2-1.png" width="100%" />

## Alternatives

- **R**
  - [midlines](https://github.com/RichardPatterson/midlines) - A more
    hydrology-oriented library that provides a multi-step approach to
    generate a smooth centerline of complex curved polygons (like
    rivers).
  - [cmgo](https://github.com/AntoniusGolly/cmgo) - The main aim of the
    package is to propose a workflow to extract channel bank metrics,
    and as a part of that workflow, centerline extraction was
    implemented.
- 🐍 Python:
  - [centerline](https://github.com/fitodic/centerline/tree/master)
    library
- 🦀 Rust:
  - [centerline_rs](https://codeberg.org/eadf/centerline_rs) library
