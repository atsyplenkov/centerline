
<!-- README.md is generated from README.Rmd. Please edit that file -->

# centerline

<!-- badges: start -->

[![R-CMD-check](https://github.com/atsyplenkov/centerline/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/atsyplenkov/centerline/actions/workflows/R-CMD-check.yaml)
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

## Example

At the heart of this package is the `cent_skeleton` function, which
efficiently computes the skeleton of closed 2D polygonal geometries. The
function uses
[`rmapshaper::ms_simplify()`](http://andyteucher.ca/rmapshaper/reference/ms_simplify.html)
by default to keep the most important nodes and reduce noise from the
beginning.

``` r
library(centerline)
library(terra)
#> terra 1.7.65

polygon <- 
  terra::vect("inst/extdata/example.gpkg", layer = "polygon")

plot(polygon)

pol_skeleton <- 
  cent_skeleton(polygon, simplify = T) 

pol_skeleton |> 
  plot(col = "blue", add = T)
```

<img src="man/figures/README-example-1.png" width="100%" />
