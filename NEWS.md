centerline 0.2.2 (2025-03-16)
=========================

### UPDATES

  * the `raybevel` package is back on CRAN
  * Added the explicit R>= 4.1.0 dependency

centerline 0.2.1 (2024-11-05)
=========================

### UPDATES

  * the `raybevel` package was removed from CRAN on October 22, 2024. It is not a core dependency, but it is crucial for some functions in the `centerline` pkg. To maintain `raybevel::skeletonize` accessibility for the `cnt_skeleton` function, `raybevel` is temporarily installed from GitHub.

centerline 0.2 (2024-10-01)
=========================

### UPDATES

  * `geom_cnt_*` function family for plotting centerlines with ggplot2

centerline 0.1.3 (2024-09-29)
=========================

### UPDATES

  * Switched from `rlang` to `checkmate` for type checks
  * Web-only vignettes to reduce package size

centerline 0.1.2 (2024-09-28)
=========================

### UPDATES

  * Website launched with some vignettes.

centerline 0.1.1 (2024-09-27)
=========================

### UPDATES

  * Added a new argument to the `cnt_skeleton()` function — `method`.
    It should be either `"voronoi"` or `"straight"`. Depending on it, the
    generating skeleton method will change.

centerline 0.1 (2024-09-24)
=========================

### UPDATES

  * CRAN first submission

centerline 0.0.5 (2024-09-23)
=========================

### UPDATES

  * Major refactoring of all core functions which led to a more stable results
  * Test coverage increased

### Bug fixes
  * Fix #2
  * Fix #5

centerline 0.0.4 (2024-09-13)
=========================

### UPDATES

  * Two times increase in centerline guessing due to 
  a change in the search algorithm.
  * Centerline guessing now occurs by calculating the longest path 
  between **skeleton nodes**, rather than *polygon nodes* as it was previously. 
  As a result, the final output is smoother at the edges.

### Bug fixes
  * Fix #1
  * Fix #3

centerline 0.0.3 (2024-05-17)
=========================

### UPDATES

  * Got rid of the `rmapshaper` dependency. Similar performance achieved only 
  with `geos` in the backend.
  * Full `geos` support.
  * More tests covering `cnt_skeleton` have been added.
  * Transition from `SpatVector` to `geos_geometry` and vice versa now happens 
  faster through the `wk` package, not through `sf` as before.

centerline 0.0.2 (2024-05-07)
=========================

### UPDATES

  * `cnt_path_guess` performs 1.25 times faster due to a wiser usage of 
  the `igraph` and `geos` packages. The `tidygraph` dependency has been removed.
  * Now, the `cnt_path` family returns one `LINESTRING`, 
  not a collection of `LINESTRINGS`.

centerline 0.0.1 (2024-03-16)
=========================

### NEW FEATURES

  * New function added `cnt_path_guess` which returns the longest centerline 
  based on polygon only

### UPDATES

  * Twofold speed increase by converting from `terra` objects to `geos` over 
  `wk` package nor `sf` as it was earlier.
  * Added ability to densify the input polygon through `geos::geos_densify`


centerline 0.0.0.9000 (2024-03-01)
=========================

### NEW FEATURES

  * Core functionality is added through the `cnt_skeleton` function
  * Super easy routing is implemented with `cnt_path` function, which can be 
  applied in cases with predefined starting and ending points
