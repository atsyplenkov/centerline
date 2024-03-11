centerline 0.0.1 (2024-03-08)
=========================

### NEW FEATURES
  
  * New function added `cnt_path_guess` which returns the longest centerline based on skeleton only

### UPDATES

  * Twofold speed increase by converting from `terra` objects to `geos` over `wk` package nor `sf` as it was earlier.
  * Added ability to densify the input polygon through `geos::geos_densify`


centerline 0.0.0.9000 (2024-03-01)
=========================

### NEW FEATURES

  * Core functionality is added through the `cnt_skeleton` function
  * Super easy routing is implemented with `cnt_path` function, which can be applied in cases with predefined starting and ending points
