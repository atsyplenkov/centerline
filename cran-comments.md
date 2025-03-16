# centerline v0.2.2
Major release of centerline package, introducing website, new family of
`geom_cnt_*()` functions and new method to generate polygon skeletons. This update also solves the current CRAN ERRORs and NOTES associated with a missing R >= 4.1.0 dependency.

## Test environments
* local Windows 10 install, R 4.4.1 patched
* local Debian 12 install, R 4.4.2 patched
* winbuilder Windows Server 2022 x64 (build 20348), R unstable (2025-03-12 r87950 ucrt)
* github actions Microsoft Windows Server 2022 10.0.20348, R 4.4.3
* github actions macOS (ARM64) 14.6.1, R 4.4.3
* github actions Ubuntu 24.04.02, R 4.4.3
* github actions Ubuntu 24.04.02, R 4.4.3 (No Suggests)
* github actions Ubuntu 24.04.05, R-devel
* github actions Ubuntu 24.04.05, R 4.3.3

## remote CMD check results
0 errors | 0 warnings | 0 notes

## local R CMD check results
0 errors | 0 warnings | 1 notes

> Notes below appear to be related to my local testing environment, since I haven't seen them during remote testing

* checking for future file timestamps ... NOTE
  unable to verify current time

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
