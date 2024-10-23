# centerline v0.2
Minor release of centerline package, introducing website, new family of
`geom_cnt_*()` functions and new method to generate polygon skeleton.

## Test environments
* local Windows 10 install, R 4.4.1 patched
* github actions Microsoft Windows Server 2022 10.0.20348, R 4.4.1
* github actions macOS (ARM64) 14.6.1, R 4.4.1
* github actions Ubuntu 22.04.05, R 4.4.1
* github actions Ubuntu 22.04.05, R 4.4.1 (No Suggests)
* github actions Ubuntu 22.04.05, R-devel
* github actions Ubuntu 22.04.05, R 4.3.3

## remote CMD check results
0 errors | 0 warnings | 0 notes

## local R CMD check results
0 errors | 0 warnings | 3 notes

- All notes appear to be related to my local testing environment, since 
I haven't seen them during remote testing

* checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
