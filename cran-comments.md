# centerline v0.2.1
Major release of centerline package, introducing website, new family of
`geom_cnt_*()` functions and new method to generate polygon skeletons. The latter depends on the `raybevel` package which was archived on CRAN on October 22, 2024. It is not a core dependency, but it is crucial for some functions in the `centerline` pkg. To maintain `raybevel::skeletonize` accessibility for the `cnt_skeleton` function, `raybevel` is temporarily installed from GitHub. See notes of the R CMD check results.

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

* checking CRAN incoming feasibility ... [39s] NOTE
  Maintainer: 'Anatoly Tsyplenkov <atsyplenkov@fastmail.com>'
  
  Unknown, possibly misspelled, fields in DESCRIPTION:
    'Remotes'
  
  Suggests or Enhances not in mainstream repositories:
    raybevel

> Notes below appear to be related to my local testing environment, since I haven't seen them during remote testing

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
