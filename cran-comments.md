## Test environments

* local Windows 10 install, R 3.6.2
* local ubuntu 18.04.2 install, R 3.6.2
* local Debian 9 (stretch), R 3.6.1
* Ubuntu 16.04.6 (on travis-ci), R 3.6.1
* r-hub builder:
    * Ubuntu 16.04, R-Release gcc
    * Ubuntu 16.04, R-Devel gcc
    * Debian, R-Release gcc
    * Debian, R-Devel gcc
    * Debian, R-Patched gcc
    * macOS 10.11 El Capitan, R-release
* winbuilder devel, release, oldrelease

## R CMD check results

* Local installs, R-release builds on travis-ci, and win-builder builds:

    0 errors | 0 warnings | 0 notes

* r-hub debian R-devel build:

    0 errors | 0 warnings | 1 note

* All other r-hub builds:

    0 errors | 0 warnings | 0 notes
  
### NOTES: 

* **r-hub debian R-devel only**: False-positive warning about (possible) 
  misspellings for four people's names in DESCRIPTION: "Kaya", "Keiichi", 
  "Yoichi", and "Yokobori". These names are spelled correctly.

## Reverse dependencies

There are no reverse dependencies.

## Additional comments

* This submission updates the package to version 0.4.3.

I fixed a newly discovered error caused by the update of the scales library
to version 1.1.0 and added relevant regression tests. 
See <https://github.com/tidyverse/ggplot2/issues/3644>.
