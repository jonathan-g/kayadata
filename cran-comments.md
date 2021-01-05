## Test environments

* local Windows 10 install, R 4.0.3
* local ubuntu 20.04 LTS install, R 4.0.3
* GitHub actions ubuntu 20.04, R 4.0.3
* r-hub builder:
    * Ubuntu 16.04, R-Release gcc
    * Ubuntu 16.04, R-Devel gcc
    * Debian, R-Release gcc
    * Debian, R-Devel gcc
    * Debian, R-Patched gcc
    * Debian, R-Devel clang
    * Fedora, R-Devel GCC
    * Fedora, R-Devel clang, gfortran
    * macOS 10.13.6 High Sierra, R-release, brew
    * macOS 10.13.6 High Sierra, R-release, CRAN setup
* winbuilder devel, release, oldrelease

## R CMD check results

* Local installs and builds on GitHub actions:

    0 errors | 0 warnings | 0 notes

* r-hub debian R-devel build:

    0 errors | 0 warnings | 1 note

* winbuilder builds:

    0 errors | 0 warnings | 0 notes

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
