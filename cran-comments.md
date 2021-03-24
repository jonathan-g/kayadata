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

* winbuilder builds:

    0 errors | 0 warnings | 0 notes

* r-hub ubuntu builds:

    0 errors | 0 warnings | 1 note

* All other r-hub builds:

    0 errors | 0 warnings | 0 notes
  
### NOTES: 

* R-Hub ubuntu builds have informative note that Suggested package vdiffr
  is missing, but the build proceeds with no warnings or errors or other
  notes.

## Reverse dependencies

There are no reverse dependencies.

## Additional comments

* This submission updates the package to version 0.5.1

* This release is a bugfix for the emissions_factors() function.
