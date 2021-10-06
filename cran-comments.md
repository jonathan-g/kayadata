## Test environments

* local Windows 10 install, R 4.0.3
* local Ubuntu 20.04 LTS install, R 4.0.4
* GitHub actions Ubuntu 20.04, R 4.0.4
* r-hub builder (with _R_CHECK_FORCE_SUGGESTS_ = "false"):
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

* All builds:

    0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

## Additional comments

* This submission updates the package to version 0.5.1

* This release fixes a bug in the emissions_factors() function.
