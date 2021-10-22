## Test environments

* local Windows 10 install, R 4.1.1
* local Ubuntu 20.04 LTS install, R 4.0.4
* GitHub actions Ubuntu 20.04, R 4.0.4
* r-hub builder:
  * Ubuntu 20.04.1 LTS, R-Release GCC
  * Ubuntu 20.04.1 LTS, R-Devel GCC
  * Debian, R-Release GCC
  * Debian, R-Patched GCC
  * Fedora, R-Devel GCC
  * Debian, R-Devel GCC ASAN/UBSAN
  * Fedora, R-Devel GCC, no long double
  * Debian, R-Devel clang, ISO-8859-15 locale
  * Fedora, R-Devel clang, gfortran
  * macOS 11.6 Big Sur, Apple Silicon (M1), R-Release
  * macOS 10.13.6 High Sierra, R-release, brew
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup
* winbuilder devel, release, oldrelease

## R CMD check results

* All builds:

    0 errors | 0 warnings

* Some r-hub builds under Debian, Fedora, and Ubuntu have notes about
  "possibly invalid URL" in the documentation with 503 return codes. 
  I have verified that the URLs are valid and work when visited interactively 
  with browsers, but often give 503's when they are retrieved by robots.
* There are no other notes from any of the Linux builds.
* There are no notes from MacOS or Windows builds.

## Reverse dependencies

There are no reverse dependencies.

## Additional comments

* This submission updates the package to version 0.5.2

* This release is a data update that brings data up to date through the 
  end of 2020.
