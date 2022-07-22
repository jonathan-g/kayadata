## Test environments

* local Windows 10 install, R 4.2.1
* local Ubuntu 20.04 LTS install, R 4.2.1
* GitHub actions:
  * Ubuntu 20.04, R release
  * Ubuntu 20.04, R devel
  * Ubuntu 20.04, R oldrel-1
  * Windows latest, R release
  * MacOS 11, R release
  * MacOS Latest, R release
* r-hub builder:
  * Debian, R-Release GCC
  * Debian, R-Patched GCC
  * Ubuntu 20.04.1 LTS, R-Release GCC
  * Ubuntu 20.04.1 LTS, R-Devel GCC
  * Debian, R-devel, clang, ISO-8859-15 locale
  * Debian, R-devel, GCC
  * Debian, R-devel, GCC, no long double
  * Fedora, R-Devel GCC
  * Fedora, R-Devel clang, gfortran
  * macOS Big Sur (M1 processor), R-release
  * macOS High Sierra, R-release, brew
  * macOS High Sierra, R-release, CRAN's setup
* winbuilder devel, release, oldrelease

## R CMD check results

* All builds:

    0 errors | 0 warnings

* Notes:
  * The win-builder builds and the r-hub Linux builds under Debian, Fedora, 
    and Ubuntu have notes about "possibly invalid URL" in the documentation with
    503 return codes from URLs in the package documentation that return 
    dynamically generated content from web queries, such as
    <https://www.eia.gov/electricity/monthly/epm_table_grapher.php?t=epmt_6_07_a>
    I have verified that the URLs are valid and work consistently when visited 
    interactively with browsers, but often give 503's when they are retrieved 
    by robots.
  * There are no other notes from any of the builds.
  * There are no notes from any of the local builds, the GitHub actions,
    or the r-hub MacOS builds.

## Reverse dependencies

There are no reverse dependencies.

## Additional comments

* This submission updates the package to version 1.2.0

* This release updates the data to the latest releases from World Bank and 
  BP, with energy use, CO2 emissions,  economic, and population data through 
  2021.
