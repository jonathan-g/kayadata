## Test environments

* local Windows 10 install, R 4.1.3
* local Ubuntu 20.04 LTS install, R 4.1.3
* local Ubuntu 20.04 LTS install, R 4.1.2
* GitHub actions:
  * Ubuntu 20.04, R 4.1.3
  * Ubuntu 20.04, R devel
  * Ubuntu 20.04, R oldrel-1
  * Windows Server 2020, R 4.1.3
  * MacOS 11, R 4.1.3
  * MacOS Latest, R 4.1.3
* r-hub builder:
  * Ubuntu 20.04.1 LTS, R-Release GCC
  * Ubuntu 20.04.1 LTS, R-Devel GCC
  * Debian, R-Release GCC
  * Debian, R-Patched GCC
  * Fedora, R-Devel GCC
  * Fedora, R-Devel GCC, no long double
  * Fedora, R-Devel clang, gfortran
  * macOS 11.6 Big Sur (M1 processor), R-release
  * macOS 10.13.6 High Sierra, R-release, brew
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup
* winbuilder devel, release, oldrelease

## R CMD check results

* All builds:

    0 errors | 0 warnings

* Notes:
  * The win-builder builds and the r-hub Linux builds under Debian, Fedora, 
    and Ubuntu have notes about "possibly invalid URL" in the documentation with
    503 return codes from URLs in the package documentation that use dynamically 
    generated content from queries, such as
    <https://www.eia.gov/electricity/monthly/epm_table_grapher.php?t=epmt_6_07_a>
    I have verified that the URLs are valid and work when visited interactively
    with browsers, but often give 503's when they are retrieved by robots.
  * There are no other notes from any of the builds.
  * There are no notes from any of the local builds, the GitHub actions,
    or the r-hub MacOS builds.

## Reverse dependencies

There are no reverse dependencies.

## Additional comments

* This submission updates the package to version 1.1.0

* This release adds options to give the user greater control over plots of Kaya
  data.
