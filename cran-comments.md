## Test environments

* local Windows 10 install, R 4.4.1
* local Ubuntu 22.04.4 LTS install, R 4.4.1
* GitHub actions:
  * Ubuntu latest, R release
  * Ubuntu latest, R devel
  * Ubuntu latest, R oldrel-1
  * Windows latest, R release
  * MacOS latest, R release
* r-hub builder:
  * linux
  * macos
  * windows
  * ubuntu-next
  * ubuntu-release
* winbuilder devel, release, oldrelease

## R CMD check results

* All builds:

    0 errors | 0 warnings

* Notes:
  * The win-builder builds have notes about "possibly invalid URL" in the 
    documentation with 503 return codes from URLs in the package documentation 
    that return dynamically generated content from web queries, such as
    <https://www.eia.gov/electricity/monthly/epm_table_grapher.php?t=epmt_6_07_a>
    I have verified that the URLs are valid and work consistently when visited 
    interactively with browsers, but often give 503's when they are retrieved 
    by robots.
  * There are no other notes from any of the builds.
  * There are no notes from any of the local builds or the GitHub actions

## Reverse dependencies

There are no reverse dependencies.

## Additional comments

* This submission updates the package to version 1.4.0

* This release updates the data to the latest releases from World Bank and 
  Energy Institute, with energy use, CO2 emissions,  economic, and population 
  data through 2024.
