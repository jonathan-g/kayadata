## Test environments

* local Windows 10 install, R 3.6.1
* local ubuntu 18.04.3 install, R 3.6.1
* local Debian 9 (stretch), R 3.6.1
* ubuntu 16.04.6 (on travis-ci), R 3.6.1
* r-hub builder:
    * Fedora Linux, R-Devel clang
    * Ubuntu 16.04, R-Release gcc
    * Windows Server 2008 R2 SP1, R-release, 32/64 bit
    * macOS 10.11 El Capitan, R-release
* winbuilder: devel, release, oldrelease

## R CMD check results

* Local installs and R-release builds on travis-ci and r-hub for MacOS, Fedora, 
  and Windows builds:

    0 errors | 0 warnings | 0 notes

* r-hub ubuntu and winbuilder builds:

    0 errors | 0 warnings | 1 note
  
### NOTES: 

* **r-hub ubuntu and winbuilder only**: False-positive warning about (possibly) 
  invalid URL <https://www.eia.gov/outlooks/archive/ieo17/>.
  This only occurs on r-hub builder and not other builds. I have checked this 
  URL manually and it is correct and working when I visit it from a web 
  browser.
  
    This URL is the underlying source of the data provided by this package
    (it's listed in "\source" lines in td_trends.Rd and td_values.Rd).
    It is a third-party web site operated by the U.S. Department of Energy
    and is not under my control.

* This is an update

    I have updated the data to incorporate the 2019 Statistical Report on 
    World Energy.

## Reverse dependencies

There are no reverse dependencies.
