## Test environments
* local Windows 10 install, R 3.6.1
* local ubuntu 18.04.2 install, R 3.6.1
* local Debian 9 (stretch), R 3.6.1
* ubuntu 14.04.6 (on travis-ci), R 3.6.1
* r-hub builder:
    * Ubuntu 16.04, R-Release gcc
    * Ubuntu 16.04, R-Devel gcc
    * Ubuntu 16.04, R-chk
    * Debian, R-Release gcc
    * Debian, R-Devel gcc
    * Debian, R-Patched gcc
    * macOS 10.11 El Capitan, R-release
* winbuilder devel, release, oldrelease

## R CMD check results

* Local installs, R-release builds on travis-ci, and win-builder builds:

    0 errors | 0 warnings | 0 notes

* r-hub debian R-devel build:

    0 errors | 0 warnings | 2 notes

* All other r-hub builds:

    0 errors | 0 warnings | 1 note
  
### NOTES: 

* **r-hub builds**: False-positive warning about 
  two (possibly) invalid URLs <https://www.eia.gov/outlooks/archive/ieo17/> and
  <https://www.eia.gov/electricity/monthly/epm_table_grapher.php?t=epmt_6_07_a>.
  This only occurs on r-hub builder and not other builds. I have checked both 
  URLs manually and they correct and working when I visit them from a web 
  browser.
  
    These URLs reference the underlying source of the data provided by this 
    package (they are listed in "\source" lines in `td_trends.Rd` and
    `td_values.Rd`, and a "\references" line in `generation_capacity.Rd`). 
    They reference a third-party web site operated by the U.S. Department of 
    Energy and is not under my control.

* **r-hub debian R-devel only**: False-positive warning about (possible) 
  misspellings for four people's names in DESCRIPTION: "Kaya", "Keiichi", 
  "Yoichi", and "Yokobori". These names are spelled correctly.

## Reverse dependencies

There are no reverse dependencies.

## Additional comments

* This submission is an update to version 0.4.2.

    I have updated the data to incorporate the 2019 Statistical Report on 
    World Energy.

