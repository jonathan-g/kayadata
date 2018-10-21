## Test environments
* local Windows 10 install, R 3.5.1
* local ubuntu 16.05.5 install, R 3.4.4
* local ubuntu 18.04.1 install, R 3.4.4
* local ubuntu 18.04.1 install, R 3.5.1
* ubuntu 14.04.5 (on travis-ci), R 3.5.1
* r-hub builder
    * ubuntu 16.04, R-devel
    * Windows Server 2008 R2 SP1, R-devel
    * Fedora Linux, R-devel clang gfortran
* winbuilder, devel, release, oldrelease

## R CMD check results

* Local installs and R-release builds on rhub and travis-ci:

    0 errors | 0 warnings | 0 notes

* r-hub and winbuilder R-devel builds except r-hub Fedora:

    0 errors | 0 warnings | 2 notes

* r-hub Fedora

    0 errors | 1 warning | 2 notes

### WARNINGS:

* **r-hub Fedora:**
  Only on RHub Fedora, I receive a warning building the vignette:

      Error: processing vignette 'policy_analysis.Rmd' failed with diagnostics:
      X11 font -adobe-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*, face 1 at size 11 could 
      not be loaded
  
    This warning is spurious and seems to have to do with the way X11 works 
    under the RHub headless Fedora installation. 
    There is nothing in the vignette that specifies specific fonts.

### NOTES: 

* False-positive warning on r-hub and winbuilder about possibly misspelled word 
  "Kaya". This word is spelled correctly.

* **r-hub only**: False-positive warning about (possibly) invalid URL 
  <https://www.eia.gov/outlooks/archive/ieo17/>.
  This only occurs on r-hub builder and not other builds. I have checked this 
  URL manually and it is correct and working when I visit it from a web 
  browser. 
  
    This URL is the underlying source of the data provided by this package
    (it's listed in "\source" lines in td_trends.Rd and td_values.Rd).
    This is a third-party web site operated by the U.S. Department of Energy
    and is not under my control.

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
