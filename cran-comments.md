## Test environments
* local Windows 10 install, R 3.5.1
* local ubuntu 18.04.2 install, R 3.5.3
* ubuntu 14.04.5 (on travis-ci), R 3.5.2
* r-hub builder
    * ubuntu 16.04, R-devel
    * Windows Server 2008 R2 SP1, R-release, 32/64 bit
    * Fedora Linux, R-devel gcc
    * macOS 10.11 El Capitan, R-release
* winbuilder, devel, release, oldrelease

## R CMD check results

* Local installs and R-release builds on travis-ci and r-hub for MacOS:

    0 errors | 0 warnings | 0 notes

* r-hub: Windows Server R-Release, Fedora R-Devel gcc, 

    0 errors | 0 warnings | 1 note
  
### NOTES: 

* False-positive warning on r-hub and winbuilder about a possibly misspelled 
  word in hte DESCRIPTION file:
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

* I first submitted in October 2018 and received the following comments from
  Uwe Ligges, which I have addressed in this re-submission:
  
    * Thanks, please explain what kaya is, i.e. by a citation  in the form 
      Authors (year) <doi:10....> or some web reference in the form <http....>.

        * I have added both an explanation of the Kaya identity and a citation
          to published references for the identity and also for the sources of
          the data included in this package.
    
    * We see: `checking examples ... NONE`. Please add examples and resubmit.
    
        * I have added examples for all of the functions exported by this 
          package.


## Reverse dependencies

This is a new release, so there are no reverse dependencies.
