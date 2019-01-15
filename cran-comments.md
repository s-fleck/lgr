## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)
* rhub


## R CMD check results

0 errors | 0 warnings | 2 note

* This is a new release
* I am getting NOTEs because I am locking/unlocking bindings. I am using this
  mechanism because I want objects of one of the R6 classes in my package
  to be modifiable by a special function but not by accident by the user. I
  could just leave the R6 class unlocked from the beginning, but I would 
  prefer to keep the locking mechanism in place for additional safety if that 
  is no issue for CRAN.
