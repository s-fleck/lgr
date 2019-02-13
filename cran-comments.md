## Test environments
* ubuntu 18.04, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* ubuntu 14.04 (remote RStudio server), R 3.4.3
* win-builder (devel and release)
* rhub (rhub::check_for_cran())


## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a resubmission that removes non-breaking-space characters from the
  .rd files for R6 classes, which caused problems with the pdf manual.
