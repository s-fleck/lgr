## Test environments
* ubuntu 18.04, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.5.2
* ubuntu 14.04 (remote RStudio server), R 3.6.1
* win-builder (devel and release)
* rhub (rhub::check_for_cran())


## R CMD check results

0 errors | 0 warnings | 0 notes

Adds appender that can write to syslog. This introduces an optional dependency 
on the linux-only rsyslog package, so checks fail on windows if suggests are 
forced. Running the tests/examples of this package will write a few lines to
the syslog on linux systems, if this is an undesired side effect I can disable 
the tests.
