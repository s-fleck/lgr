# Yog

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

yog is yet another logging package for R that is build on the back of
R6 and data.table. The difference to other logging packages is that yog
maintains the log in memory as a data.table. That does not mean that it lacks
the facility to log to the console or to logfiles.


Storing the log as a data.table gives us the following advantages:

* You always have the whole log of the current R session available
* You can subset and search the log with data.table or data.frame syntax, one
  of the things particularily good at R
* Logging can be lighning fast if you don't define any appenders 
  (at the cost of beeing lost if R is terminated)
* Custom appenders (f.e. that write to databases) are very easy to implement.
* A cached 

This package is under heavy development right now, come back later.


## Installation

``` r
devtools::install.github("s-fleck/yog")
```
