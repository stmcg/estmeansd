Package Updates
---------------

### Changes in Version 0.2.1 (2020-08-12)

-   Fixed a minor error in `bc.mean.sd()`. The function previously
    failed to calculate the sample mean when the following set of
    conditions held: `preserve.tail` was set to `FALSE`, `avoid.mc` was
    set to `TRUE`, and the shape parameter was of magnitude less than
    0.01.
-   Updated references

### Changes in Version 0.2.0 (2019-03-26)

-   Begun using Git as a version-control system and hosting repository
    on GitHub (<https://github.com/stmcg/estmeansd>) for the development
    version of the package
-   Added `plot.qe.fit()`, an S3 plot method for objects returned by
    `qe.fit()`. Consequently, added ‘graphics’ and ‘grDevices’ to
    Imports
-   Added `summary.qe.mean.sd()`, an S3 summary method for objects
    returned by `qe.mean.sd()`
-   Removed prior on the power parameter lambda in `bc.mean.sd()`
-   Added bounds on the sample size in `bc.mean.sd()` and `qe.fit()`
-   Added URL and BugReport webpage to the DESCRIPTION file
-   Added README.md file
-   Added NEWS.md file to track changes
-   Minor updates to documentation

### Changes in Version 0.1.1 (2019-02-04)

-   No longer imports the ‘metamedian’ package. Consequently, the
    internal function `get.scenario()` was written
-   Renamed `two_sample_default` argument to `two.sample.default`

### Changes in Version 0.1.0 (2019-01-11)

-   First version released on CRAN
