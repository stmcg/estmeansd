
<!-- README.md is generated from README.Rmd. Please edit that file -->

# estmeansd: Estimating the Sample Mean and Standard Deviation from Commonly Reported Quantiles in Meta-Analysis

[![Build_Status](https://travis-ci.org/stmcg/estmeansd.svg?branch=master)](https://travis-ci.org/stmcg/estmeansd)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/estmeansd)](https://cran.r-project.org/package=estmeansd)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/estmeansd)](https://www.r-pkg.org/pkg/estmeansd)
[![CRAN_Download_Badge_All](https://cranlogs.r-pkg.org/badges/grand-total/estmeansd)](https://www.r-pkg.org/pkg/estmeansd)

The `estmeansd` package implements the methods of [McGrath et
al. (2020)](https://journals.sagepub.com/doi/full/10.1177/0962280219889080)
and [Cai et
al. (2021)](https://journals.sagepub.com/doi/full/10.1177/09622802211047348)
for estimating the sample mean and standard deviation from commonly
reported quantiles in meta-analysis. Specifically, these methods can be
applied to studies that report one of the following sets of summary
statistics:

-   S1: median, minimum and maximum values, and sample size
-   S2: median, first and third quartiles, and sample size
-   S3: median, minimum and maximum values, first and third quartiles,
    and sample size

Additionally, the Shiny app
[estmeansd](https://smcgrath.shinyapps.io/estmeansd/) implements these
methods.

## Installation

You can install the released version of `estmeansd` from CRAN with:

``` r
install.packages("estmeansd")
```

After installing the `devtools` package (i.e., calling
`install.packages(devtools)`), the development version of `estmeansd`
can be installed from GitHub with:

``` r
devtools::install_github("stmcg/estmeansd")
```

## Usage

Specifically, this package implements the Box-Cox (BC), Quantile
Estimation (QE), and Method for Unknown Non-Normal Distributions (MLN)
approaches to estimate the sample mean and standard deviation. The BC,
QE, and MLN methods can be applied using the `bc.mean.sd()`
`qe.mean.sd()`, and `mln.mean.sd()` functions, respectively:

``` r
library(estmeansd)
set.seed(1)
bc.mean.sd(min.val = 2, med.val = 4, max.val = 9, n = 100) # BC Method
#> $est.mean
#> [1] 4.210971
#> 
#> $est.sd
#> [1] 1.337348
qe.mean.sd(min.val = 2, med.val = 4, max.val = 9, n = 100) # QE Method
#> $est.mean
#> [1] 4.347284
#> 
#> $est.sd
#> [1] 1.502171
mln.mean.sd(min.val = 2, med.val = 4, max.val = 9, n = 100) # MLN Method
#> $est.mean
#> [1] 4.195238
#> 
#> $est.sd
#> [1] 1.294908
```
