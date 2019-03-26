
<!-- README.md is generated from README.Rmd. Please edit that file -->

# estmeansd: Estimating the Sample Mean and Standard Deviation from Commonly Reported Quantiles in Meta-Analysis

[![Build\_Status](https://travis-ci.org/stmcg/estmeansd.svg?branch=master)](https://travis-ci.org/stmcg/estmeansd)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/estmeansd)](https://cran.r-project.org/package=estmeansd)
[![CRAN\_Download\_Badge](https://cranlogs.r-pkg.org/badges/estmeansd)](https://www.r-pkg.org/pkg/estmeansd)
[![CRAN\_Download\_Badge\_All](https://cranlogs.r-pkg.org/badges/grand-total/estmeansd)](https://www.r-pkg.org/pkg/estmeansd)

The `estmeansd` package implements the methods of [McGrath et
al. (2019)](https://arxiv.org/abs/1903.10498) for estimating the sample
mean and standard deviation from commonly reported quantiles in
meta-analysis. Specifically, these methods can be applied to studies
that report one of the following sets of summary statistics:

  - S1: median, minimum and maximum values, and sample size
  - S2: median, first and third quartiles, and sample size
  - S3: median, minimum and maximum values, first and third quartiles,
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

Specifically, this package implements the Box-Cox (BC) and Quantile
Estimation (QE) methods to estimate the sample mean and standard
deviation. The BC and QE methods can be applied using the `bc.mean.sd()`
and `qe.mean.sd()` functions, respectively:

``` r
library(estmeansd)
bc.mean.sd(min.val = 2, med.val = 4, max.val = 9, n = 100) # BC Method
#> $est.mean
#> [1] 4.204881
#> 
#> $est.sd
#> [1] 1.277236
qe.mean.sd(min.val = 2, med.val = 4, max.val = 9, n = 100) # QE Method
#> $est.mean
#> [1] 4.347284
#> 
#> $est.sd
#> [1] 1.502171
```
