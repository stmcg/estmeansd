
<!-- README.md is generated from README.Rmd. Please edit that file -->

# estmeansd: Estimating the Sample Mean and Standard Deviation from Commonly Reported Quantiles in Meta-Analysis

[![Build\_Status](https://travis-ci.org/stmcg/estmeansd.svg?branch=master)](https://travis-ci.org/stmcg/estmeansd)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/estmeansd)](https://cran.r-project.org/package=estmeansd)
[![CRAN\_Download\_Badge](https://cranlogs.r-pkg.org/badges/estmeansd)](https://www.r-pkg.org/pkg/estmeansd)
[![CRAN\_Download\_Badge\_All](https://cranlogs.r-pkg.org/badges/grand-total/estmeansd)](https://www.r-pkg.org/pkg/estmeansd)

The `estmeansd` package implements the methods of McGrath et al. (in
preparation) for estimating the sample mean and standard deviation from
commonly reported quantiles in meta-analysis. Specifically, these
methods can be applied to studies that report one of the following sets
of summary statistics:

  - S1: median, minimum and maximum values, and sample size
  - S2: median, first and third quartiles, and sample size
  - S3: median, minimum and maximum values, first and third quartiles,
    and sample size

Additionally, the shiny app
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
