
<!-- README.md is generated from README.Rmd. Please edit that file -->

# estmeansd: Estimating the Sample Mean and Standard Deviation from Commonly Reported Quantiles in Meta-Analysis

[![CRAN_Status_Badge](https://badges.cranchecks.info/worst/estmeansd.svg)](https://cran.r-project.org/package=estmeansd)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/estmeansd)](https://www.r-pkg.org/pkg/estmeansd)
[![CRAN_Download_Badge_All](https://cranlogs.r-pkg.org/badges/grand-total/estmeansd)](https://www.r-pkg.org/pkg/estmeansd)

The `estmeansd` package implements the methods of [McGrath et
al. (2020)](https://doi.org/10.1177/0962280219889080) and [Cai et
al. (2021)](https://doi.org/10.1177/09622802211047348) for estimating
the sample mean and standard deviation from commonly reported quantiles
in meta-analysis. Specifically, these methods can be applied to studies
that report one of the following sets of summary statistics:

- S1: median, minimum and maximum values, and sample size
- S2: median, first and third quartiles, and sample size
- S3: median, minimum and maximum values, first and third quartiles, and
  sample size

This package also implements the methods described by [McGrath et
al. (2023)](https://doi.org/10.1177/09622802221139233) to estimate the
standard error of these mean and standard deviation estimators. The
estimated standard errors are needed for computing the weights in
conventional inverse-variance weighted meta-analysis approaches.

Additionally, the Shiny app
[estmeansd](https://smcgrath.shinyapps.io/estmeansd/) implements these
methods.

Note that the R package
[metamedian](https://CRAN.R-project.org/package=metamedian) can apply
these methods (as well as several others) to perform a meta-analysis.
See [McGrath et al. (in press)](https://doi.org/10.1002/jrsm.1686) for a
guide on using the `metamedian` package.

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

# BC Method
res_bc <- bc.mean.sd(min.val = 2, med.val = 4, max.val = 9, n = 100)
res_bc 
#> $est.mean
#> [1] 4.210971
#> 
#> $est.sd
#> [1] 1.337348

# QE Method
res_qe <- qe.mean.sd(min.val = 2, med.val = 4, max.val = 9, n = 100)
res_qe
#> $est.mean
#> [1] 4.347284
#> 
#> $est.sd
#> [1] 1.502171

# MLN Method
res_mln <- mln.mean.sd(min.val = 2, med.val = 4, max.val = 9, n = 100) 
res_mln
#> $est.mean
#> [1] 4.195238
#> 
#> $est.sd
#> [1] 1.294908
```

To estimate the standard error of these mean estimators, we can apply
the `get_SE()` function as follows:

``` r
# BC Method
res_bc_se <- get_SE(res_bc)
res_bc_se$est.se
#> [1] 0.1649077

# QE Method
res_qe_se <- get_SE(res_qe)
res_qe_se$est.se
#> [1] 0.2391081

# MLN Method
res_mln_se <- get_SE(res_mln)
res_mln_se$est.se
#> [1] 0.1505351
```
