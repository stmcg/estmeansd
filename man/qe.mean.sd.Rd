% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qe.mean.sd.R
\name{qe.mean.sd}
\alias{qe.mean.sd}
\title{Quantile estimation method for estimating the sample mean and standard deviation}
\usage{
qe.mean.sd(
  min.val,
  q1.val,
  med.val,
  q3.val,
  max.val,
  n,
  qe.fit.control = list()
)
}
\arguments{
\item{min.val}{numeric value giving the sample minimum.}

\item{q1.val}{numeric value giving the sample first quartile.}

\item{med.val}{numeric value giving the sample median.}

\item{q3.val}{numeric value giving the sample third quartile.}

\item{max.val}{numeric value giving the sample maximum.}

\item{n}{numeric value giving the sample size.}

\item{qe.fit.control}{optional list of control parameters for \code{\link{qe.fit}}.}
}
\value{
A list with the following components:
\item{est.mean}{Estimated sample mean.}
\item{est.sd}{Estimated sample standard deviation.}
\item{selected.dist}{Selected outcome distribution.}
\item{values}{Values of the objective functions evaluated at the estimated paramters of each candidate distribution.}
\item{...}{Some additional elements.}
}
\description{
This function applies the quantile estimation (QE) method to estimate the sample mean and standard deviation from a study that presents one of the following sets of summary statistics: \itemize{
\item S1: median, minimum and maximum values, and sample size
\item S2: median, first and third quartiles, and sample size
\item S3: median, minimum and maximum values, first and third quartiles, and sample size
 }
}
\details{
In brief, the QE method fits candidate distribution(s) by minimizing the distance between observed and distribution quantiles. See \code{\link{qe.fit}} for further details concerning the distribution fitting step. If multiple candidate distributions are fit, the distribution with the best fit (i.e., the fitted distribution obtaining the smallest distance between observed and distribution quantiles) is selected as the underlying outcome distribution. The mean and standard devition of the selected distribution are used to estimate the sample mean and standard deviation, respectively
}
\examples{
## Generate S2 summary data
set.seed(1)
n <- 100
x <- stats::rlnorm(n, 2.5, 1)
quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
obs.mean <- mean(x)
obs.sd <- stats::sd(x)

## Estimate the sample mean and standard deviation using the QE method
qe.mean.sd(q1.val = quants[1], med.val = quants[2], q3.val = quants[3],
    n = n)

}
\references{
McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{29}(9):2520-2537.
}
