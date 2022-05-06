#' Quantile estimation method for estimating the sample mean and standard deviation
#'
#' This function applies the quantile estimation (QE) method to estimate the sample mean and standard deviation from a study that presents one of the following sets of summary statistics: \itemize{
#' \item S1: median, minimum and maximum values, and sample size
#' \item S2: median, first and third quartiles, and sample size
#' \item S3: median, minimum and maximum values, first and third quartiles, and sample size
#'  }
#'
#' In brief, the QE method fits candidate distribution(s) by minimizing the distance between observed and distribution quantiles. See \code{\link{qe.fit}} for further details concerning the distribution fitting step. If multiple candidate distributions are fit, the distribution with the best fit (i.e., the fitted distribution obtaining the smallest distance between observed and distribution quantiles) is selected as the underlying outcome distribution. The mean and standard devition of the selected distribution are used to estimate the sample mean and standard deviation, respectively
#'
#' @param min.val numeric value giving the sample minimum.
#' @param q1.val numeric value giving the sample first quartile.
#' @param med.val numeric value giving the sample median.
#' @param q3.val numeric value giving the sample third quartile.
#' @param max.val numeric value giving the sample maximum.
#' @param n numeric value giving the sample size.
#' @param qe.fit.control optional list of control parameters for \code{\link{qe.fit}}.
#'
#' @return A list with the following components:
#' \item{est.mean}{Estimated sample mean.}
#' \item{est.sd}{Estimated sample standard deviation.}
#' \item{selected.dist}{Selected outcome distribution.}
#' \item{values}{Values of the objective functions evaluated at the estimated paramters of each candidate distribution.}
#' \item{...}{Some additional elements.}
#'
#' @references McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{29}(9):2520-2537.
#'
#' @examples
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 100
#' x <- stats::rlnorm(n, 2.5, 1)
#' quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
#' obs.mean <- mean(x)
#' obs.sd <- stats::sd(x)
#'
#' ## Estimate the sample mean and standard deviation using the QE method
#' qe.mean.sd(q1.val = quants[1], med.val = quants[2], q3.val = quants[3],
#'     n = n)
#'
#' @export

qe.mean.sd <- function(min.val, q1.val, med.val, q3.val, max.val, n,
                   qe.fit.control = list()) {
  args <- as.list(environment())
  x <- qe.fit(min.val = min.val, q1.val = q1.val, med.val = med.val,
              q3.val = q3.val, max.val = max.val, n = n,
              qe.fit.control = qe.fit.control)
  selected.dist <- names(which.min(x$values))
  ests <- get.mean.sd(x, selected.dist)
  output <- list(est.mean = ests$est.mean, est.sd = ests$est.sd,
                 selected.dist = selected.dist, values = x$values,
                 args = args, scenario = x$scenario, fitted.dists = x)
  for (dist.name in names(x$values)){
    ests.all <- get.mean.sd(x, dist.name)
    output[paste0(dist.name, '.est.mean')] <- ests.all$est.mean
    output[paste0(dist.name, '.est.sd')] <- ests.all$est.sd
  }
  class(output) <- "qe.mean.sd"
  return(output)
}

