#' Parametric bootstrap standard error estimation
#'
#' A generic function for computing parametric bootstrap standard error (SE) estimates.
#'
#' @param x an object for which a standard error estimate is desired
#' @param ... other arguments.
#' @return A list with the following components:
#' \item{est.se}{Estimated standard error of the mean estimator.}
#' \item{boot.means}{Bootstrap replicates of the mean estimates.}
#' \item{boot.sds}{Bootstrap replicates of the standard deviation estimates.}
#'
#' @references McGrath S., Katzenschlager S., Zimmer A.J., Seitel A., Steele R., Benedetti A. (In preparation). Standard error estimation in meta-analysis of studies reporting medians.
#'
#' @export
get_SE <- function(x, ...){
  UseMethod("get_SE")
}

#' Parametric bootstrap standard error estimation for the quantile estimation approach
#'
#' Computes a parametric bootstrap standard error estimate for the quantile estimation approach.
#'
#' @param x object of class "qe.mean.sd".
#' @param nboot numeric value giving the number of bootstrap replicates. The default is \code{1000}.
#' @param shift.when.negative logical scalar indicating whether to add a constant to the generated sample quantiles if the smallest quantile (i.e., the minimum value in scenarios S1 and S3, the first quartile in scenario S2) is negative. When this argument is set to \code{TRUE}, a constant is added such that the smallest quantile equals \code{shift.val}. The default is \code{TRUE}.
#' @param shift.val numeric value to which the smallest quantile should be shifted to if it is negative (see argument \code{shift.when.negative}). This argument is only applicable when \code{shift.when.negative} is set to \code{TRUE}. The default is \code{0.5}.
#' @param ... other arguments.
#' @return A list with the following components:
#' \item{est.se}{Estimated standard error of the mean estimator.}
#' \item{boot.means}{Bootstrap replicates of the mean estimates.}
#' \item{boot.sds}{Bootstrap replicates of the standard deviation estimates.}
#'
#' @references McGrath S., Katzenschlager S., Zimmer A.J., Seitel A., Steele R., Benedetti A. (In preparation). Standard error estimation in meta-analysis of studies reporting medians.
#' @references McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{29}(9):2520-2537.
#'
#' @examples
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 250
#' x <- stats::rlnorm(n, 5, 0.25)
#' quants <- stats::quantile(x, probs = c(0, 0.5, 1))
#'
#' ## Estimate the mean and its standard error
#' res <- qe.mean.sd(min.val = quants[1], med.val = quants[2], max.val = quants[3],
#'                   n = n)
#' get_SE(res)$est.se
#' @seealso \code{\link{qe.mean.sd}}
#' @export
get_SE.qe.mean.sd <- function(x, nboot = 1000, shift.when.negative = TRUE,
                              shift.val = 0.5, ...){
  boot_means <- boot_sds <- rep(NA, nboot)
  for (i in 1:nboot){
    if (x$selected.dist == 'normal'){
      dat <- stats::rnorm(n = x$args$n, mean = x$fitted.dists$norm.par[1],
                          sd = x$fitted.dists$norm.par[2])
    } else if (x$selected.dist == 'log-normal'){
      dat <- stats::rlnorm(n = x$args$n, meanlog = x$fitted.dists$lnorm.par[1],
                           sdlog = x$fitted.dists$lnorm.par[2])
    } else if (x$selected.dist == 'gamma'){
      dat <- stats::rgamma(n = x$args$n, shape = x$fitted.dists$gamma.par[1],
                           rate = x$fitted.dists$gamma.par[2])
    } else if (x$selected.dist == 'weibull'){
      dat <- stats::rweibull(n = x$args$n, shape = x$fitted.dists$weibull.par[1],
                             scale = x$fitted.dists$weibull.par[2])
    } else if (x$selected.dist == 'beta'){
      dat <- stats::rbeta(n = x$args$n, shape1 = x$fitted.dists$beta.par[1],
                          shape2 = x$fitted.dists$beta.par[2])
    }
    q_res <- get_quants(dat = dat, scenario = x$scenario,
                        shift.when.negative = shift.when.negative,
                        shift.val = shift.val)
    if (x$scenario == 'S1'){
      fit <- qe.mean.sd(min.val = q_res$quants[1], med.val = q_res$quants[2],
                        max.val = q_res$quants[3], n = x$args$n,
                        qe.fit.control = x$args$qe.fit.control)
    } else if (x$scenario == 'S2'){
      fit <- qe.mean.sd(q1.val = q_res$quants[1], med.val = q_res$quants[2],
                        q3.val = q_res$quants[3], n = x$args$n,
                        qe.fit.control = x$args$qe.fit.control)
    } else if (x$scenario == 'S3'){
      fit <- qe.mean.sd(min.val = q_res$quants[1], q1.val = q_res$quants[2],
                        med.val = q_res$quants[3], q3.val = q_res$quants[4],
                        max.val = q_res$quants[5], n = x$args$n,
                        qe.fit.control = x$args$qe.fit.control)
    }
    boot_means[i] <- fit$est.mean - q_res$shift.param
    boot_sds[i] <- fit$est.sd
  }
  est.se <- stats::sd(boot_means)
  return(list(est.se = est.se, boot_means = boot_means, boot_sds = boot_sds))
}

#' Parametric bootstrap standard error estimation for the box-cox approach
#'
#' Computes a parametric bootstrap standard error estimate for the box-cox approach.
#'
#' @param x object of class "bc.mean.sd".
#' @param nboot numeric value giving the number of bootstrap replicates. The default is \code{1000}.
#' @param shift.when.negative logical scalar indicating whether to add a constant to the generated sample quantiles if the smallest quantile (i.e., the minimum value in scenarios S1 and S3, the first quartile in scenario S2) is negative. When this argument is set to \code{TRUE}, a constant is added such that the smallest quantile equals \code{shift.val}. The default is \code{TRUE}.
#' @param shift.val numeric value to which the smallest quantile should be shifted to if it is negative (see argument \code{shift.when.negative}). This argument is only applicable when \code{shift.when.negative} is set to \code{TRUE}. The default is \code{0.5}.
#' @param ... other arguments.
#' @return A list with the following components:
#' \item{est.se}{Estimated standard error of the mean estimator.}
#' \item{boot.means}{Bootstrap replicates of the mean estimates.}
#' \item{boot.sds}{Bootstrap replicates of the standard deviation estimates.}
#'
#' @references McGrath S., Katzenschlager S., Zimmer A.J., Seitel A., Steele R., Benedetti A. (In preparation). Standard error estimation in meta-analysis of studies reporting medians.
#' @references McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{29}(9):2520-2537.
#'
#' @examples
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 250
#' x <- stats::rlnorm(n, 5, 0.25)
#' quants <- stats::quantile(x, probs = c(0, 0.5, 1))
#'
#' ## Estimate the mean and its standard error
#' res <- bc.mean.sd(min.val = quants[1], med.val = quants[2], max.val = quants[3],
#'                   n = n)
#' get_SE(res)$est.se
#' @seealso \code{\link{bc.mean.sd}}
#' @export
get_SE.bc.mean.sd <- function(x, nboot = 1000, shift.when.negative = TRUE,
                              shift.val = 0.5, ...){
  boot_means <- boot_sds <- rep(NA, nboot)
  for (i in 1:nboot){
    dat_trans <- stats::rnorm(n = x$args$n, mean = x$location, sd = x$scale)
    dat <- inv.smooth.bc.transform(transformed.vals = dat_trans, lambda = x$shape)
    q_res <- get_quants(dat = dat, scenario = x$scenario,
                        shift.when.negative = shift.when.negative,
                        shift.val = shift.val)
    if (x$scenario == 'S1'){
      fit <- bc.mean.sd(min.val = q_res$quants[1], med.val = q_res$quants[2],
                        max.val = q_res$quants[3], n = x$args$n,
                        preserve.tail = x$args$preserve.tail,
                        avoid.mc = x$args$avoid.mc)
    } else if (x$scenario == 'S2'){
      fit <- bc.mean.sd(q1.val = q_res$quants[1], med.val = q_res$quants[2],
                        q3.val = q_res$quants[3], n = x$args$n,
                        preserve.tail = x$args$preserve.tail,
                        avoid.mc = x$args$avoid.mc)
    } else if (x$scenario == 'S3'){
      fit <- bc.mean.sd(min.val = q_res$quants[1], q1.val = q_res$quants[2],
                        med.val = q_res$quants[3], q3.val = q_res$quants[4],
                        max.val = q_res$quants[5], n = x$args$n,
                        preserve.tail = x$args$preserve.tail,
                        avoid.mc = x$args$avoid.mc)
    }
    boot_means[i] <- fit$est.mean - q_res$shift.param
    boot_sds[i] <- fit$est.sd
  }
  est.se <- stats::sd(boot_means)
  return(list(est.se = est.se, boot_means = boot_means, boot_sds = boot_sds))
}

#' Parametric bootstrap standard error estimation for the method for unknown non-normal distributions approach
#'
#' Computes a parametric bootstrap standard error estimate for the method for unknown non-normal distributions approach.
#'
#' @param x object of class "mln.mean.sd".
#' @param nboot numeric value giving the number of bootstrap replicates. The default is \code{1000}.
#' @param shift.when.negative logical scalar indicating whether to add a constant to the generated sample quantiles if the smallest quantile (i.e., the minimum value in scenarios S1 and S3, the first quartile in scenario S2) is negative. When this argument is set to \code{TRUE}, a constant is added such that the smallest quantile equals \code{shift.val}. The default is \code{TRUE}.
#' @param shift.val numeric value to which the smallest quantile should be shifted to if it is negative (see argument \code{shift.when.negative}). This argument is only applicable when \code{shift.when.negative} is set to \code{TRUE}. The default is \code{0.5}.
#' @param ... other arguments.
#' @return A list with the following components:
#' \item{est.se}{Estimated standard error of the mean estimator.}
#' \item{boot.means}{Bootstrap replicates of the mean estimates.}
#' \item{boot.sds}{Bootstrap replicates of the standard deviation estimates.}
#'
#' @references McGrath S., Katzenschlager S., Zimmer A.J., Seitel A., Steele R., Benedetti A. (In preparation). Standard error estimation in meta-analysis of studies reporting medians.
#' @references Cai S., Zhou J., and Pan J. (2021). Estimating the sample mean and standard deviation from order statistics and sample size in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{30}(12):2701-2719.
#'
#' @examples
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 250
#' x <- stats::rlnorm(n, 5, 0.25)
#' quants <- stats::quantile(x, probs = c(0, 0.5, 1))
#'
#' ## Estimate the mean and its standard error
#' res <- mln.mean.sd(min.val = quants[1], med.val = quants[2], max.val = quants[3],
#'                   n = n)
#' get_SE(res)$est.se
#' @seealso \code{\link{mln.mean.sd}}
#' @export
get_SE.mln.mean.sd <- function(x, nboot = 1000, shift.when.negative = TRUE,
                               shift.val = 0.5, ...){
  boot_means <- boot_sds <- rep(NA, nboot)
  for (i in 1:nboot){
    dat_trans <- stats::rnorm(n = x$args$n, mean = x$location, sd = x$scale)
    dat <- inv.smooth.bc.transform(transformed.vals = dat_trans, lambda = x$shape)
    q_res <- get_quants(dat = dat, scenario = x$scenario,
                         shift.when.negative = shift.when.negative,
                         shift.val = shift.val)
    if (x$scenario == 'S1'){
      fit <- mln.mean.sd(min.val = q_res$quants[1], med.val = q_res$quants[2],
                        max.val = q_res$quants[3], n = x$args$n)
    } else if (x$scenario == 'S2'){
      fit <- mln.mean.sd(q1.val = q_res$quants[1], med.val = q_res$quants[2],
                        q3.val = q_res$quants[3], n = x$args$n)
    } else if (x$scenario == 'S3'){
      fit <- mln.mean.sd(min.val = q_res$quants[1], q1.val = q_res$quants[2],
                        med.val = q_res$quants[3], q3.val = q_res$quants[4],
                        max.val = q_res$quants[5], n = x$args$n)
    }
    boot_means[i] <- fit$est.mean - q_res$shift.param
    boot_sds[i] <- fit$est.sd
  }
  est.se <- stats::sd(boot_means)
  return(list(est.se = est.se, boot_means = boot_means, boot_sds = boot_sds))
}

get_quants <- function(dat, scenario, shift.when.negative, shift.val){
  if (scenario == 'S1'){
    quants <- unname(stats::quantile(dat, probs = c(0, 0.5, 1)))
  } else if (scenario == 'S2'){
    quants <- unname(stats::quantile(dat, probs = c(0.25, 0.5, 0.75)))
  } else if (scenario == 'S3'){
    quants <- unname(stats::quantile(dat, probs = c(0, 0.25, 0.5, 0.75, 1)))
  }
  if (shift.when.negative){
    shift.param <- ifelse(quants[1] <= 0, shift.val - quants[1], 0)
  } else {
    shift.param <- 0
  }
  quants <- quants + shift.param
  return(list(quants = quants, shift.param = shift.param))
}
