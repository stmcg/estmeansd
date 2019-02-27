#' Summary method for objects of class "qe.mean.sd"
#'
#' Summary method for objects of class "qe.mean.sd".
#'
#' @param object object of class "qe.mean.sd".
#' @param digits integer specifying the number of decimal places.
#' @param ... other arguments.
#' @return A \emph{5 x 3} matrix with columns for the estimated sample mean, estimated standard deviation, and sum of squares (of the objective function used in \code{\link{qe.fit}}) under each candidate distribution.
#' @seealso \code{\link{qe.mean.sd}}
#' @examples
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 100
#' x <- stats::rlnorm(n, 2.5, 1)
#' quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
#' obs.mean <- mean(x)
#' obs.sd <- stats::sd(x)
#'
#' ## Estimate the sample mean and standard deviation using the BC method
#' res <- qe.mean.sd(q1.val = quants[1], med.val = quants[2],
#'     q3.val = quants[3], n = n)
#' summary(res)
#'
#' @export

## S3 method for class 'qe.mean.sd'
summary.qe.mean.sd <- function(object, digits = 5, ...) {
  res.mat <- matrix(nrow = length(object$values), ncol = 3)
  rownames(res.mat) <- names(object$values[order(object$values)])
  colnames(res.mat) <- c("Mean", "SD", "SS")
  for (dist.name in row.names(res.mat)){
    res.mat[dist.name, "Mean"] <- object[[paste0(dist.name, ".est.mean")]]
    res.mat[dist.name, "SD"] <- object[[paste0(dist.name, ".est.sd")]]
    res.mat[dist.name, "SS"] <- unname(object$values[dist.name])
  }
  res.mat <- round(res.mat, digits = digits)
  return(res.mat)
}
