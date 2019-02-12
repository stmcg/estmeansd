#' Print method for objects of class "bc.mean.sd"
#'
#' Print method for objects of class "bc.mean.sd".
#'
#' @param x object of class "bc.mean.sd".
#' @param ... other arguments.
#' @return No value is returned.
#' @seealso \code{\link{bc.mean.sd}}
#' @examples
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 1000
#' x <- stats::rlnorm(n, 2.5, 1)
#' quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
#' obs.mean <- mean(x)
#' obs.sd <- stats::sd(x)
#'
#' ## Estimate the sample mean and standard deviation using the BC method
#' res <- bc.mean.sd(q1.val = quants[1], med.val = quants[2],
#'     q3.val = quants[3], n = n)
#' print(res)
#'
#' @export

## S3 method for class 'bc.mean.sd'
print.bc.mean.sd <- function(x, ...) {
  cat("$est.mean\n")
  print(x$est.mean)
  cat("\n$est.sd\n")
  print(x$est.sd)
}
