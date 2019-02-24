#' Plot method for objects of class "qe.fit"
#'
#' This function plots the cumulative distribution functions of the fitted distributions along with the summary data reported by the study (i.e., the S1, S2, or S3 data).
#'
#' Users may need to use the \code{limits} argument so that a sensible interval is used for plotting. By default, the limits of the interval are the sample minimum and maximum values in scenarios S1 and S3, which is often adequate. In scenario S2, the limits are based on the quantiles of the distribution with the best fit (i.e., the fitted distribution obtaining the smallest distance between observed and distribution quantiles). If the normal distribution is the best fit, the limits of the interval are the 1/n th quantile and 1-1/n th quantile of the fitted normal distribution. If any of the other distributions are selected, the 1/n th quantile and 0.90 quantile are used as the limits of the interval. Depending on the skewness of the data, users may need to adjust the upper limit.
#'
#' @param x object of class "qe.fit".
#' @param distributions character vector specifying the names of distributions to be plotted. The options are: \code{"normal"}, \code{"log-normal"}, \code{"gamma"}, \code{"weibull"}, and \code{"beta"}. By default, the normal, log-normal, gamma, and Weibull distributions are plotted. If one of the specified distributions was not successfully fit (e.g., \code{\link{qe.fit}} failed to converge for the given distribution), the distribution will not be included in the plot.
#' @param points logical scalar indicating whether to plot the observed summary data (i.e., the sample quantiles reported by the study). The default is \code{TRUE}.
#' @param limits numeric vector of length 2 specifying the bounds of the interval in which to evaluate of the cumulative distribution functions.  See 'Details'.
#' @param col character vector specifying the colors to use for the distributions. The kth element in this vector corresponds to the color to be used for the kth element in the \code{distribution} argument.
#' @param legend logical scalar indicating whether to plot a legend. The default is \code{TRUE}.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param ylim y-axis limits.
#' @param cex.points The magnification to be used for the plotted observed summary data (i.e., for the \code{points} argument).
#' @param pch.points either an integer specifying a symbol or a single character to be used as the default in plotting the observed summary data (i.e., for the \code{points} argument). See \code{\link[graphics]{points}} for possible values and their interpretation.
#' @param length.out numeric scalar specifying the number of points to be used for evaluating each of the cumulative distribution functions.
#' @param ... other graphical parameters (see \code{\link[graphics]{par}}).
#' @seealso \code{\link{qe.fit}}
#' @examples
#' ## Example 1
#' ## Generate S3 summary data
#' set.seed(1)
#' n <- 100
#' x <- stats::rlnorm(n, 2.5, 1)
#' quants <- stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
#'
#' ## Fit distributions
#' res <- qe.fit(min.val = quants[1], q1.val = quants[2], med.val = quants[3],
#'        q3.val = quants[4], max.val = quants[5], n = n)
#' plot(res)
#'
#' ## Example 2
#' res <- qe.fit(q1.val = 1, med.val = 2, q3.val = 3, n = 100)
#' plot(res, limits = c(0, 5))
#'
#' @export

## S3 method for class 'qe.fit'
plot.qe.fit <- function(x, distributions = c("normal", "log-normal", "gamma",
                                             "weibull"),
                        points = TRUE, limits, col, legend = TRUE,
                        xlab = 'x', ylab = 'F(x)', ylim = c(0, 1),
                        cex.points = 0.85, pch.points = 16, length.out = 1e4,
                        ...) {

  if (!inherits(x, "qe.fit")){
    stop("Argument 'x' must be an object of class \"qe.fit\".")
  }

  if(all(is.na(x$values))){
    stop("Plotting fitted distributions is not possible because no
         distributions were fit.")
  }
  dists.fitted <- names(x$values[!is.na(x$values)])
  dists.to.plot <- distributions[distributions %in% dists.fitted]

  if(length(dists.to.plot) == 0){
    stop("Plotting fitted distributions is not possible because none
         of the specified distributions were fit")
  }

  if (missing(col)){
    col <- grDevices::rainbow(length(dists.to.plot))
  } else {
    if (length(col) != length(distributions)){
      stop("Length of 'distributions' and length of 'col' must be equal.")
    }
    col <- col[distributions %in% dists.fitted]
  }
  names(col) <- dists.to.plot

  if (missing(limits)){
    if (all(c("min.val", "max.val") %in% names(x$num.input))){
      limits <- c(x$num.input$min.val, x$num.input$max.val)
    } else{
      best.fit <- names(which.min(x$values))
      if (best.fit == "normal"){
        quants <- c(1/x$num.input$n, 1-1/x$num.input$n)
      } else {
        quants <- c(1/x$num.input$n, min(1-1/x$num.input$n, 0.9))
      }
      if (best.fit == "normal"){
        limits <- stats::qnorm(quants, mean = x$norm.par['mu'],
                               sd = x$norm.par['sigma'])
      } else if (best.fit == "log-normal"){
        limits <- stats::qlnorm(quants, meanlog = x$lnorm.par['mu'],
                                sdlog = x$lnorm.par['sigma'])
      } else if (best.fit == "gamma"){
        limits <- stats::qgamma(quants, shape = x$gamma.par['shape'],
                                rate = x$gamma.par['rate'])
      } else if (best.fit == "weibull"){
        limits <- stats::qweibull(quants, shape = x$weibull.par['shape'],
                                  scale = x$weibull.par['scale'])
      } else if (best.fit == "beta"){
        limits <- stats::qbeta(quants, shape1 = x$beta.par['shape1'],
                               shape2 = x$beta.par['shape2'])
      }
    }
  }

  x.vals <- seq(limits[1], limits[2], length.out = length.out)
  y.norm <- stats::pnorm(x.vals, x$norm.par[1], x$norm.par[2])
  y.lnorm <- stats::plnorm(x.vals, x$lnorm.par[1], x$lnorm.par[2])
  y.gamma <- stats::pgamma(x.vals, x$gamma.par[1], x$gamma.par[2])
  y.weibull <- stats::pweibull(x.vals, x$weibull.par[1], x$weibull.par[2])
  y.beta <- stats::pbeta(x.vals, x$beta.par[1], x$beta.par[2])

  plotted <- FALSE
  if ("normal" %in% dists.to.plot){
    graphics::plot(x.vals, y.norm, type = "l", col = col['normal'], ylim = ylim,
                   xlab = xlab, ylab = ylab, ...)
    plotted <- TRUE
  }
  if ("log-normal" %in% dists.to.plot){
    if (!plotted){
      graphics::plot(x.vals, y.lnorm, type = "l", col = col['log-normal'],
                     ylim = ylim, xlab = xlab, ylab = ylab, ...)
    } else {
      graphics::lines(x.vals, y.lnorm, type = 'l', col = col['log-normal'], ...)
    }
    plotted <- TRUE
  }
  if ("gamma" %in% dists.to.plot){
    if (!plotted){
      graphics::plot(x.vals, y.gamma, type = "l", col = col['gamma'],
                     ylim = ylim, xlab = xlab, ylab = ylab, ...)
    } else {
      graphics::lines(x.vals, y.gamma, type = "l", col = col['gamma'], ...)
    }
    plotted <- TRUE
  }
  if ("weibull" %in% dists.to.plot){
    if (!plotted){
      graphics::plot(x.vals, y.weibull, type = "l", col = col['weibull'],
                     ylim = ylim, xlab = xlab, ylab = ylab, ...)
    } else {
      graphics::lines(x.vals, y.weibull, type = "l", col = col['weibull'], ...)
    }
    plotted <- TRUE
  }
  if ("beta" %in% dists.to.plot){
    if (!plotted){
      graphics::plot(x.vals, y.beta, type = "l", col = col['beta'],
                     ylim = ylim, xlab = xlab, ylab = ylab, ...)
    } else {
      graphics::lines(x.vals, y.beta, type = "l", col = col['beta'], ...)
    }
  }
  if (points){
    if ("min.val" %in% names(x$num.input)){
      graphics::points(x$num.input$min.val, 1/x$num.input$n, pch=pch.points,
                       cex=cex.points)
    }
    if ("q1.val" %in% names(x$num.input)){
      graphics::points(x$num.input$q1.val, 0.25, pch=pch.points, cex=cex.points)
    }
    if ("med.val" %in% names(x$num.input)){
      graphics::points(x$num.input$med.val, 0.5, pch=pch.points, cex=cex.points)
    }
    if ("q3.val" %in% names(x$num.input)){
      graphics::points(x$num.input$q3.val, 0.75, pch=pch.points, cex=cex.points)
    }
    if ("max.val" %in% names(x$num.input)){
      graphics::points(x$num.input$max.val, 1-1/x$num.input$n, pch=pch.points,
                       cex=cex.points)
    }
  }
  if (legend){
    legend(x.vals[round(length(x.vals)*0.7)], 0.55,
           sapply(dists.to.plot, simpleCap),
           col=col, lty = 1, bty='n')
  }
}
