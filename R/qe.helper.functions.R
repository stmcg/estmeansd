get.mean.sd <- function(x, family) {
  if (!(family %in% c("normal", "log-normal", "gamma", "weibull", "beta"))) {
    stop("family must be either normal, log-normal, gamma, Weibull, or beta.")
  }
  if (family == "normal") {
    par <- unname(x$norm.par)
    est.mean <- par[1]
    est.sd <- par[2]
  }
  else if (family == "log-normal") {
    par <- unname(x$lnorm.par)
    est.mean <- exp(par[1] + par[2]^2 / 2)
    est.sd <- sqrt((exp(par[2]^2) - 1) * exp(2 * par[1] + par[2]^2))
  }
  else if (family == "gamma") {
    par <- unname(x$gamma.par)
    est.mean <- par[1] / par[2]
    est.sd <- sqrt(par[1] / (par[2]^2))
  }
  else if (family == "weibull") {
    par <- unname(x$weibull.par)
    est.mean <- par[2]* gamma(1 + 1/par[1])
    est.sd <- sqrt(par[2]^2 * (gamma(1 + 2 / par[1]) -
                                 (gamma(1 + 1 / par[1]))^2))
  }
  else if (family == "beta") {
    par <- unname(x$beta.par)
    est.mean <- par[1]/(par[1]+par[2])
    est.sd <- sqrt(par[1] * par[2] / ((par[1] + par[2])^2 *
                                        (par[1] + par[2] + 1)))
  }
  return(list(est.mean = est.mean, est.sd = est.sd))
}

set.qe.fit.control <- function(quants, n, scenario, twosample_default){
  con <- list()

  if (scenario == "S1" | scenario == "S2") {
    con$norm.mu.bounds <- c(quants[1], quants[3])
    med.val <- quants[2]
    if (min(quants) > 0) {
      con$lnorm.mu.start <- log(med.val)
      con$lnorm.mu.bounds <- c(log(quants[1]), log(quants[3]))
    }
  }
  if (scenario == "S3") {
    con$norm.mu.bounds <- c(quants[2], quants[4])
    med.val <- quants[3]
    if (min(quants) > 0) {
      con$lnorm.mu.start <- log(med.val)
      con$lnorm.mu.bounds <- c(log(quants[2]), log(quants[4]))
    }
  }

  con$norm.sigma.bounds <- c(1e-3, 50)
  con$lnorm.sigma.bounds = c(1e-3, 10)

  if (twosample_default){
    con$norm.mu.start = med.val
    con$norm.sigma.start = 1
    con$lnorm.sigma.start = 1
    con$gamma.shape.start = 1
    con$gamma.rate.start = 1
    con$gamma.shape.bounds = c(1e-3, 40)
    con$gamma.rate.bounds = c(1e-3, 40)
    con$weibull.shape.start = 1
    con$weibull.scale.start = 1
    con$weibull.shape.bounds = c(1e-3, 50)
    con$weibull.scale.bounds = c(1e-3, 50)
  } else {
    mean.hat <- metaBLUE::Luo.mean(quants, n, scenario)$muhat
    sd.hat <- metaBLUE::Wan.std(quants, n, scenario)$sigmahat

    con$norm.mu.start <- mean.hat
    con$norm.sigma.start <- sd.hat

    con$lnorm.mu.start <- log(mean.hat / sqrt(1 + (sd.hat/mean.hat)^2))
    con$lnorm.sigma.start <- sqrt(log( 1 + (sd.hat/mean.hat)^2))

    con$gamma.shape.start <- mean.hat^2/sd.hat^2
    con$gamma.rate.start <- mean.hat/sd.hat^2

    con$beta.shape1.start <- mean.hat * (((mean.hat * (1 - mean.hat)) / (sd.hat^2)) - 1)
    con$beta.shape2.start <- con$beta.shape1.start * (1 - mean.hat) / mean.hat

    start.val <- (mean.hat/sd.hat)^1.086
    obj.fun <- function(k, mean.hat, sd.hat) {
      temp1 <- gamma((k + 2)/k)
      temp2 <- gamma((k + 1)/k)
      temp3 <- sqrt((temp1 / (temp2^2)) - 1)
      ((sd.hat / mean.hat) - temp3)^2
    }
    no.fit <- function(e) {
      return(list(par = NA))
    }
    get.weibull.start <- tryCatch({
      stats::nlminb(start = start.val, objective = obj.fun, lower = 1e-3,
                    mean.hat = mean.hat, sd.hat = sd.hat)
    },
    error = no.fit,
    warning = no.fit
    )
    if (!is.na(get.weibull.start$par)){
      con$weibull.shape.start <- get.weibull.start$par
    } else {
      con$weibull.shape.start <- start.val
    }
    con$weibull.scale.start <- mean.hat / gamma((con$weibull.shape.start + 1)
                                                / con$weibull.shape.start)

    con$gamma.shape.bounds <- c(1e-3, 1e2)
    con$gamma.rate.bounds <- c(1e-3, 1e2)
    con$weibull.shape.bounds <- c(1e-3, 1e2)
    con$weibull.scale.bounds <- c(1e-3, 1e2)
    con$beta.shape1.bounds <- c(10^(-3), 40)
    con$beta.shape2.bounds <- c(10^(-3), 40)
  }
  return(con)
}
