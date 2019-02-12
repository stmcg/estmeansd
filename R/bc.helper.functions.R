get.location.scale.shape <- function(min.val, q1.val, med.val, q3.val, max.val,
                                     n, scenario) {
  if (scenario == "S1") {
    quants <- c(min.val, med.val, max.val)
  } else if (scenario == "S2") {
    quants <- c(q1.val, med.val, q3.val)
  } else if (scenario == "S3") {
    quants <- c(min.val, q1.val, med.val, q3.val, max.val)
  }

  lambda <- find.lambda(min.val, q1.val, med.val, q3.val, max.val, n, quants,
    scenario
  )
  lambda <- round.lambda(lambda, n)
  transformed.vals <- smooth.bc.transform(quants, lambda)

  location <- metaBLUE::Luo.mean(transformed.vals, n, scenario)$muhat
  scale <- metaBLUE::Wan.std(transformed.vals, n, scenario)$sigmahat
  shape <- as.numeric(lambda)
  c(location = location, scale = scale, shape = shape)
}

find.lambda <- function(min.val, q1.val, med.val, q3.val, max.val, n, quants,
                        scenario) {
  if (scenario == "S1") {
    ratio.diff <- ((log(max.val) - log(med.val)) /
                     (log(med.val) - log(min.val)) - 1)^2
  } else if (scenario == "S2") {
    ratio.diff <- ((log(q3.val) - log(med.val)) /
                     (log(med.val) - log(q1.val)) - 1)^2
  } else if (scenario == "S3") {
    temp1 <- ((log(q3.val) - log(med.val)) / (log(med.val) - log(q1.val)) - 1)^2
    temp2 <- ((log(max.val) - log(med.val)) /
                (log(med.val) - log(min.val)) - 1)^2
    ratio.diff <- temp1 + temp2
  }

  if (abs(ratio.diff) < 1e-8) {
    return(0)
  } else {
    lambda.to.err <- function(lambda) {
      transformed.vals <- smooth.bc.transform(quants, lambda)
      if (scenario == "S1") {
        t.min <- transformed.vals[1]
        t.med <- transformed.vals[2]
        t.max <- transformed.vals[3]
        ((t.max - t.med) / (t.med - t.min) - 1)^2
      } else if (scenario == "S2") {
        t.q1 <- transformed.vals[1]
        t.med <- transformed.vals[2]
        t.q3 <- transformed.vals[3]
        ((t.q3 - t.med) / (t.med - t.q1) - 1)^2
      } else if (scenario == "S3") {
        t.min <- transformed.vals[1]
        t.q1 <- transformed.vals[2]
        t.med <- transformed.vals[3]
        t.q3 <- transformed.vals[4]
        t.max <- transformed.vals[5]
        temp1 <- ((t.q3 - t.med) / (t.med - t.q1) - 1)^2
        temp2 <- ((t.max - t.med) / (t.med - t.min) - 1)^2
        temp1 + temp2
      }
    }
    no.fit <- function(e) {
      list(minimum = NA)
    }
    opt <- tryCatch({
      stats::optimize(lambda.to.err, c(-2, 3))
    },
    error = no.fit
    )
    if (is.na(opt$minimum)) {
      stop("Optimization algorithm for finding lambda did not converge.")
    }
    opt$minimum
  }
}

round.lambda <- function(lambda, n, prior.power = 1, prior.strength = 200) {
  prior.weight <- prior.strength * (prior.power - lambda)^2
  ret <- (lambda * n + prior.power * prior.weight) / (n + prior.weight)
  if (abs(ret) < 1e-8) {
    0
  } else {
    ret
  }
}

smooth.bc.transform <- function(vals, lambda) {
  if (abs(lambda) < 1e-8) {
    log(vals)
  } else {
    (sign.pow(vals, lambda) - 1) / lambda
  }
}

inv.smooth.bc.transform <- function(transformed.vals, lambda) {
  if (abs(lambda) < 1e-8) {
    exp(transformed.vals)
  } else {
    sign.pow(transformed.vals * lambda + 1, 1 / lambda)
  }
}

sign.pow <- function(a, b) {
  sign(a) * abs(a)^b
}
