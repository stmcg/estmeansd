check_errors <- function(min.val, q1.val, med.val, q3.val, max.val, n, scenario){
  if (missing(n)) {
    stop("Need to specify n")
  }
  if (is.na(n) | n < 3 | n > 1e6){
    stop("Value of n must be between 3 and 1,000,000")
  }
  if (scenario == 'S1'){
    quants <- c(min.val, med.val, max.val)
    if (!is.numeric(quants)){
      stop(paste('The quantiles are not of type numeric. The values of min.val, med.val, and max.val are:', min.val, med.val, max.val))
    }
    if (is.unsorted(quants)){
      stop(paste('The quantiles are not in increasing order. The values of min.val, med.val, and max.val are:', min.val, med.val, max.val))
    }
    if (length(unique(c(quants))) < 3){
      warning(paste('Some of the quantiles are equal to each other. This can result in unexpected behaviour of the method. The values of min.val, med.val, and max.val are:', min.val, med.val, max.val))
    }
  } else if (scenario == 'S2'){
    quants <- c(q1.val, med.val, q3.val)
    if (!is.numeric(quants)){
      stop(paste('The quantiles are not of type numeric. The values of q1.val, med.val, and q3.val are:', q1.val, med.val, q3.val))
    }
    if (is.unsorted(quants)){
      stop(paste('The quantiles are not in increasing order. The values of q1.val, med.val, and q3.val are:', q1.val, med.val, q3.val))
    }
    if (length(unique(quants)) < 3){
      warning(paste('Some of the quantiles are equal to each other. This can result in unexpected behaviour of the method. The values of q1.val, med.val, and q3.val are:', q1.val, med.val, q3.val))
    }
  } else if (scenario == 'S3'){
    quants <- c(min.val, q1.val, med.val, q3.val, max.val)
    if (!is.numeric(quants)){
      stop(paste('The quantiles are not of type numeric. The values of min.val, q1.val, med.val, q3.val, and max.val are:', min.val, q1.val, med.val, q3.val, max.val))
    }
    if (is.unsorted(quants)){
      stop(paste('The quantiles are not in increasing order. The values of min.val, q1.val, med.val, q3.val, and max.val are:', min.val, q1.val, med.val, q3.val, max.val))
    }
    if (length(unique(quants)) < 5){
      warning(paste('Some of the quantiles are equal to each other. This can result in unexpected behaviour of the method. The values of min.val, q1.val, med.val, q3.val, and max.val are:', min.val, q1.val, med.val, q3.val, max.val))
    }
  }
}
