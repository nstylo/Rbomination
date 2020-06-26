#' @export
Sxx <- function(x) {
  n <- length(x)
  return(sum(x^2) - n * mean(x) ^ 2)
}

#' @export
Syy <- function(y) {
  n <- length(y)
  return(sum(y^2) - n * mean(y) ^ 2)
}

#' @export
Sxy <- function(x, y) {
  n <- length(x)
  return(sum(x * y) - n * mean(x) * mean(y))
}

#' @export
slope <- function(x, y) {
  return(Sxy(x, y) / Sxx(x))
}

#' @export
intercept <- function(x, y) {
  return(mean(y) - (mean(x) * slope(x, y)))
}

#' @export
fit <- function(x, slope, intercept) {
  return(intercept + slope * x)
}

#' @export
residuals <- function(y, y_fit) {
  return(y - y_fit)
}

#' @export
SSE <- function(y, y_fit) {
  return(sum(residuals(y, y_fit) ^ 2))
}

#' @export
SSR <- function(y, y_fit) {
  return(sum((y_fit - mean(y)) ^ 2))
}

#' @export
var_residual <- function(y, y_fit) {
  n <- length(x)
  return((1 / (n - 2)) * SSE(y, y_fit))
}

#' @export
var_intercept <- function(x, var_res) {
  n <- length(x)
  return(var_res * ((1 / n) + (mean(x) / Sxx(x))))
}

#' @export
var_slope <- function(x, var_residual) {
  return(var_residual / Sxx(x))
}

#' @export
t0_slope <- function(x, y, beta0) {
  s <- slope(x, y)
  i <- intercept(x, y)
  f <- fit(x, s, i)
  var_r <- var_residual(y, f)
  return((s - beta0) / sqrt(var_r / Sxx(x)))
}

#' @export
t0_intercept <- function(x, y, beta0) {
  n <- length(x)
  s <- slope(x, y)
  i <- intercept(x, y)
  f <- fit(x, s, i)
  var_r <- var_residual(y, f)
  return((i - beta0) / sqrt(var_r / ((1 / n) + (mean(x) ^ 2 / Sxx(x)))))
}

#' @export
x0sq <- function(x, y, var0) {
  n <- length(x)
  s <- slope(x, y)
  i <- intercept(x, y)
  f <- fit(x, s, i)
  var_r <- var_residual(y, f)
  return(((n - 2) * var_r ^ 2) / (var0 ^ 2))
}

#' @export
F0 <- function(y, y_fit) {
  n <- length(y)
  return((SSR(y, y_fit)) / (SSE(y, y_fit) / (n - 2)))
}

#' @export
ci_intercept <- function(x, y, alpha, type="two.sided") {
  n <- length(x)
  i <- intercept(x, y)
  s <- slope(x, y)
  f <- fit(x, s, i)
  var_r <- var_residual(y, f)
  fac <- sqrt(var_r * ((1 / n) + (mean(x) ^ 2 / Sxx(x))))

  if (type == "two.sided") {
    t = qt(alpha / 2, n - 2, lower.tail=F)
    return(c(i - t * fac, i + t * fac))
  } else if (type == "lower") {
    t = qt(alpha, n - 2, lower.tail=F)
    return(i - t * fac)
  } else if (type == "upper") {
    t = qt(alpha, n - 2, lower.tail=F)
    return(i + t * fac)
  } else {
    stop("either two.sided, upper or lower needs to be specified.")
  }
}

#' @export
ci_slope <- function(x, y, alpha, type="two.sided") {
  n <- length(x)
  i <- intercept(x, y)
  s <- slope(x, y)
  f <- fit(x, s, i)
  var_r <- var_residual(y, f)
  fac <- sqrt(var_r / Sxx(x))

  if (type == "two.sided") {
    t = qt(alpha / 2, n - 2, lower.tail=F)
    return(c(s - t * fac, s + t * fac))
  } else if (type == "lower") {
    t = qt(alpha, n - 2, lower.tail=F)
    return(s - t * fac)
  } else if (type == "upper") {
    t = qt(alpha, n - 2, lower.tail=F)
    return(s + t * fac)
  } else {
    stop("either two.sided, upper or lower needs to be specified.")
  }
}

#' @export
ci_var <- function(x, y, alpha, type="two.sided") {
  n <- length(x)
  i <- intercept(x, y)
  s <- slope(x, y)
  f <- fit(x, s, i)
  var_r <- var_residual(y, f)
  fac <- (n - 2) * var_r

  if (type == "two.sided") {
    t0 = qchisq(alpha / 2, n - 2, lower.tail=F)
    t1 = qchisq(alpha / 2, n - 2, lower.tail=T)
    return(c(fac / t0, fac / t1))
  } else if (type == "lower") {
    t = qchisq(alpha, n - 2, lower.tail=F)
    return(fac / t)
  } else if (type == "upper") {
    t = qchisq(alpha, n - 2, lower.tail=T)
    return(fac / t)
  } else {
    stop("either two.sided, upper or lower needs to be specified.")
  }
}

#' @export
mean_response <- function(slope, intercept, x0) {
  return(intercept + slope * x0)
}

#' @export
var_mean_response <- function(x, y, x0) {
  n <- length(x)
  i <- intercept(x, y)
  s <- slope(x, y)
  f <- fit(x, s, i)
  var_r <- var_residual(y, f)

  return(var_r * ((1 / n) + (((x0 - mean(x)) ^ 2) / Sxx(x))))
}

#' @export
ci_mean_response <- function(x, y, x0, alpha, type="two.sided") {
  n <- length(x)
  fac = sqrt(var_mean_response(x, y, x0))
  mr = mean_response(slope(x, y), intercept(x, y), x0)

  if (type == "two.sided") {
    t = qt(alpha / 2, n - 2, lower.tail=F)
    return(c(mr - t * fac, mr + t * fac))
  } else if (type == "lower") {
    t = qt(alpha, n - 2, lower.tail=F)
    return(mr - t * fac)
  } else if (type == "upper") {
    t = qt(alpha, n - 2, lower.tail=F)
    return(mr + t * fac)
  } else {
    stop("either two.sided, upper or lower need to be specified.")
  }
}
