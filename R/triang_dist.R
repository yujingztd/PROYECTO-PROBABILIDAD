#' @title Triangular Distribution
#' @description Density, distribution function, quantile function and random generation for the triangular distribution.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param min lower limit of the distribution.
#' @param max upper limit of the distribution.
#' @param mode mode of the distribution.
#' @name triangdist


#' @rdname triangdist
#' @export
dtriang <- function(x, min, max, mode) {
  if (min >= max) stop("min must be < max")
  if (mode < min || mode > max) stop("mode must be in [min, max]")
  h <- 2 / (max - min)
  ifelse(
    x < min | x > max, 0,
    ifelse(
      x <= mode,
      h * (x - min) / (mode - min),
      h * (max - x) / (max - mode)
    )
  )
}

#' @rdname triangdist
#' @export
ptriang <- function(q, min, max, mode) {
  if (min >= max) stop("min must be < max")
  if (mode < min || mode > max) stop("mode must be in [min, max]")
  ifelse(
    q <= min, 0,
    ifelse(
      q <= mode,
      ((q - min)^2) / ((max - min)*(mode - min)),
      ifelse(
        q <= max,
        1 - ((max - q)^2) / ((max - min)*(max - mode)),
        1
      )
    )
  )
}

#' @rdname triangdist
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(p < 0 | p > 1)) stop("p must be in [0,1]")
  if (min >= max) stop("min must be < max")
  if (mode < min || mode > max) stop("mode must be in [min, max]")
  Fc <- (mode - min) / (max - min)
  ifelse(
    p <= Fc,
    min + sqrt(p * (max - min) * (mode - min)),
    max - sqrt((1 - p) * (max - min) * (max - mode))
  )
}

#' @rdname triangdist
#' @export
rtriang <- function(n, min, max, mode) {
  if (n <= 0) stop("n must be positive")
  u <- runif(n)
  qtriang(u, min, max, mode)
}