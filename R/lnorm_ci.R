#' Lognormal confidence interval for an abundance estimate
#'
#' Computes an asymmetric confidence interval for a strictly positive
#' quantity (typically an estimate of abundance \eqn{\hat N}) under the
#' assumption that the estimator is lognormally distributed. This is the
#' interval used throughout distance sampling (Buckland et al. 2001): the
#' bounds are \eqn{\hat N / C} and \eqn{\hat N \times C}, with
#' \deqn{C = \exp\!\left(z_{1-\alpha/2}\,\sqrt{\log(1 + \mathrm{cv}^2)}\right),}
#' where \eqn{z_{1-\alpha/2}} is the standard normal quantile.
#'
#' @details
#' The interval is symmetric on the log scale, so its geometric mean equals
#' the point estimate: \eqn{\sqrt{\mathrm{lo} \times \mathrm{hi}} = N}. When
#' \code{cv = 0} the interval collapses to the point estimate
#' (\code{lo == hi == N}).
#'
#' The function is vectorised: \code{N} and \code{cv} must have equal length,
#' or one of them must be a scalar (recycled against the other). Unequal
#' non-scalar lengths raise an error rather than being silently recycled.
#' Inputs are validated: \code{N} must be finite and strictly positive,
#' \code{cv} finite and non-negative, and \code{alpha} a single number in
#' \eqn{(0, 1)}.
#'
#' @param N Numeric vector of point estimate(s) (e.g. abundance \eqn{\hat N}).
#'   Must be finite and strictly positive.
#' @param cv Numeric vector of coefficient(s) of variation of \code{N}
#'   (i.e. \code{se(N) / N}). Must be finite and non-negative, and either the
#'   same length as \code{N} or a scalar.
#' @param alpha Numeric scalar significance level in \eqn{(0, 1)}. Defaults to
#'   \code{0.05}, giving a 95\% confidence interval.
#'
#' @return A named \code{list} with two numeric vectors, each the length of the
#'   recycled \code{N}/\code{cv}:
#'   \describe{
#'     \item{\code{lo}}{lower confidence bound(s), \eqn{N / C}.}
#'     \item{\code{hi}}{upper confidence bound(s), \eqn{N \times C}.}
#'   }
#'
#' @references
#' Buckland, S.T., Anderson, D.R., Burnham, K.P., Laake, J.L., Borchers, D.L.,
#' & Thomas, L. (2001). \emph{Introduction to Distance Sampling}. Oxford
#' University Press.
#'
#' @examples
#' # 95% CI for an abundance estimate of 1200 with CV = 0.25
#' lnorm_ci(1200, 0.25)
#'
#' # vectorised over several estimates
#' lnorm_ci(c(500, 1200), c(0.30, 0.25))
#'
#' # 90% CI
#' lnorm_ci(1200, 0.25, alpha = 0.10)
#'
#' @importFrom stats qnorm
#' @export
lnorm_ci <- function(N, cv, alpha = 0.05) {

  # --- input validation ---
  if (!is.numeric(N) || length(N) < 1L)
    stop("`N` must be a non-empty numeric vector.", call. = FALSE)
  if (!is.numeric(cv) || length(cv) < 1L)
    stop("`cv` must be a non-empty numeric vector.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L)
    stop("`alpha` must be a single number.", call. = FALSE)

  if (anyNA(N) || anyNA(cv) || anyNA(alpha))
    stop("`N`, `cv`, and `alpha` must not contain NA.", call. = FALSE)
  if (any(!is.finite(N)) || any(!is.finite(cv)))
    stop("`N` and `cv` must be finite.", call. = FALSE)

  if (any(N <= 0))
    stop("`N` must be strictly positive.", call. = FALSE)
  if (any(cv < 0))
    stop("`cv` must be non-negative.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1)
    stop("`alpha` must lie in (0, 1).", call. = FALSE)

  if (length(N) != length(cv) && length(N) != 1L && length(cv) != 1L)
    stop("`N` and `cv` must have equal length, or one must be scalar.",
         call. = FALSE)

  # --- lognormal bounds ---
  C <- exp(qnorm(1 - alpha / 2) * sqrt(log(cv^2 + 1)))
  list(lo = N / C, hi = N * C)
}
