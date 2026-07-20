#' Per-year partial effect of a factor-smooth spatial term
#'
#' For a density surface model whose spatial term varies by year through a
#' factor-smooth basis (\code{s(x, y, year_fac, bs = "fs")}), \code{year} is not
#' a standalone model term — it is bundled inside the smooth. This function
#' extracts an interpretable per-year effect by evaluating that smooth's partial
#' contribution over a set of locations and averaging it (optionally area
#' weighted), returning one value per year on the link (log) scale with a
#' confidence interval.
#'
#' @details
#' For each level of \code{year_fac} the smooth's partial effect is
#' \eqn{\eta_k(x, y) = } (per-year intercept) \eqn{+} (per-year spatial
#' deviation). Averaged over the grid the spatial deviation is approximately
#' zero, so the weighted average \eqn{\bar\eta_k = a_k^\top \hat\beta} (with
#' \eqn{a_k} the weighted-mean basis row for year \eqn{k}) estimates the
#' year-level shift. Its standard error uses the smooth's covariance,
#' \eqn{\mathrm{se} = \sqrt{a_k^\top V a_k}} — the correct variance for a
#' weighted mean of correlated cell predictions, not an average of per-cell
#' standard errors.
#'
#' Because the \code{"fs"} basis treats the factor as a random effect, the
#' estimates are shrunk toward the overall mean (\eqn{0} on this centred scale);
#' data-poor years are pulled hardest. This differs from a fixed
#' \code{year_fac} main effect or an \code{s(x, y, by = year_fac)} term, which
#' give unshrunk per-year effects.
#'
#' Works with \pkg{dsm} objects (for which \code{off.set} is supplied to
#' \code{predict()}, as that method requires it) and with plain \pkg{mgcv}
#' \code{gam} objects. The model is assumed to contain a \code{year_fac} term;
#' if it also contains \code{season}, that term is held at its reference level
#' (it does not affect the extracted \code{year_fac} columns).
#'
#' @param model A fitted \code{dsm}/\code{gam} whose model frame contains a
#'   \code{year_fac} factor entering through a factor-smooth
#'   (\code{s(x, y, year_fac, bs = "fs")}).
#' @param data A \code{data.frame} of prediction locations with numeric columns
#'   \code{x} and \code{y} (e.g. grid-cell centroids). Geometry, if any, should
#'   be dropped by the caller.
#' @param weights Optional non-negative numeric vector, length \code{nrow(data)},
#'   giving averaging weights (e.g. cell areas for an area-weighted mean).
#'   Defaults to equal weights.
#' @param level Confidence level for the interval. Defaults to \code{0.95}.
#'
#' @return A \code{data.frame} with one row per year and columns
#'   \code{year} (numeric), \code{partial}, \code{se}, \code{lower},
#'   \code{upper} — the partial effect and CI on the link (log) scale.
#'
#' @examples
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   set.seed(1)
#'   n   <- 300
#'   dat <- data.frame(
#'     x        = runif(n),
#'     y        = runif(n),
#'     year_fac = factor(sample(2010:2013, n, replace = TRUE)),
#'     season   = factor(sample(c("Spring", "Summer"), n, replace = TRUE))
#'   )
#'   dat$count <- rpois(n, 3)
#'   m <- mgcv::gam(count ~ s(x, y, year_fac, bs = "fs") + season,
#'                  data = dat, family = poisson())
#'   grid <- expand.grid(x = seq(0, 1, 0.25), y = seq(0, 1, 0.25))
#'   year_partial_effect(m, grid)
#' }
#'
#' @importFrom stats coef predict qnorm vcov
#' @export
year_partial_effect <- function(model, data, weights = NULL, level = 0.95) {

  # --- input validation ---
  if (is.null(model$model) || !"year_fac" %in% names(model$model))
    stop("`model` must be a fitted gam/dsm with a `year_fac` term ",
         "(e.g. s(x, y, year_fac, bs = \"fs\")).", call. = FALSE)
  yf <- levels(model$model$year_fac)
  if (is.null(yf))
    stop("`model$model$year_fac` must be a factor.", call. = FALSE)
  if (!is.data.frame(data) || !all(c("x", "y") %in% names(data)))
    stop("`data` must be a data.frame with numeric columns `x` and `y`.",
         call. = FALSE)
  n <- nrow(data)
  if (n < 1L)
    stop("`data` has no rows.", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (!is.numeric(weights) || length(weights) != n || anyNA(weights) ||
      any(weights < 0) || sum(weights) <= 0)
    stop("`weights` must be a non-negative numeric vector of length ",
         "nrow(data) with a positive sum.", call. = FALSE)
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      level <= 0 || level >= 1)
    stop("`level` must be a single number in (0, 1).", call. = FALSE)

  # --- setup ---
  w     <- weights / sum(weights)
  beta  <- stats::coef(model)
  V     <- stats::vcov(model)
  z     <- stats::qnorm(1 - (1 - level) / 2)
  is_dsm     <- inherits(model, "dsm")
  has_season <- "season" %in% names(model$model)
  seas0 <- if (has_season)
    factor(levels(model$model$season)[1], levels = levels(model$model$season))

  # --- per-year weighted-mean partial effect + CI ---
  parts <- lapply(yf, function(yy) {
    nd <- data
    nd$year_fac <- factor(yy, levels = yf)
    if (has_season) nd$season <- seas0
    Lp <- if (is_dsm)
      stats::predict(model, newdata = nd, type = "lpmatrix", off.set = 1)
    else
      stats::predict(model, newdata = nd, type = "lpmatrix")
    cols <- grep("year_fac", colnames(Lp), fixed = TRUE)   # s(x,y,year_fac) basis
    if (length(cols) == 0L)
      stop("No `year_fac` smooth columns found in the model's lpmatrix.",
           call. = FALSE)
    a   <- as.numeric(w %*% Lp[, cols, drop = FALSE])       # weighted-mean basis row
    est <- sum(a * beta[cols])
    se  <- sqrt(as.numeric(t(a) %*% V[cols, cols] %*% a))
    data.frame(year    = suppressWarnings(as.numeric(as.character(yy))),
               partial = est,
               se      = se,
               lower   = est - z * se,
               upper   = est + z * se)
  })
  do.call(rbind, parts)
}
