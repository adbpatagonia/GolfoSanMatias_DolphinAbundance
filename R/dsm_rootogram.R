#' Tweedie parameters of a fitted density surface model
#'
#' Extracts the Tweedie power \code{p} and the scale (dispersion) parameter
#' \code{phi} from a \code{gam}/\code{dsm} fitted with a Tweedie family, so the
#' fitted response distribution can be evaluated.
#'
#' @details
#' Two Tweedie families reach these fits and they store \code{p} differently:
#'
#' \itemize{
#'   \item \code{\link[mgcv]{tw}} is an extended family that ESTIMATES \code{p}
#'     alongside the smoothing parameters. It carries a \code{getTheta()}
#'     accessor, and \code{getTheta(TRUE)} returns \code{p} itself while
#'     \code{getTheta(FALSE)} returns the unconstrained parameter mgcv actually
#'     optimises (a negative number). Taking the wrong one is silent: it
#'     produces a plausible-looking rootogram that reads as model misfit.
#'   \item \code{\link[mgcv]{Tweedie}} takes \code{p} as a fixed argument and is
#'     an ordinary \code{family} with no \code{getTheta()}, so \code{p} has to
#'     be read off the family string. These fits return \code{AIC = NA} and
#'     therefore never enter the model-selection tables, but a rootogram for one
#'     is perfectly well defined.
#' }
#'
#' \code{phi} is the estimated scale parameter, \code{model$scale} (identical
#' to \code{model$sig2} and to \code{summary(model)$scale}).
#'
#' @param model A fitted \code{dsm} or \code{gam} object with a Tweedie family.
#'
#' @return A list with \code{p} (Tweedie power, strictly between 1 and 2),
#'   \code{phi} (scale parameter) and \code{fixed_p} (\code{TRUE} if \code{p}
#'   was supplied rather than estimated).
#'
#' @seealso \code{\link{dsm_rootogram}}
#'
#' @examples
#' \dontrun{
#' dsm_tweedie_pars(dd.dsm.soap.season.year)
#' }
#'
#' @export
dsm_tweedie_pars <- function(model) {
  stopifnot(inherits(model, "gam"))
  fam <- model$family
  if (is.null(fam) || !grepl("^Tweedie", fam$family))
    stop("dsm_rootogram() needs a Tweedie fit; this model's family is '",
         if (is.null(fam)) "<none>" else fam$family, "'.", call. = FALSE)

  if (is.function(fam$getTheta)) {
    p       <- as.numeric(fam$getTheta(TRUE))     # TRUE = p, FALSE = the
    fixed_p <- FALSE                              # unconstrained parameter
  } else {
    p       <- suppressWarnings(as.numeric(
                 sub("^Tweedie\\(p? *=? *([0-9.eE+-]+)\\).*$", "\\1", fam$family)))
    fixed_p <- TRUE
  }
  if (length(p) != 1L || !is.finite(p))
    stop("could not recover the Tweedie power from family '", fam$family, "'.",
         call. = FALSE)
  if (p <= 1 || p >= 2)
    stop(sprintf("Tweedie power p = %g; the compound Poisson-gamma ", p),
         "representation the rootogram uses needs p strictly between 1 and 2.",
         call. = FALSE)

  phi <- as.numeric(model$scale)
  if (length(phi) != 1L || !is.finite(phi) || phi <= 0)
    stop("model$scale is not a positive finite dispersion.", call. = FALSE)

  list(p = p, phi = phi, fixed_p = fixed_p)
}


#' Tweedie distribution function, vectorised over means and quantiles
#'
#' Evaluates \eqn{P(Y \le q)} for a Tweedie random variable with power
#' \code{p} in \eqn{(1, 2)}, for every combination of a vector of means and a
#' vector of quantiles.
#'
#' @details
#' For \eqn{1 < p < 2} the Tweedie is the compound Poisson-gamma distribution:
#' \eqn{Y = \sum_{k=1}^{N} G_k} with \eqn{N \sim \mathrm{Poisson}(\lambda)} and
#' \eqn{G_k \sim \mathrm{Gamma}(\alpha, \theta)} independent, where
#' \eqn{\lambda = \mu^{2-p} / (\phi (2-p))},
#' \eqn{\alpha = (2-p)/(p-1)} and \eqn{\theta = \phi (p-1) \mu^{p-1}}.
#' The distribution therefore has an atom of size \eqn{e^{-\lambda}} at zero
#' and a density on \eqn{(0, \infty)}, and its distribution function is the
#' Poisson mixture
#' \deqn{F(q) = \sum_{k \ge 0} \mathrm{dpois}(k, \lambda) \,
#'              \mathrm{pgamma}(q; k\alpha, \theta),}
#' with the \eqn{k = 0} term contributing \eqn{e^{-\lambda}} for every
#' \eqn{q \ge 0}. The sum is truncated where the Poisson tail is negligible.
#'
#' \code{\link[tweedie]{ptweedie}} computes the same quantity and is the
#' reference this function is tested against, but it inverts the series
#' numerically one mean at a time: one bin edge over the ~6300 segments of this
#' project's DSMs takes 45-65 seconds, so a fifteen-bin rootogram for four
#' models would run for hours. Evaluating the mixture directly and vectorising
#' over means makes the same table take well under a second.
#'
#' @param q Numeric vector of quantiles. \code{Inf} is allowed and returns 1.
#' @param mu Numeric vector of means (all strictly positive).
#' @param p Tweedie power, strictly between 1 and 2.
#' @param phi Scale (dispersion) parameter, strictly positive.
#' @param tol Poisson-tail mass to neglect when truncating the mixture.
#'   Defaults to \code{1e-12}.
#'
#' @return A numeric matrix with \code{length(mu)} rows and \code{length(q)}
#'   columns, \code{[i, j]} holding \eqn{P(Y_i \le q_j)}.
#'
#' @seealso \code{\link[tweedie]{ptweedie}}, \code{\link{dsm_rootogram}}
#'
#' @references
#' Jorgensen, B. (1987). Exponential dispersion models. \emph{Journal of the
#' Royal Statistical Society B} 49:127-162.
#'
#' Dunn, P.K. & Smyth, G.K. (2005). Series evaluation of Tweedie exponential
#' dispersion model densities. \emph{Statistics and Computing} 15:267-280.
#'
#' @examples
#' dsm_tweedie_cdf(q = c(0, 1, 2, Inf), mu = c(0.2, 1, 5), p = 1.45, phi = 2.1)
#'
#' @export
dsm_tweedie_cdf <- function(q, mu, p, phi, tol = 1e-12) {
  stopifnot(is.numeric(q), length(q) >= 1L,
            is.numeric(mu), length(mu) >= 1L, all(is.finite(mu)), all(mu > 0),
            is.numeric(p), length(p) == 1L, p > 1, p < 2,
            is.numeric(phi), length(phi) == 1L, phi > 0)

  lambda <- mu^(2 - p) / (phi * (2 - p))     # jump rate
  alpha  <- (2 - p) / (p - 1)                # gamma shape per jump
  theta  <- phi * (p - 1) * mu^(p - 1)       # gamma scale per jump

  k_max <- max(5L, stats::qpois(1 - tol, max(lambda)) + 5L)
  if (k_max > 1e5L)
    stop("Poisson mixture would need ", k_max, " terms (max lambda = ",
         signif(max(lambda), 4), "); refusing to evaluate.", call. = FALSE)

  out    <- matrix(0, nrow = length(mu), ncol = length(q))
  is_inf <- is.infinite(q) & q > 0
  fin    <- which(!is_inf)

  for (k in 0:k_max) {
    w <- stats::dpois(k, lambda)
    if (max(w) < .Machine$double.xmin) next
    if (k == 0L) {
      # the atom at zero: contributes to every q >= 0
      out[, fin] <- out[, fin] + outer(w, as.numeric(q[fin] >= 0))
    } else {
      for (j in fin)
        out[, j] <- out[, j] + w * stats::pgamma(q[j], shape = k * alpha,
                                                 scale = theta)
    }
  }
  if (any(is_inf)) out[, is_inf] <- 1

  # truncation and floating point can push a value a hair outside [0, 1]
  out[] <- pmin(pmax(out, 0), 1)
  dimnames(out) <- NULL
  out
}


#' Rootogram for a density surface model
#'
#' Builds a hanging rootogram (Kleiber & Zeileis 2016) for a \code{dsm}/
#' \code{gam} fitted with a Tweedie family: the observed number of segments
#' falling in each count bin against the number the fitted model expects, both
#' on the square-root scale. Optionally computed separately within blocks, so
#' the comparison can be made region by region rather than only over the whole
#' survey area.
#'
#' @details
#' \strong{What it is for.} The model-selection tables rank candidates by AIC
#' and the residual checks look at autocorrelation and basis dimension, but
#' none of those asks the simplest question: does the fitted distribution
#' produce the counts that were actually seen? A rootogram answers exactly
#' that, and it is sensitive to the two failure modes that matter for
#' segment-level count data -- overdispersion, which shows as too few observed
#' counts in the middle bins and too many in the tail, and excess zeros, which
#' shows as one large positive deviation in the zero bin. Kleiber & Zeileis's
#' point is that plotting on the square-root scale is what makes the small
#' frequencies readable at all: the standard error of \eqn{\sqrt{O}} is roughly
#' constant, so a deviation of a given size means the same thing in the zero
#' bin and in the tail.
#'
#' \strong{How the expected counts are computed.} The published rootogram is
#' built for discrete models (Poisson, negative binomial, their
#' zero-inflated and hurdle variants), where the expected frequency of count
#' \eqn{y} is \eqn{\sum_i P(Y_i = y)}. A Tweedie DSM with
#' \eqn{1 < p < 2} is not discrete: it puts an atom at zero and a continuous
#' density on \eqn{(0, \infty)}, so \eqn{P(Y_i = y) = 0} for every positive
#' \eqn{y} and the discrete construction degenerates. Expected frequencies are
#' therefore obtained by differencing the per-segment distribution function
#' over the bins,
#' \deqn{E_j = \sum_i \left[ F_i(b_j) - F_i(a_j) \right],}
#' with the zero bin taken as the atom \eqn{E_1 = \sum_i F_i(0)} and the
#' remaining bins as half-open intervals \eqn{(a_j, b_j]}. The bins tile
#' \eqn{[0, \infty)}, so \code{expected} and \code{observed} both sum to the
#' number of segments. Because the response is integer, bin \eqn{(k-1, k]}
#' contains exactly the segments with count \eqn{k}.
#'
#' Each \eqn{F_i} is the fitted Tweedie distribution function at that segment's
#' own mean, and \code{\link[stats]{fitted}} carries the DSM offset (segment
#' area times effective strip half-width times detection probability), so the
#' expectation is on the count scale the response is on. Note that the offset
#' treats detection probability as KNOWN: uncertainty in the fitted detection
#' function is not propagated into \code{expected}, so a rootogram tests the
#' spatial and distributional part of the model, not the detection part.
#'
#' \strong{Reading it.} \code{ymax} is \eqn{\sqrt{E_j}} and \code{ymin} is
#' \eqn{\sqrt{E_j} - \sqrt{O_j}}: bars hang from the expected curve, and the
#' distance of a bar's foot from zero is \code{resid}, \eqn{\sqrt{O_j} -
#' \sqrt{E_j}}. A foot below zero means the model expects more segments in that
#' bin than were seen, above zero fewer. The classic overdispersion signature
#' is a run of feet above zero at the ends and below zero in the middle; excess
#' zeros is a single foot well above zero in the zero bin with the rest
#' roughly flat.
#'
#' \code{band} is a two-standard-error half-width for \code{resid}. Treating
#' \eqn{O_j} as a sum of independent Bernoulli indicators gives
#' \eqn{\mathrm{Var}(O_j) = \sum_i \pi_{ij}(1 - \pi_{ij})}, and the delta
#' method turns that into \eqn{2\,\mathrm{sd}(O_j) / (2\sqrt{E_j})} on the
#' square-root scale. Read \code{sig} as a flag rather than a test: it ignores
#' the uncertainty in the fitted parameters, and it assumes the segments are
#' independent, which the along-track correlogram shows they are not entirely.
#' The true envelope is wider, so an exceedance is weaker evidence than it
#' looks -- the same caveat as \code{\link{dsm_correlogram}}'s band.
#'
#' \strong{Blocks.} With \code{group}, the same construction is applied within
#' each level, summing only over the segments in that block. This is the
#' survey-block scale: it localises a misfit that the pooled rootogram averages
#' away, which is what makes it useful for judging the number and placement of
#' soap-film knots -- a block whose zero bin or tail is persistently off is a
#' region the spatial surface is not describing. Blocks are small, so the bins
#' should usually be coarser than the pooled ones; pass \code{breaks}
#' explicitly to keep them comparable across blocks.
#'
#' @param model A fitted \code{dsm} or \code{gam} object with a Tweedie family
#'   and a log link.
#' @param breaks Numeric vector of bin edges, increasing, starting at
#'   \code{0} and typically ending at \code{Inf}. \code{breaks[1] = 0} defines
#'   the zero bin (the atom); each later element is the upper edge of a
#'   half-open bin. \code{NULL} (the default) builds integer bins up to a
#'   cutoff chosen from the data -- see \code{max_count} and \code{min_tail_n}.
#' @param group Optional grouping vector the same length as the model's
#'   response, giving the block each segment belongs to. \code{NULL} (the
#'   default) pools over the whole survey area.
#' @param max_count Largest count given its own bin when \code{breaks} is
#'   \code{NULL}. Defaults to \code{20}.
#' @param min_tail_n When \code{breaks} is \code{NULL}, the cutoff is the
#'   largest integer leaving at least this many observations in the lumped
#'   upper bin. Defaults to \code{5}.
#'
#' @return A \code{data.table}, one row per bin (per block if \code{group} was
#'   given), with columns
#'   \describe{
#'     \item{\code{group}}{block label; present only when \code{group} was given.}
#'     \item{\code{bin}}{ordered factor labelling the bin (\code{"0"},
#'       \code{"1"}, ..., \code{"4-6"}, \code{">10"}).}
#'     \item{\code{lower}, \code{upper}}{bin edges on the response scale. The
#'       first bin is the point \code{{0}}; bin \eqn{j} is \eqn{(a_j, b_j]}.}
#'     \item{\code{observed}}{number of segments with a count in the bin.}
#'     \item{\code{expected}}{number the fitted model expects there.}
#'     \item{\code{se}}{Poisson-binomial standard deviation of
#'       \code{observed}.}
#'     \item{\code{sqrt_obs}, \code{sqrt_exp}}{the two on the square-root scale.}
#'     \item{\code{ymin}, \code{ymax}}{extent of the hanging bar.}
#'     \item{\code{resid}}{\code{sqrt_obs - sqrt_exp}, the deviation the bar's
#'       foot shows.}
#'     \item{\code{band}}{two-standard-error half-width for \code{resid}.}
#'     \item{\code{sig}}{\code{TRUE} if \code{abs(resid) > band}. A flag, not a
#'       test -- see \strong{Details}.}
#'   }
#'
#' @seealso \code{\link{dsm_tweedie_cdf}} for the distribution function,
#'   \code{\link{dsm_tweedie_pars}} for the parameters,
#'   \code{\link{dsm_correlogram}} and \code{\link{dsm_kcheck}} for the other
#'   two DSM diagnostics.
#'
#' @references
#' Kleiber, C. & Zeileis, A. (2016). Visualizing count data regressions using
#' rootograms. \emph{The American Statistician} 70:296-303.
#'
#' Tukey, J.W. (1977). \emph{Exploratory Data Analysis}. Addison-Wesley.
#'
#' @examples
#' \dontrun{
#' # whole survey area
#' rg <- dsm_rootogram(dd.dsm.soap.season.year)
#' rg[1]                                   # the zero bin: excess zeros?
#'
#' # by survey block, bins held fixed so the blocks are comparable
#' blk <- dsm_spatial_blocks(m$data$x, m$data$y, n = c(3, 3))
#' dsm_rootogram(m, breaks = c(0, 1, 2, 4, 8, Inf), group = blk$block)
#' }
#'
#' @export
dsm_rootogram <- function(model, breaks = NULL, group = NULL,
                          max_count = 20L, min_tail_n = 5L) {
  stopifnot(inherits(model, "gam"))
  pars <- dsm_tweedie_pars(model)

  y  <- as.numeric(model$y)
  mu <- as.numeric(stats::fitted(model))
  if (length(mu) != length(y))
    stop("fitted() and model$y have different lengths (", length(mu), " vs ",
         length(y), ").", call. = FALSE)
  if (anyNA(y) || anyNA(mu))
    stop("NA in the response or the fitted values; rootogram undefined.",
         call. = FALSE)
  if (any(y < 0)) stop("negative counts in the response.", call. = FALSE)

  # ---- bins ---------------------------------------------------------------
  if (is.null(breaks)) {
    cand   <- seq_len(as.integer(max_count))
    ok     <- vapply(cand, function(c) sum(y > c) >= min_tail_n, logical(1))
    cutoff <- if (any(ok)) max(cand[ok]) else 1L
    breaks <- c(0, seq_len(cutoff), Inf)
  }
  breaks <- as.numeric(breaks)
  if (breaks[1] != 0)
    stop("breaks[1] must be 0: the first bin is the atom at zero.", call. = FALSE)
  if (length(breaks) < 2L || any(diff(breaks) <= 0))
    stop("breaks must be strictly increasing and have at least two elements.",
         call. = FALSE)

  lower <- c(0, breaks[-length(breaks)])
  upper <- breaks
  lab   <- character(length(breaks))
  lab[1] <- "0"
  for (j in seq_along(breaks)[-1]) {
    lab[j] <- if (is.infinite(upper[j])) sprintf(">%g", lower[j])
              else if (upper[j] - lower[j] == 1) sprintf("%g", upper[j])
              else sprintf("%g-%g", lower[j] + 1, upper[j])
  }

  # ---- per-segment bin probabilities -------------------------------------
  # F evaluated once at each distinct edge, then differenced
  cdf <- dsm_tweedie_cdf(upper, mu, pars$p, pars$phi)   # n x nbin
  pi  <- cdf
  if (ncol(cdf) > 1L) pi[, -1L] <- cdf[, -1L, drop = FALSE] -
                                   cdf[, -ncol(cdf), drop = FALSE]
  pi[] <- pmax(pi, 0)

  # ---- observed ----------------------------------------------------------
  in_bin <- function(idx) {
    o <- numeric(length(breaks))
    yy <- y[idx]
    o[1] <- sum(yy == 0)
    for (j in seq_along(breaks)[-1])
      o[j] <- sum(yy > lower[j] & yy <= upper[j])
    o
  }

  one <- function(idx) {
    o <- in_bin(idx)
    pj <- pi[idx, , drop = FALSE]
    data.table::data.table(
      bin      = factor(lab, levels = lab),
      lower    = lower,
      upper    = upper,
      observed = o,
      expected = colSums(pj),
      se       = sqrt(colSums(pj * (1 - pj)))
    )
  }

  if (is.null(group)) {
    out <- one(seq_along(y))
  } else {
    if (length(group) != length(y))
      stop("`group` has length ", length(group), " but the model has ",
           length(y), " observations. NA covariate values drop rows from the ",
           "fit but not from segdata.", call. = FALSE)
    g   <- if (is.factor(group)) droplevels(group) else factor(group)
    out <- data.table::rbindlist(lapply(levels(g), function(lv) {
      cbind(group = lv, one(which(g == lv)))
    }))
    out[, group := factor(group, levels = levels(g))]
    data.table::setcolorder(out, c("group", "bin"))
  }

  out[, `:=`(sqrt_obs = sqrt(observed), sqrt_exp = sqrt(expected))]
  out[, `:=`(ymax  = sqrt_exp,
             ymin  = sqrt_exp - sqrt_obs,
             resid = sqrt_obs - sqrt_exp,
             band  = se / sqrt(expected))]
  out[, sig := is.finite(band) & abs(resid) > band]
  out[]
}


#' Assign points to a regular grid of spatial blocks
#'
#' Cuts the bounding box of a set of projected coordinates into an
#' \code{n[1]} by \code{n[2]} grid and labels each point with the cell it falls
#' in, dropping cells with too few points.
#'
#' @details
#' This survey has no design strata: \code{segdata} carries no block or stratum
#' column and \code{survey.area.shp} is a single polygon, so the "survey block"
#' scale at which a rootogram can be computed has to be DEFINED rather than
#' looked up. A regular grid over the gulf is the choice made here. It is
#' arbitrary, but it has the property the alternatives lack: the blocks do not
#' depend on the model, so the same blocks can be compared across candidate
#' models and across the two species. Partitioning by nearest soap-film knot
#' would be more directly tied to the knot question, but each soap variant has
#' its own knot set, so those blocks would shift underneath the comparison.
#'
#' Cells are labelled by column then row from the south-west corner
#' (\code{"C1R1"}, \code{"C1R2"}, ...). Points in cells holding fewer than
#' \code{min_n} points get \code{NA}, since a rootogram over a handful of
#' segments is unreadable; those segments are simply absent from the block
#' figure.
#'
#' @param x,y Numeric vectors of projected coordinates, the same length.
#' @param n Integer vector of length 1 or 2 giving the number of grid columns
#'   and rows. Defaults to \code{c(3, 3)}.
#' @param min_n Minimum number of points for a cell to be kept. Defaults to
#'   \code{30}.
#'
#' @return A \code{data.table} with \code{x}, \code{y}, \code{col}, \code{row}
#'   and \code{block} (a factor, \code{NA} for dropped cells).
#'
#' @examples
#' dsm_spatial_blocks(runif(200), runif(200), n = c(2, 2), min_n = 10)
#'
#' @export
dsm_spatial_blocks <- function(x, y, n = c(3L, 3L), min_n = 30L) {
  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y), length(x) > 0)
  if (length(n) == 1L) n <- c(n, n)
  stopifnot(length(n) == 2L, all(n >= 1L))

  xb <- seq(min(x), max(x), length.out = n[1] + 1L)
  yb <- seq(min(y), max(y), length.out = n[2] + 1L)
  xb[length(xb)] <- xb[length(xb)] + 1e-6      # include the upper edge
  yb[length(yb)] <- yb[length(yb)] + 1e-6

  ci <- as.integer(cut(x, xb, right = FALSE, labels = FALSE))
  ri <- as.integer(cut(y, yb, right = FALSE, labels = FALSE))

  out <- data.table::data.table(x = x, y = y, col = ci, row = ri)
  out[, block := factor(sprintf("C%dR%d", col, row))]
  out[, .n_blk := .N, by = block]
  out[.n_blk < min_n, block := NA]
  out[, .n_blk := NULL]
  out[, block := droplevels(block)]
  out[]
}
