# Tests for dsm_rootogram() / dsm_tweedie_pars()
#
# Run with:
#   testthat::test_file("tests/testthat/test-dsm_rootogram.R")
#   # or the whole suite:
#   testthat::test_dir("tests/testthat")
#
# The load-bearing test here is "analytic expected counts match a Monte Carlo
# simulation from the fitted model". A rootogram built on a wrong Tweedie power
# or a wrong scale parameter looks perfectly plausible -- it just reads as model
# misfit -- so eyeballing the figure cannot tell a diagnostic from a decorated
# bug. Only the closed loop (fit -> extract (p, phi) -> analytic bin
# probabilities vs. rtweedie replicates) can.

# The repo is an analysis project, not an R package, so make the functions
# available regardless of how the test is invoked.
if (!exists("dsm_rootogram", mode = "function")) {
  candidates <- c(
    file.path("..", "..", "R", "dsm_rootogram.R"),  # wd = tests/testthat
    file.path("R", "dsm_rootogram.R")               # wd = project root
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) stop("Could not locate R/dsm_rootogram.R to source for testing.")
  source(hit)
}

has_deps <- requireNamespace("mgcv", quietly = TRUE) &&
  requireNamespace("tweedie", quietly = TRUE) &&
  requireNamespace("data.table", quietly = TRUE)

# A toy Tweedie GAM with a KNOWN p and phi, on counts with a lot of zeros --
# the same shape as a segment-level DSM response.
if (has_deps) {
  # mgcv must be ATTACHED, not merely available: the closure tw() builds calls
  # ldTweedie() unqualified, so a namespace-qualified mgcv::gam(family =
  # mgcv::tw()) fails with "could not find function ldTweedie".
  suppressMessages(library(mgcv))

  set.seed(20260910)
  n_toy   <- 800
  p_true  <- 1.4
  phi_true <- 1.8
  toy <- data.frame(x = runif(n_toy, -1, 1), z = runif(n_toy, -1, 1))
  mu_toy   <- exp(0.4 + 1.2 * toy$x - 0.8 * toy$z^2)
  toy$y    <- round(tweedie::rtweedie(n_toy, xi = p_true, mu = mu_toy, phi = phi_true))
  m_toy    <- mgcv::gam(y ~ s(x) + s(z), data = toy, family = mgcv::tw(),
                        method = "REML")
  blk_toy  <- factor(ifelse(toy$x < 0, "west", "east"))

  # A second fit on the UNROUNDED Tweedie draws, for the parameter-recovery
  # check only. Rounding to counts is not a Tweedie operation -- it moves every
  # draw in (0, 0.5) onto the atom at zero -- and mgcv duly returns p = 1.14
  # rather than 1.4 for m_toy. That is the rounding, not a bug in the
  # extraction, so recovery has to be tested on data that really is Tweedie.
  n_cont  <- 2000
  cont    <- data.frame(x = runif(n_cont, -1, 1), z = runif(n_cont, -1, 1))
  cont$y  <- tweedie::rtweedie(n_cont, xi = p_true, phi = phi_true,
                               mu = exp(0.4 + 1.2 * cont$x - 0.8 * cont$z^2))
  m_cont  <- mgcv::gam(y ~ s(x) + s(z), data = cont, family = mgcv::tw(),
                       method = "REML")
}

# ---------------------------------------------------------------------------
# Parameter extraction
# ---------------------------------------------------------------------------
test_that("dsm_tweedie_pars recovers the p and phi the fit was simulated from", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  pars <- dsm_tweedie_pars(m_cont)

  expect_named(pars, c("p", "phi", "fixed_p"))
  # Loose tolerances: this checks that the RIGHT parameter came back on the
  # RIGHT scale, not that mgcv estimates it precisely. getTheta(FALSE) returns
  # about -1.35 here, so the (1, 2) bounds alone catch the wrong accessor.
  expect_gt(pars$p, 1)
  expect_lt(pars$p, 2)
  expect_equal(pars$p, p_true, tolerance = 0.1)
  expect_equal(pars$phi, phi_true, tolerance = 0.2)
  expect_equal(pars$phi, m_cont$scale)
})

test_that("dsm_tweedie_pars agrees with the p printed in the family string", {
  skip_if_not_installed("mgcv")
  p_str <- as.numeric(sub(".*p *= *([0-9.]+).*", "\\1", m_toy$family$family))
  expect_equal(dsm_tweedie_pars(m_toy)$p, p_str, tolerance = 1e-3)
})

test_that("dsm_tweedie_pars reads p off a fixed-p Tweedie() fit", {
  skip_if_not_installed("mgcv")
  # Tweedie(p) is an ordinary family with no getTheta(), so p has to come from
  # the family string. These fits return AIC = NA and so never enter the
  # model-selection tables, but a rootogram for one is perfectly well defined.
  m_fixed <- mgcv::gam(y ~ s(x), data = toy,
                       family = mgcv::Tweedie(p = 1.4, link = "log"))
  pars <- dsm_tweedie_pars(m_fixed)
  expect_equal(pars$p, 1.4)
  expect_true(pars$fixed_p)
  expect_false(dsm_tweedie_pars(m_toy)$fixed_p)
})

test_that("dsm_tweedie_pars rejects a non-Tweedie fit", {
  skip_if_not_installed("mgcv")
  m_pois <- mgcv::gam(y ~ s(x), data = toy, family = poisson())
  expect_error(dsm_tweedie_pars(m_pois), "Tweedie")
  m_gaus <- mgcv::gam(y ~ s(x), data = toy)
  expect_error(dsm_tweedie_pars(m_gaus), "Tweedie")
})

test_that("dsm_tweedie_pars refuses a Tweedie power outside (1, 2)", {
  # a stub rather than a real fit: mgcv may or may not accept p = 1 at
  # construction, and what is under test is the guard, not mgcv.
  stub <- structure(list(family = list(family = "Tweedie(1)"), scale = 2),
                    class = c("gam", "glm", "lm"))
  expect_error(dsm_tweedie_pars(stub), "1 and 2")
  stub$family$family <- "Tweedie(2)"
  expect_error(dsm_tweedie_pars(stub), "1 and 2")
})

# ---------------------------------------------------------------------------
# The CDF engine, against an independent implementation
# ---------------------------------------------------------------------------
test_that("dsm_tweedie_cdf agrees with tweedie::ptweedie", {
  skip_if_not_installed("tweedie")
  # tweedie::ptweedie is the reference but is far too slow to use as the engine
  # (one bin edge over ~6300 segments takes minutes), hence the compound
  # Poisson-gamma series. Small n here so the reference is affordable.
  set.seed(11)
  mu <- c(0.01, 0.2, 1, 3, 12)
  p  <- 1.45
  phi <- 2.1
  q  <- c(0, 1, 2, 5, 30)
  ours <- dsm_tweedie_cdf(q, mu, p, phi)
  ref  <- outer(mu, q, function(m, qq)
    tweedie::ptweedie(q = qq, mu = m, phi = phi, power = p))

  expect_equal(dim(ours), c(length(mu), length(q)))
  expect_equal(ours, ref, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("dsm_tweedie_cdf is a proper CDF: monotone, in [0,1], 1 at Inf", {
  mu <- c(0.05, 0.5, 4, 25)
  cdf <- dsm_tweedie_cdf(c(0, 1, 2, 4, 8, Inf), mu, p = 1.6, phi = 1.3)
  expect_true(all(cdf >= 0 & cdf <= 1))
  expect_true(all(apply(cdf, 1, diff) >= -1e-12))
  expect_equal(unname(cdf[, ncol(cdf)]), rep(1, length(mu)), tolerance = 1e-8)
  expect_true(all(is.finite(cdf)))
})

# ---------------------------------------------------------------------------
# Shape of the returned table
# ---------------------------------------------------------------------------
test_that("dsm_rootogram returns one row per bin, partitioning the response", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  rg <- dsm_rootogram(m_toy)

  expect_s3_class(rg, "data.table")
  expect_true(all(c("bin", "lower", "upper", "observed", "expected",
                    "sqrt_obs", "sqrt_exp", "ymin", "ymax", "resid",
                    "se", "band", "sig") %in% names(rg)))
  # bins must tile [0, Inf): first is the point mass at zero, last is open
  expect_equal(rg$lower[1], 0)
  expect_equal(rg$upper[1], 0)
  expect_true(is.infinite(rg$upper[nrow(rg)]))
  expect_equal(rg$lower[-1], rg$upper[-nrow(rg)])
  # and therefore account for every observation and all the probability
  expect_equal(sum(rg$observed), length(m_toy$y))
  expect_equal(sum(rg$expected), length(m_toy$y), tolerance = 1e-4)
})

test_that("dsm_rootogram hangs the bars from the expected curve", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  rg <- dsm_rootogram(m_toy)

  expect_equal(rg$ymax, sqrt(rg$expected))
  expect_equal(rg$ymin, sqrt(rg$expected) - sqrt(rg$observed))
  expect_equal(rg$resid, sqrt(rg$observed) - sqrt(rg$expected))
})

test_that("dsm_rootogram honours explicit breaks", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  rg <- dsm_rootogram(m_toy, breaks = c(0, 1, 3, Inf))

  expect_equal(nrow(rg), 4L)                       # {0}, (0,1], (1,3], (3,Inf)
  expect_equal(rg$observed[rg$bin == "0"], sum(m_toy$y == 0))
  expect_equal(rg$observed[rg$bin == "2-3"], sum(m_toy$y > 1 & m_toy$y <= 3))
  expect_equal(rg$observed[nrow(rg)], sum(m_toy$y > 3))
})

# ---------------------------------------------------------------------------
# Correctness: the closed loop
# ---------------------------------------------------------------------------
test_that("the zero bin equals the closed-form Tweedie zero probability", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  rg   <- dsm_rootogram(m_toy)
  pars <- dsm_tweedie_pars(m_toy)
  mu   <- as.numeric(stats::fitted(m_toy))

  # compound Poisson-gamma: P(Y = 0) = exp(-lambda), lambda = mu^(2-p)/(phi(2-p))
  closed <- sum(exp(-mu^(2 - pars$p) / (pars$phi * (2 - pars$p))))
  expect_equal(rg$expected[1], closed, tolerance = 1e-6)
})

test_that("analytic expected bin counts match Monte Carlo from the fitted model", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  rg    <- dsm_rootogram(m_toy)
  pars  <- dsm_tweedie_pars(m_toy)
  mu    <- as.numeric(stats::fitted(m_toy))

  set.seed(7)
  n_rep <- 400L
  sim <- vapply(seq_len(n_rep), function(i) {
    ysim <- tweedie::rtweedie(length(mu), xi = pars$p, mu = mu, phi = pars$phi)
    # same binning rule as the helper: the atom {0}, then (lower, upper]
    vapply(seq_len(nrow(rg)), function(j)
      if (j == 1L) sum(ysim == 0)
      else sum(ysim > rg$lower[j] & ysim <= rg$upper[j]), numeric(1))
  }, numeric(nrow(rg)))

  mc_mean <- rowMeans(sim)
  mc_se   <- apply(sim, 1, stats::sd) / sqrt(n_rep)
  # every bin within 4 Monte Carlo standard errors of the analytic value
  # (+ a small absolute floor for bins whose expectation is near zero)
  expect_true(all(abs(mc_mean - rg$expected) < 4 * mc_se + 0.5),
              info = paste(sprintf("bin %s: analytic %.2f, MC %.2f (se %.3f)",
                                   rg$bin, rg$expected, mc_mean, mc_se),
                           collapse = "; "))
})

# ---------------------------------------------------------------------------
# Blocks
# ---------------------------------------------------------------------------
test_that("grouped rootograms partition the pooled one", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  rg  <- dsm_rootogram(m_toy)
  rgb <- dsm_rootogram(m_toy, breaks = rg$upper, group = blk_toy)

  expect_true("group" %in% names(rgb))
  expect_setequal(unique(as.character(rgb$group)), c("east", "west"))
  agg <- rgb[, list(observed = sum(observed), expected = sum(expected)), by = "bin"]
  expect_equal(agg$observed, rg$observed)
  expect_equal(agg$expected, rg$expected, tolerance = 1e-6)
})

test_that("dsm_rootogram rejects a group vector of the wrong length", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  expect_error(dsm_rootogram(m_toy, group = blk_toy[-1]), "length")
})

# ---------------------------------------------------------------------------
# Uncertainty band
# ---------------------------------------------------------------------------
test_that("band is the 2-SE half-width of sqrt(observed) under Poisson-binomial", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tweedie")
  rg <- dsm_rootogram(m_toy)
  # delta method: se(sqrt(O)) = se(O) / (2 sqrt(E)), so 2 SE = se(O) / sqrt(E)
  expect_equal(rg$band, rg$se / sqrt(rg$expected))
  expect_equal(rg$sig, abs(rg$resid) > rg$band)
  expect_true(all(rg$se <= sqrt(rg$expected) + 1e-8))   # Var <= E for binomial
})

# ---------------------------------------------------------------------------
# Spatial blocks
# ---------------------------------------------------------------------------
test_that("dsm_spatial_blocks tiles the bounding box and keeps every point", {
  set.seed(3)
  x <- runif(600, 100, 200)
  y <- runif(600, -50, 50)
  b <- dsm_spatial_blocks(x, y, n = c(3L, 2L), min_n = 1L)

  expect_s3_class(b, "data.table")
  expect_equal(nrow(b), 600L)
  expect_equal(b$x, x)
  expect_false(anyNA(b$block))
  expect_setequal(as.character(unique(b$block)),
                  paste0("C", rep(1:3, each = 2), "R", rep(1:2, 3)))
  # every point assigned exactly once, so the block sizes sum to n
  expect_equal(sum(table(b$block)), 600L)
  expect_true(all(b$col %in% 1:3), all(b$row %in% 1:2))
})

test_that("dsm_spatial_blocks includes the points on the upper edges", {
  # the max of x and y must land in the LAST cell, not fall outside the cut
  b <- dsm_spatial_blocks(c(0, 1, 2), c(0, 1, 2), n = c(2L, 2L), min_n = 1L)
  expect_false(anyNA(b$col))
  expect_false(anyNA(b$row))
  expect_equal(b$col, c(1L, 2L, 2L))
  expect_equal(b$row, c(1L, 2L, 2L))
})

test_that("dsm_spatial_blocks drops under-sampled cells to NA", {
  # 100 points in the SW cell, 3 in the NE cell
  x <- c(runif(100, 0, 1), c(9, 9.5, 9.9))
  y <- c(runif(100, 0, 1), c(9, 9.5, 9.9))
  b <- dsm_spatial_blocks(x, y, n = c(2L, 2L), min_n = 30L)

  expect_equal(sum(is.na(b$block)), 3L)
  expect_equal(nlevels(b$block), 1L)          # dropped level really gone
  expect_true(all(is.na(b$block[101:103])))
})

test_that("dsm_spatial_blocks recycles a scalar n and validates its inputs", {
  b1 <- dsm_spatial_blocks(runif(100), runif(100), n = 2L, min_n = 1L)
  b2 <- dsm_spatial_blocks(runif(100), runif(100), n = c(2L, 2L), min_n = 1L)
  expect_equal(nlevels(b1$block), nlevels(b2$block))
  expect_error(dsm_spatial_blocks(1:10, 1:9), "length")
})
