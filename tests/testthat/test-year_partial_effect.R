# Tests for year_partial_effect()
#
# Run with:
#   testthat::test_file("tests/testthat/test-year_partial_effect.R")
#   # or the whole suite:
#   testthat::test_dir("tests/testthat")

# The repo is an analysis project, not an R package, so make the function
# available regardless of how the test is invoked.
if (!exists("year_partial_effect", mode = "function")) {
  candidates <- c(
    file.path("..", "..", "R", "year_partial_effect.R"),  # wd = tests/testthat
    file.path("R", "year_partial_effect.R")               # wd = project root
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) stop("Could not locate R/year_partial_effect.R to source for testing.")
  source(hit)
}

# A small fs-year model + prediction grid, built once (needs mgcv, a Suggests).
has_mgcv <- requireNamespace("mgcv", quietly = TRUE)
if (has_mgcv) {
  set.seed(42)
  n   <- 300
  dat <- data.frame(
    x        = runif(n),
    y        = runif(n),
    year_fac = factor(sample(2010:2013, n, replace = TRUE)),
    season   = factor(sample(c("Spring", "Summer"), n, replace = TRUE))
  )
  dat$count <- rpois(n, 3)
  m    <- mgcv::gam(count ~ s(x, y, year_fac, bs = "fs") + season,
                    data = dat, family = poisson())
  grid <- expand.grid(x = seq(0, 1, 0.25), y = seq(0, 1, 0.25))
  yf   <- levels(m$model$year_fac)
}

test_that("returns a data.frame with one row per year and the expected columns", {
  skip_if_not_installed("mgcv")
  res <- year_partial_effect(m, grid)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("year", "partial", "se", "lower", "upper"))
  expect_equal(nrow(res), length(yf))
  expect_setequal(res$year, as.numeric(yf))
})

test_that("point estimate equals the (equal-weighted) mean term contribution", {
  skip_if_not_installed("mgcv")
  res <- year_partial_effect(m, grid)                       # equal weights
  seas0 <- factor(levels(m$model$season)[1], levels = levels(m$model$season))
  ref <- vapply(yf, function(yy) {
    nd <- data.frame(grid, year_fac = factor(yy, levels = yf), season = seas0)
    mean(predict(m, nd, type = "terms")[, "s(x,y,year_fac)"])
  }, numeric(1))
  expect_equal(res$partial, unname(ref), tolerance = 1e-8)
})

test_that("CI brackets the estimate and se is non-negative", {
  skip_if_not_installed("mgcv")
  res <- year_partial_effect(m, grid)
  expect_true(all(res$se >= 0))
  expect_true(all(res$lower <= res$partial))
  expect_true(all(res$upper >= res$partial))
})

test_that("higher level widens the interval", {
  skip_if_not_installed("mgcv")
  w95 <- year_partial_effect(m, grid, level = 0.95)
  w99 <- year_partial_effect(m, grid, level = 0.99)
  expect_true(all(w99$lower <= w95$lower))
  expect_true(all(w99$upper >= w95$upper))
  expect_equal(w95$partial, w99$partial)   # point estimate unaffected by level
})

test_that("weights are scale-invariant (normalised internally)", {
  skip_if_not_installed("mgcv")
  w1 <- year_partial_effect(m, grid, weights = rep(1, nrow(grid)))
  w2 <- year_partial_effect(m, grid, weights = rep(7, nrow(grid)))
  expect_equal(w1$partial, w2$partial)
  # a non-uniform weighting generally shifts the estimate
  ww <- seq_len(nrow(grid))
  expect_false(isTRUE(all.equal(w1$partial,
                                year_partial_effect(m, grid, weights = ww)$partial)))
})

test_that("input validation", {
  skip_if_not_installed("mgcv")
  expect_error(year_partial_effect(m, data.frame(a = 1, b = 2)),
               "must be a data.frame with numeric columns")
  expect_error(year_partial_effect(m, grid, weights = rep(1, 3)),
               "non-negative numeric vector of length")
  expect_error(year_partial_effect(m, grid, weights = c(-1, rep(1, nrow(grid) - 1))),
               "non-negative")
  expect_error(year_partial_effect(m, grid, level = 1.5), "single number")
  expect_error(year_partial_effect(m, grid, level = 0),   "single number")

  # model without a year_fac term
  m0 <- mgcv::gam(count ~ s(x, y) + season, data = dat, family = poisson())
  expect_error(year_partial_effect(m0, grid), "year_fac")
})
