# Tests for lnorm_ci()
#
# Run with:
#   testthat::test_file("tests/testthat/test-lnorm_ci.R")
#   # or, for the whole suite:
#   testthat::test_dir("tests/testthat")

# The repo is an analysis project, not an R package, so make the function
# available regardless of how the test is invoked.
if (!exists("lnorm_ci", mode = "function")) {
  candidates <- c(
    file.path("..", "..", "R", "lnorm_ci.R"),  # wd = tests/testthat (test_file/test_dir)
    file.path("R", "lnorm_ci.R")               # wd = project root
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) stop("Could not locate R/lnorm_ci.R to source for testing.")
  source(hit)
}

test_that("returns a named list of bounds", {
  res <- lnorm_ci(1200, 0.25)
  expect_type(res, "list")
  expect_named(res, c("lo", "hi"))
  expect_length(res$lo, 1L)
  expect_length(res$hi, 1L)
})

test_that("bounds match the lognormal quantiles (independent check via qlnorm)", {
  N     <- 1200
  cv    <- 0.25
  alpha <- 0.05
  sdlog <- sqrt(log(cv^2 + 1))

  res <- lnorm_ci(N, cv, alpha = alpha)

  expect_equal(res$lo, qlnorm(alpha / 2,     meanlog = log(N), sdlog = sdlog))
  expect_equal(res$hi, qlnorm(1 - alpha / 2, meanlog = log(N), sdlog = sdlog))
})

test_that("interval brackets the point estimate and is log-symmetric", {
  N   <- 1200
  res <- lnorm_ci(N, 0.25)

  expect_lt(res$lo, N)
  expect_gt(res$hi, N)

  # geometric mean of the bounds equals N  <=>  symmetric on the log scale
  expect_equal(sqrt(res$lo * res$hi), N)
  expect_equal(log(N) - log(res$lo), log(res$hi) - log(N))
})

test_that("cv = 0 collapses the interval to the point estimate", {
  res <- lnorm_ci(1200, 0)
  expect_equal(res$lo, 1200)
  expect_equal(res$hi, 1200)
})

test_that("smaller alpha widens the interval", {
  wide   <- lnorm_ci(1200, 0.25, alpha = 0.01)
  narrow <- lnorm_ci(1200, 0.25, alpha = 0.05)

  expect_lt(wide$lo, narrow$lo)
  expect_gt(wide$hi, narrow$hi)
})

test_that("larger cv widens the interval", {
  small_cv <- lnorm_ci(1200, 0.10)
  large_cv <- lnorm_ci(1200, 0.40)

  expect_lt(large_cv$lo, small_cv$lo)
  expect_gt(large_cv$hi, small_cv$hi)
})

test_that("vectorised over N and cv", {
  N   <- c(500, 1200, 2000)
  cv  <- c(0.30, 0.25, 0.15)
  res <- lnorm_ci(N, cv)

  expect_length(res$lo, 3L)
  expect_length(res$hi, 3L)

  # each element equals the scalar call on that element
  for (i in seq_along(N)) {
    one <- lnorm_ci(N[i], cv[i])
    expect_equal(res$lo[i], one$lo)
    expect_equal(res$hi[i], one$hi)
  }
})

test_that("scalar cv is recycled against a vector N", {
  res <- lnorm_ci(c(500, 1200, 2000), 0.25)
  expect_length(res$lo, 3L)
  expect_equal(res$lo, lnorm_ci(c(500, 1200, 2000), rep(0.25, 3))$lo)
})

test_that("non-numeric or empty N / cv are rejected", {
  expect_error(lnorm_ci("a", 0.25),        "must be a non-empty numeric")
  expect_error(lnorm_ci(numeric(0), 0.25), "must be a non-empty numeric")
  expect_error(lnorm_ci(1200, "b"),        "must be a non-empty numeric")
  expect_error(lnorm_ci(1200, numeric(0)), "must be a non-empty numeric")
})

test_that("out-of-range N and cv are rejected", {
  expect_error(lnorm_ci(0, 0.25),    "strictly positive")
  expect_error(lnorm_ci(-5, 0.25),   "strictly positive")
  expect_error(lnorm_ci(1200, -0.1), "non-negative")
})

test_that("NA and non-finite inputs are rejected", {
  expect_error(lnorm_ci(NA_real_, 0.25), "must not contain NA")
  expect_error(lnorm_ci(1200, NA_real_), "must not contain NA")
  expect_error(lnorm_ci(Inf, 0.25),      "must be finite")
  expect_error(lnorm_ci(1200, Inf),      "must be finite")
})

test_that("alpha must be a single number in (0, 1)", {
  expect_error(lnorm_ci(1200, 0.25, alpha = 0),            "must lie in")
  expect_error(lnorm_ci(1200, 0.25, alpha = 1),            "must lie in")
  expect_error(lnorm_ci(1200, 0.25, alpha = -0.1),         "must lie in")
  expect_error(lnorm_ci(1200, 0.25, alpha = c(0.05, 0.1)), "single number")
})

test_that("incompatible non-scalar lengths are rejected (no silent recycling)", {
  expect_error(lnorm_ci(c(1, 2), c(0.1, 0.2, 0.3)), "equal length")
  expect_error(lnorm_ci(c(1, 2, 3, 4), c(0.1, 0.2)), "equal length")
})
