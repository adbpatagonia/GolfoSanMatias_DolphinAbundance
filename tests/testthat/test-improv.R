# Tests for improv()
#
# Run with:
#   testthat::test_file("tests/testthat/test-improv.R")
#   # or, for the whole suite:
#   testthat::test_dir("tests/testthat")

# The repo is an analysis project, not an R package, so make the function
# available regardless of how the test is invoked.
if (!exists("improv", mode = "function")) {
  candidates <- c(
    file.path("..", "..", "R", "improv.r"),  # wd = tests/testthat (test_file/test_dir)
    file.path("R", "improv.r")               # wd = project root
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) stop("Could not locate R/improv.r to source for testing.")
  source(hit)
}

test_that("returns a numeric of the recycled length", {
  expect_type(improv(100, 50), "double")
  expect_length(improv(100, 50), 1L)
  expect_length(improv(c(1, 2, 3), c(1, 2, 3)), 3L)
})

test_that("matches the closed form 100 * (old - new) / old (independent check)", {
  old <- c(0.42, 100, 200, 1000)
  new <- c(0.30,  50, 200, 1050)
  expect_equal(improv(old, new), round(100 * (old - new) / old, 1))
})

test_that("recovers a known percentage reduction", {
  # build `new` as `old` reduced by a target percentage, then recover it
  old <- c(200, 80, 1000)
  p   <- c(25,  10,   -5)          # last one is an *increase* (negative improvement)
  new <- old * (1 - p / 100)
  expect_equal(improv(old, new), p)
})

test_that("sign convention: smaller new is a positive improvement", {
  expect_gt(improv(100, 50),  0)   # reduction  -> positive
  expect_lt(improv(100, 200), 0)   # increase   -> negative
  expect_equal(improv(200, 200), 0)  # unchanged -> zero
})

test_that("known values", {
  expect_equal(improv(0.42, 0.30),  28.6)
  expect_equal(improv(0.30, 0.42), -40.0)
  expect_equal(improv(100, 50),     50.0)
  expect_equal(improv(100, 200),  -100.0)
})

test_that("result is rounded to one decimal place", {
  expect_equal(improv(3, 2), 33.3)                 # 33.333... -> 33.3
  x <- improv(7, 3)
  expect_equal(x, round(x, 1))
})

test_that("vectorised element-wise and recycles a scalar", {
  old <- c(0.42, 1200)
  new <- c(0.30,  950)
  res <- improv(old, new)
  expect_length(res, 2L)
  for (i in seq_along(old)) expect_equal(res[i], improv(old[i], new[i]))

  # scalar `old` recycled against a vector `new`
  expect_equal(improv(100, c(50, 200)), c(50, -100))
})

test_that("edge cases: old = 0 and NA propagate as in base arithmetic", {
  expect_equal(improv(0,  5), -Inf)
  expect_equal(improv(0, -5),  Inf)
  expect_true(is.nan(improv(0, 0)))
  expect_true(is.na(improv(NA_real_, 5)))
  expect_true(is.na(improv(5, NA_real_)))
})
