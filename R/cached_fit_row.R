#' Cache one row of a slow model-comparison loop
#'
#' Runs \code{expr} and caches its value on disk under \code{key}, so a loop of
#' expensive fits can be re-entered after an interruption without repeating the
#' work already done.
#'
#' @details
#' Written because a machine crash part-way through the environmental-covariate
#' refits lost about forty minutes of fitting: the loop assembled everything in
#' memory and wrote its CSV only at the end, so a run that died on the sixth of
#' seven fits left nothing behind. Each row is now cached as soon as it is
#' produced, and a re-run skips whatever is already cached.
#'
#' An fs-block DSM on this data takes roughly 11 minutes to fit and a soap-film
#' DSM about 30 seconds, so this matters far more for the former.
#'
#' Cache invalidation is by \code{key} only -- the contents of \code{expr} are
#' not hashed. If the model formula or the data changes, delete the cache
#' directory (or pass \code{force = TRUE}) rather than trusting a stale row.
#'
#' @param key Character scalar naming this row, e.g. \code{"clo_k20"}. Used as
#'   the cache filename, so keep it filesystem-safe.
#' @param dir Directory to hold the cache. Created if absent.
#' @param expr Expression producing the row (typically a one-row
#'   \code{data.table}). Evaluated only when the row is not already cached.
#' @param force If \code{TRUE}, ignore any cached value and re-evaluate.
#' @param quiet If \code{TRUE}, do not report cache hits and timings.
#'
#' @return The value of \code{expr}, either freshly computed or from cache.
#'
#' @examples
#' \dontrun{
#' rows <- lapply(covs, function(cv)
#'   cached_fit_row(sprintf("%s_k20", cv), ".cache", {
#'     m <- fit_model(cv, k = 20)
#'     data.table::data.table(covariate = cv, AIC = AIC(m))
#'   }))
#' data.table::rbindlist(rows)
#' }
#'
#' @export
cached_fit_row <- function(key, dir, expr, force = FALSE, quiet = FALSE) {
  stopifnot(is.character(key), length(key) == 1L, nzchar(key))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(dir, paste0(key, ".rds"))

  if (!force && file.exists(f)) {
    if (!quiet) cat(sprintf("  [cached] %s\n", key))
    return(readRDS(f))
  }

  t0  <- Sys.time()
  val <- eval.parent(substitute(expr))
  saveRDS(val, f)
  if (!quiet)
    cat(sprintf("  [done]   %s  (%.0f s)\n", key,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  val
}


#' Collect every cached row in a directory
#'
#' @param dir Cache directory written by \code{\link{cached_fit_row}}.
#' @param keys Optional character vector restricting which keys to read, in the
#'   order wanted. Missing keys are skipped with a warning.
#'
#' @return A \code{data.table} of the cached rows, row-bound.
#'
#' @export
collect_cached_rows <- function(dir, keys = NULL) {
  if (is.null(keys)) {
    f <- sort(list.files(dir, pattern = "\\.rds$", full.names = TRUE))
  } else {
    f <- file.path(dir, paste0(keys, ".rds"))
    miss <- !file.exists(f)
    if (any(miss)) {
      warning("cached rows missing: ", paste(keys[miss], collapse = ", "),
              call. = FALSE)
      f <- f[!miss]
    }
  }
  if (!length(f)) return(data.table::data.table())
  data.table::rbindlist(lapply(f, readRDS), fill = TRUE)
}
