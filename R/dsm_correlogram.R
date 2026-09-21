#' Along-track residual correlogram for a density surface model
#'
#' Computes the lag-\eqn{h} autocorrelation of a fitted \code{\link[dsm]{dsm}}
#' object's residuals along the survey track, for \eqn{h = 1, \dots,}
#' \code{max.lag}. This is the same quantity \code{\link[dsm]{dsm_cor}}
#' computes, and it reproduces that function's values exactly, but it fixes
#' three problems that make \code{dsm_cor} awkward or wrong on this project's
#' data (see \strong{Details}).
#'
#' @details
#' The residuals of a segment and of the segment \eqn{h} places further along
#' the same track are paired within each \code{group}, ordered by
#' \code{order_by}, and correlated. Both the density surface model's AIC and
#' the REML smoothing-parameter selection assume the segments are independent,
#' so a correlogram that decays away from a clearly non-zero lag-1 value is
#' evidence that assumption is violated: the effective sample size is below the
#' nominal one, and AIC will over-select complexity. A correlogram scattered
#' inside the band is evidence of no \emph{unmodelled} correlation -- which is
#' not the same as no correlation, since a flexible spatial term can absorb
#' along-track structure into the fitted surface.
#'
#' Three departures from \code{dsm::dsm_cor}:
#'
#' \enumerate{
#'   \item \strong{Ordering.} \code{dsm_cor} orders segments with
#'     \code{order()} applied directly to the \code{Segment.Label} column. In
#'     this project \code{Sample.Label} is zero-padded to only two digits
#'     (\code{"20060512_01"}) while the segment counter runs to 166 within a
#'     survey day, so a character sort places \code{_100} before \code{_99} and
#'     scrambles the series in 157 of 386 transects. \code{order_by} must
#'     therefore be a \emph{numeric} index; \code{dsm_seg_num} builds it.
#'   \item \strong{Significance band.} \code{dsm_cor} draws its band at
#'     \code{2/sqrt(2 * n_pairs)}, because it takes \code{length()} of a
#'     two-column matrix of pairs rather than the number of pairs. This
#'     function returns \code{2/sqrt(n_pairs)}, which is about 40\% wider.
#'     Both are optimistic: the pairs overlap, so the true envelope is wider
#'     still and an exceedance is weaker evidence than it looks.
#'   \item \strong{No plotting.} \code{dsm_cor} plots on \code{ylim = c(0, 1)},
#'     where the lag-0 value of exactly 1 compresses every other lag onto the
#'     axis. This function returns the numbers and leaves the drawing to the
#'     caller.
#' }
#'
#' The choice of \code{group} sets the scale the correlogram can see.
#' \code{Transect.Label} treats each survey leg as an independent series (386
#' legs, median 15 segments), which discards pairs spanning two legs of the
#' same day and cannot reach beyond about lag 15. \code{traj_id} treats the
#' whole survey day as one series (82 days, median ~77 segments), giving many
#' more pairs per lag and reach out to lag 25, at the cost of pairing segments
#' across a gap between legs. Run both; they should agree.
#'
#' @param model A fitted \code{dsm} (or \code{gam}) object.
#' @param group Grouping vector the same length as the model's data, defining
#'   the independent series (e.g. \code{Transect.Label} or \code{traj_id}).
#' @param order_by Numeric vector the same length as \code{group}, giving the
#'   position of each segment along its track. Must be numeric -- see
#'   \strong{Details}.
#' @param max.lag Integer, largest lag to compute. Defaults to \code{12}.
#' @param resid.type Residual type passed to \code{\link[stats]{residuals}}.
#'   Defaults to \code{"scaled.pearson"}, as \code{dsm_cor} does.
#'
#' @return A \code{data.table}, one row per lag, with columns
#'   \describe{
#'     \item{\code{lag}}{the lag \eqn{h}.}
#'     \item{\code{cor}}{the lag-\eqn{h} residual correlation.}
#'     \item{\code{n_pairs}}{number of segment pairs contributing.}
#'     \item{\code{band}}{\code{2/sqrt(n_pairs)}, the (optimistic) noise
#'       envelope.}
#'     \item{\code{sig}}{\code{TRUE} if \code{abs(cor) > band}. Read this as a
#'       flag, not a test: with many lags and several models, a few
#'       exceedances are expected by chance, and only a \emph{run} of them
#'       starting at lag 1 indicates autocorrelation.}
#'   }
#'
#' @seealso \code{\link{dsm_seg_num}} to build \code{order_by};
#'   \code{\link[dsm]{dsm_cor}} for the original.
#'
#' @references
#' Miller, D.L., Burt, M.L., Rexstad, E.A., & Thomas, L. (2013). Spatial models
#' for distance sampling data: recent developments and future directions.
#' \emph{Methods in Ecology and Evolution} 4:1001-1010.
#'
#' @examples
#' \dontrun{
#' sn <- dsm_seg_num(m$data$Sample.Label)
#' dsm_correlogram(m, m$data$Transect.Label, sn, max.lag = 12)
#' dsm_correlogram(m, m$data$traj_id,        sn, max.lag = 25)
#' }
#'
#' @export
dsm_correlogram <- function(model, group, order_by, max.lag = 12L,
                            resid.type = "scaled.pearson") {
  stopifnot(inherits(model, "gam"), is.numeric(max.lag), length(max.lag) == 1L,
            max.lag >= 1L)
  if (!is.numeric(order_by))
    stop("`order_by` must be numeric: a character segment label sorts ",
         "lexicographically and scrambles the along-track order. ",
         "Use dsm_seg_num().", call. = FALSE)

  r <- stats::residuals(model, type = resid.type)
  if (length(group) != length(r) || length(order_by) != length(r))
    stop(sprintf("length mismatch: residuals %d, group %d, order_by %d. ",
                 length(r), length(group), length(order_by)),
         "NA covariate values drop rows from the fit but not from segdata.",
         call. = FALSE)
  if (anyNA(r)) stop("NA residuals; correlogram cannot be computed.", call. = FALSE)

  # index of each series, ordered along the track
  ord <- lapply(split(seq_along(r), group),
                function(ix) ix[order(order_by[ix])])

  out <- data.table::rbindlist(lapply(seq_len(max.lag), function(h) {
    a <- unlist(lapply(ord, function(ix)
      if (length(ix) > h) r[ix[seq_len(length(ix) - h)]] else NULL))
    if (length(a) < 3L) return(NULL)
    b <- unlist(lapply(ord, function(ix)
      if (length(ix) > h) r[ix[(h + 1L):length(ix)]] else NULL))
    data.table::data.table(lag = h, cor = stats::cor(a, b), n_pairs = length(a))
  }))

  out[, band := 2 / sqrt(n_pairs)]
  out[, sig := abs(cor) > band]
  out[]
}


#' Numeric along-track segment index from a Sample.Label
#'
#' Extracts the trailing segment number from labels of the form
#' \code{"<survey>_<n>"} (e.g. \code{"20060512_01"} -> \code{1}).
#'
#' @details
#' \code{Sample.Label} in this project is zero-padded to two digits but the
#' counter runs to 166 within a survey day, so sorting the labels as character
#' places \code{_100} before \code{_99}. Anything that needs segments in
#' along-track order must sort on this numeric index instead. The counter runs
#' across the whole survey day (\code{traj_id}) rather than restarting at 1 for
#' each \code{Transect.Label}, so within a transect it is a valid \emph{ordering}
#' but not a \code{1..n} key.
#'
#' @param sample_label Character vector of \code{Sample.Label} values.
#'
#' @return An integer vector the same length as \code{sample_label}.
#'
#' @examples
#' dsm_seg_num(c("20060512_01", "20060512_99", "20060512_100"))
#'
#' @export
dsm_seg_num <- function(sample_label) {
  n <- as.integer(sub("^.*_", "", as.character(sample_label)))
  if (anyNA(n))
    stop("could not parse a trailing _<number> from every Sample.Label",
         call. = FALSE)
  n
}


#' Tidy basis-dimension check for a density surface model
#'
#' Wraps \code{\link[mgcv]{k.check}} and returns it as a \code{data.table} with
#' the smooth label as a column, so results for several models can be stacked.
#'
#' @details
#' \code{k.check} compares the estimated degrees of freedom of each smooth with
#' the basis dimension \code{k_prime} it was given, and tests whether the residuals
#' still hold pattern at a finer scale than the basis can represent. The two
#' columns say different things and should not be merged into one verdict:
#'
#' \code{edf} close to \code{k_prime} is evidence the basis is the binding
#' constraint -- the term is as wiggly as \code{k} allowed rather than as
#' wiggly as the penalty wanted -- so its deviance, \code{edf} and hence its
#' contribution to AIC reflect the ceiling. Refit at higher \code{k} and see
#' whether \code{edf} moves.
#'
#' A low \code{k_index} with a small \code{p_value} says only that residual
#' variance is higher between neighbouring covariate values than expected.
#' Raising \code{k} is one cause among several. The discriminating question is
#' whether the \code{k_index} is depressed for one smooth or for all of them: a
#' model-wide depression, especially on a smooth using a small fraction of its
#' basis, points at the mean-variance relationship or at structure the
#' randomisation is picking up elsewhere, not at that smooth's \code{k}. The
#' test is randomisation-based, so \code{p_value} moves between runs; raise
#' \code{n.rep} rather than reading a borderline value closely.
#'
#' A smooth penalised to near-zero \code{edf} is the opposite situation: the
#' term is effectively absent from the model whatever \code{k_prime} it was given.
#'
#' @param model A fitted \code{dsm} (or \code{gam}) object.
#' @param n.rep Number of randomisations for the residual test. Defaults to
#'   \code{400}.
#' @param subsample Passed to \code{\link[mgcv]{k.check}}; the fit is
#'   subsampled above this many rows. Defaults to \code{5000}.
#'
#' @return A \code{data.table} with one row per smooth: \code{smooth},
#'   \code{k_prime}, \code{edf}, \code{k_index}, \code{p_value}, plus
#'   \code{edf_frac} (\code{edf / k_prime}) as a quick read on how close the
#'   fit sits to its basis ceiling.
#'
#' @seealso \code{\link[mgcv]{k.check}}, \code{\link[mgcv]{choose.k}}
#'
#' @examples
#' \dontrun{
#' dsm_kcheck(lo.dsm.xy.byyear.season.depth)
#' }
#'
#' @export
dsm_kcheck <- function(model, n.rep = 400L, subsample = 5000L) {
  stopifnot(inherits(model, "gam"))
  kc <- mgcv::k.check(model, subsample = subsample, n.rep = n.rep)
  if (is.null(kc) || !nrow(kc))
    return(data.table::data.table(smooth = character(), k_prime = numeric(),
                                  edf = numeric(), k_index = numeric(),
                                  p_value = numeric(), edf_frac = numeric()))
  out <- data.table::data.table(
    smooth  = rownames(kc),
    k_prime = as.numeric(kc[, "k'"]),
    edf     = as.numeric(kc[, "edf"]),
    k_index = as.numeric(kc[, "k-index"]),
    p_value = as.numeric(kc[, ncol(kc)])
  )
  out[, edf_frac := round(edf / k_prime, 3)]
  out[]
}


#' Lag-1 residual autocorrelation for a model-selection table
#'
#' @description
#' A one-row summary of \code{dsm_correlogram(..., max.lag = 1)}, meant to be
#' bound onto an AIC ranking so the ranking carries its own health warning.
#'
#' @details
#' AIC -- and the REML smoothing-parameter selection underneath it -- assumes the
#' segments are independent. They are contiguous pieces of one survey track, so
#' that can fail, and when it does the effective sample size is below the nominal
#' one and AIC over-selects complexity. With ~57 candidates per species, a model
#' can therefore win a ranking on an AIC its own residuals do not support.
#'
#' This has already happened twice in this project and been caught only by
#' looking: the DD soap knot sweep kept improving AIC to 485 knots while lag-1
#' rose past its band, and in the LO soap table the nominal winner
#' \code{count ~ s(x,y,so) + s(Ano) + s(sst)} is the one model in the top 15
#' whose residuals are significantly correlated -- every LO model that drops
#' \code{season} fails this check and every model that keeps it passes. Neither
#' is visible from AIC, deltaAIC or deviance. Hence the column.
#'
#' Read \code{lag1_sig == TRUE} as "do not select this model on its AIC", not as
#' "this model is wrong".
#'
#' The band is \code{2 / sqrt(n_pairs)}, so it is IDENTICAL for every model
#' fitted to the same segments; it varies across a table only when the rows were
#' fitted to different data (e.g. a full-data row against an n_obs == 2 subset).
#'
#' Deliberately total: anything that stops the correlogram being computable --
#' a model carrying no \code{$data}, missing \code{Transect.Label} or
#' \code{Sample.Label}, NA residuals, too few pairs -- returns NA rather than
#' erroring, because a diagnostic column must never take a selection table down
#' with it.
#'
#' @param model A fitted \code{dsm}/\code{gam} object.
#' @param resid.type Residual type passed to \code{dsm_correlogram}.
#'
#' @return A list of \code{lag1}, \code{lag1_band}, \code{lag1_sig}.
#'
#' @seealso \code{\link{dsm_correlogram}}, \code{\link{dsm_lag1_cols}}
#' @export
dsm_lag1 <- function(model, resid.type = "scaled.pearson") {
  na_out <- list(lag1 = NA_real_, lag1_band = NA_real_, lag1_sig = NA)
  d <- tryCatch(model$data, error = function(e) NULL)
  if (is.null(d) || !all(c("Transect.Label", "Sample.Label") %in% names(d)))
    return(na_out)
  ct <- tryCatch(
    dsm_correlogram(model, d$Transect.Label, dsm_seg_num(d$Sample.Label),
                    max.lag = 1L, resid.type = resid.type),
    error = function(e) NULL)
  if (is.null(ct) || !nrow(ct)) return(na_out)
  list(lag1      = round(ct$cor[1], 4),
       lag1_band = round(ct$band[1], 4),
       lag1_sig  = ct$sig[1])
}


#' Lag-1 columns for a list of models
#'
#' @description
#' Vectorised \code{\link{dsm_lag1}}: returns one row per model, ready to
#' \code{cbind} onto a selection table.
#'
#' @param models A list of fitted models, or a character vector of object names
#'   to \code{get()} from \code{envir}.
#' @param envir Environment used when \code{models} is a character vector.
#'
#' @return A \code{data.table} with columns \code{lag1}, \code{lag1_band},
#'   \code{lag1_sig}, one row per model, in the order given.
#'
#' @export
dsm_lag1_cols <- function(models, envir = parent.frame()) {
  if (is.character(models))
    models <- lapply(models, function(n) get(n, envir = envir))
  data.table::rbindlist(lapply(models, function(m)
    as.data.frame(dsm_lag1(m), stringsAsFactors = FALSE)))
}


#' Report a selection table's autocorrelation band once
#'
#' @description
#' The band is the same for every model fitted to the same segments, so printing
#' it per row is noise. This prints it once, with a count of how many rows fail,
#' and is meant to be called right before \code{print()}ing a selection table.
#'
#' @param tab A selection table carrying \code{lag1} and \code{lag1_sig}.
#' @param what A label for the table, used in the message.
#'
#' @return \code{tab}, invisibly.
#' @export
dsm_lag1_note <- function(tab, what = "this table") {
  if (!all(c("lag1", "lag1_sig") %in% names(tab))) return(invisible(tab))
  sig  <- sum(tab$lag1_sig %in% TRUE)
  band <- unique(stats::na.omit(tab$lag1_band))
  message(sprintf(
    paste0("%s: lag-1 residual autocorrelation band %s; %d of %d models exceed ",
           "it.
  Rows with lag1_sig = TRUE break the independence their own ",
           "AIC assumes -- do not select on AIC alone."),
    what,
    if (length(band) == 1L) sprintf("%.4f", band) else "varies by row",
    sig, nrow(tab)))
  invisible(tab)
}
