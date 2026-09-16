# ADB / Claude
# 2026-09-15
#
# Is the common dolphin's spatially structured rootogram misfit the MODEL, or
# the under-resolved interior knot grid?
#
# THE QUESTION
# UTIL_DSM_Diagnostics.R computes its rootograms from the objects in
# dd_output.RData, which are the ORIGINAL soap arm: interior knot grid 10x8
# (40 knots kept), s(x,y) edf 30.9 of 49, edf_frac 0.64. UTIL_DSM_SoapRevised_DD.R
# already established that grid is binding -- refining it to 14x11 (92 knots)
# moves the covariate-free base 24.19 AIC units and drops edf_frac to 0.45 --
# and UTIL_DSM_SoapTuning_DD.R shows AIC keeps falling well past that.
#
# So the block-scale rootogram finding (NW block over-predicted across every
# positive bin, mid-west block the reverse) is confounded. A spatial surface
# that cannot bend as much as the data wants would produce exactly that
# pattern, and so would a genuinely mis-specified model. The stored fit cannot
# tell them apart, because only one grid was ever rootogrammed.
#
# WHAT THIS DOES
# Refits ONE formula -- dd.dsm.soap.season.year, the model the report presents
# -- at the two knot grids, and compares the rootograms with IDENTICAL bins.
# That is the minimum that discriminates:
#   block structure persists at 92 knots -> it is the model
#   block structure collapses            -> it was the grid
#
# The k = 20 covariate bump of the revised arm is irrelevant here: this formula
# is count ~ s(x,y,so) + season + s(Ano) and carries no environmental smooth,
# so the knot grid is the ONLY thing that differs between the two fits.
#
# Deliberately NOT a knot sweep. The methods text uses the block scale to
# investigate knot number and location and says "(not shown)"; that is
# exploratory use, not a figure. Nine grids x 22 bins x 8 blocks answers a
# question nobody asked. If the two-grid comparison says the grid matters,
# widening it is a separate decision.
#
# GUARDS. Two, both necessary. The boundary and knots are RECONSTRUCTED here
# rather than stored, so a refit at the original grid must reproduce the stored
# AIC (6100.53) or a reconstruction mismatch would masquerade as a knot effect;
# and the refit at 14x11 must reproduce the revised arm's AIC (6076.34) from
# DD_soap_revised_selection.csv or this is not the arm that CSV describes.
#
# OUTPUT  output/CommonDolphin/DSM/DD_rootogram_knotgrid.csv
#                                  DD_rootogram_knotgrid_block.csv
#                                  DD_rootogram_knotgrid.png
#
# COST  two soap fits, roughly 45-120 s each, both cached.

library(dsm)
library(mgcv)
library(data.table)
library(ggplot2)
library(patchwork)

source(file.path(here::here(), "R", "dsm_rootogram.R"))
source(file.path(here::here(), "R", "dsm_correlogram.R"))   # dsm_kcheck
source(file.path(here::here(), "R", "cached_fit_row.R"))

if (!exists("dd.dsm.soap.season.year")) {
  message("loading output/CommonDolphin/dd_output.RData")
  load("output/CommonDolphin/dd_output.RData", envir = .GlobalEnv)
}

.diag_dir <- file.path("output", "CommonDolphin", "DSM")
dir.create(.diag_dir, showWarnings = FALSE, recursive = TRUE)
.cache <- file.path(.diag_dir, ".cache_rgknots_dd")

KNOT_NGRID_REV <- c(14L, 11L)
AIC_STORED_ORIG <- 6100.53   # DD_soap_revised_selection.csv, arm = original
AIC_STORED_REV  <- 6076.34   # DD_soap_revised_selection.csv, arm = revised
RG_MAX_COUNT    <- 20L
RG_TAIL_MULT    <- 2
BLOCK_GRID      <- c(3L, 3L)
BLOCK_MIN_N     <- 60L
BLOCK_BREAKS    <- c(0, 1, 2, 4, 8, Inf)

# ---------------------------------------------------------------------------
# 1. Knot sets, and the fitter -- lifted verbatim from
#    UTIL_DSM_SoapRevised_DD.R so the two arms cannot drift apart
# ---------------------------------------------------------------------------
.bnd_loop <- bnd_soap[[1]]
.bmat     <- cbind(.bnd_loop$x, .bnd_loop$y)
.bnd_dist <- function(px, py)
  vapply(seq_along(px), function(i)
    min(sqrt((.bnd_loop$x - px[i])^2 + (.bnd_loop$y - py[i])^2)), numeric(1))
.kn   <- make.soapgrid(.bnd_loop, n.grid = KNOT_NGRID_REV)
.keep <- as.logical(in.out(.bmat, cbind(.kn$x, .kn$y))) &
  .bnd_dist(.kn$x, .kn$y) > knot_buffer
knots_revised <- data.frame(x = .kn$x[.keep], y = .kn$y[.keep])

cat(sprintf("interior knots: original %s -> %d | revised %s -> %d\n",
            paste(knot_ngrid, collapse = "x"), nrow(knots),
            paste(KNOT_NGRID_REV, collapse = "x"), nrow(knots_revised)))

# dsm() resolves `knots` by NAME, so the active set has to be staged globally
.fit_soap <- function(rhs, knots_use) {
  assign(".knots_active", knots_use, envir = globalenv())
  rhs <- if (nzchar(rhs)) paste("+", rhs) else ""
  f <- as.formula(sprintf(
    'count ~ s(x, y, bs = "so", xt = list(bnd = bnd_soap), k = 10) %s', rhs))
  environment(f) <- globalenv()
  dsm(f, ddf.obj = df.dd, segment.data = segdata,
      observation.data = obsdata_dd_mod,
      family = tw(link = "log"), method = "REML", knots = .knots_active)
}

# ---------------------------------------------------------------------------
# 2. The two fits, cached
# ---------------------------------------------------------------------------
m_orig <- cached_fit_row("fit_orig_10x8",  .cache,
                         .fit_soap("season + s(Ano)", knots))
m_rev  <- cached_fit_row("fit_rev_14x11",  .cache,
                         .fit_soap("season + s(Ano)", knots_revised))

.chk <- function(m, target, what) {
  a <- AIC(m)
  ok <- isTRUE(all.equal(a, target, tolerance = 1e-4))
  cat(sprintf("guard %-8s refit AIC %.2f vs stored %.2f -> %s\n",
              what, a, target, if (ok) "OK" else "MISMATCH"))
  ok
}
.g1 <- .chk(m_orig, AIC_STORED_ORIG, "original")
.g2 <- .chk(m_rev,  AIC_STORED_REV,  "revised")
if (!.g1)
  stop("the reconstructed boundary/knots do not reproduce the stored original ",
       "fit, so any difference below could be reconstruction rather than knots.")
if (!.g2)
  warning("the 14x11 refit does not match the revised arm's stored AIC; the ",
          "comparison still stands as a knot contrast but this is not the arm ",
          "DD_soap_revised_selection.csv describes.", call. = FALSE)

.arms <- list(`original (10x8, 40 knots)` = m_orig,
              `revised (14x11, 92 knots)` = m_rev)

cat("\n=== the two fits ===\n")
print(rbindlist(lapply(names(.arms), function(a) {
  m <- .arms[[a]]; kc <- dsm_kcheck(m, n.rep = 200)[smooth == "s(x,y)"]
  pars <- dsm_tweedie_pars(m)
  data.table(arm = a, n = length(m$y), df = round(attr(logLik(m), "df"), 2),
             AIC = round(AIC(m), 2), Dev = round(summary(m)$dev.expl, 3),
             p = round(pars$p, 4), phi = round(pars$phi, 3),
             edf_xy = round(kc$edf, 2), k_prime_xy = kc$k_prime,
             edf_frac_xy = kc$edf_frac)
})))

# ---------------------------------------------------------------------------
# 3. Pooled rootograms, IDENTICAL bins
#
# Bins come from the original fit and are handed to both. Letting each fit pick
# its own would make the two tables incomparable row by row, which is the only
# thing this script is for.
# ---------------------------------------------------------------------------
rg_breaks <- dsm_rootogram(m_orig, max_count = RG_MAX_COUNT)$upper
.tail_cut <- max(rg_breaks[is.finite(rg_breaks)])
.tail     <- .tail_cut * RG_TAIL_MULT^seq_len(20)
.tail     <- .tail[.tail < max(m_orig$y)]
rg_breaks <- c(rg_breaks[is.finite(rg_breaks)], .tail, Inf)

rg <- rbindlist(lapply(names(.arms), function(a)
  cbind(arm = a, dsm_rootogram(.arms[[a]], breaks = rg_breaks))))
rg[, arm := factor(arm, levels = names(.arms))]

cat("\n=== pooled rootogram, same bins ===\n")
print(dcast(rg, bin ~ arm, value.var = c("observed", "expected", "resid"))[
  , lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\nbins outside the band:\n")
print(rg[, .(n_bins = .N, outside = sum(sig),
             worst_bin = as.character(bin[which.max(abs(resid))]),
             worst_resid = round(resid[which.max(abs(resid))], 2)), by = arm])

fwrite(rg, file.path(.diag_dir, "DD_rootogram_knotgrid.csv"))

# ---------------------------------------------------------------------------
# 4. Block rootograms -- THE DISCRIMINATING COMPARISON
# ---------------------------------------------------------------------------
.blk  <- dsm_spatial_blocks(m_orig$data$x, m_orig$data$y,
                            n = BLOCK_GRID, min_n = BLOCK_MIN_N)
.grp  <- ifelse(!is.na(.blk$block), as.character(.blk$block), NA)

rg_blk <- rbindlist(lapply(names(.arms), function(a) {
  x <- dsm_rootogram(.arms[[a]], breaks = BLOCK_BREAKS, group = .grp)
  cbind(arm = a, x[!is.na(group)])
}))
rg_blk[, arm := factor(arm, levels = names(.arms))]
rg_blk[, group := droplevels(group)]

cat("\n=== by block: does the structure survive the finer grid? ===\n")
.s <- rg_blk[, .(n_seg = sum(observed), outside = sum(sig),
                 sum_resid = round(sum(resid), 2),
                 worst_bin = as.character(bin[which.max(abs(resid))]),
                 worst_resid = round(resid[which.max(abs(resid))], 2)),
             by = .(group, arm)]
print(dcast(.s, group ~ arm, value.var = c("outside", "sum_resid",
                                           "worst_resid")))
cat("\ntotal bars outside the band, over all blocks:\n")
print(rg_blk[, .(bars = .N, outside = sum(sig),
                 mean_abs_resid = round(mean(abs(resid)), 3)), by = arm])

fwrite(rg_blk, file.path(.diag_dir, "DD_rootogram_knotgrid_block.csv"))

# ---------------------------------------------------------------------------
# 5. Figure: suspended, the two arms side by side
# ---------------------------------------------------------------------------
rg[, bin_i := as.integer(bin)]
rg_blk[, bin_i := as.integer(bin)]
rg_blk[, `:=`(b_col = as.integer(sub("^C([0-9]+)R.*$", "\\1", group)),
              b_row = as.integer(sub("^.*R([0-9]+)$", "\\1", group)))]
rg_blk[, row_f := factor(b_row, levels = rev(sort(unique(b_row))))]

.fill <- scale_fill_manual(values = c(`FALSE` = "grey85", `TRUE` = "#f0b27a"),
                           labels = c("within band", "outside band"),
                           name = NULL, drop = FALSE)
.thm <- theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

p_pool <- ggplot(rg, aes(bin_i)) +
  geom_ribbon(aes(ymin = -band, ymax = band), fill = "grey88") +
  geom_col(aes(y = resid, fill = sig), width = .8, colour = "grey30",
           linewidth = .2) +
  geom_hline(yintercept = 0, linewidth = .3) +
  .fill +
  scale_x_continuous(breaks = unique(rg$bin_i), labels = levels(rg$bin)) +
  facet_wrap(~ arm, ncol = 1) +
  labs(subtitle = "Whole survey area", x = "Count per segment",
       y = "sqrt(obs) - sqrt(exp)") +
  .thm

p_blk <- ggplot(rg_blk, aes(bin_i)) +
  geom_ribbon(aes(ymin = -band, ymax = band), fill = "grey88") +
  geom_col(aes(y = resid, fill = sig), width = .8, colour = "grey30",
           linewidth = .2) +
  geom_hline(yintercept = 0, linewidth = .3) +
  .fill +
  scale_x_continuous(breaks = unique(rg_blk$bin_i), labels = levels(rg_blk$bin)) +
  facet_grid(row_f + arm ~ b_col, labeller = label_both) +
  labs(subtitle = "By survey block (b_col across, row_f up)",
       x = "Count per segment", y = "sqrt(obs) - sqrt(exp)") +
  .thm

p <- (p_pool | p_blk) +
  patchwork::plot_layout(guides = "collect", widths = c(1, 1.6)) +
  patchwork::plot_annotation(
    title = "Common dolphin: does the rootogram misfit follow the knot grid?",
    subtitle = paste("count ~ s(x,y,so) + season + s(Ano), refitted at two",
                     "interior knot grids with identical bins.",
                     "\nIf the block structure survives the finer grid it is the",
                     "model; if it collapses it was the grid.")) &
  theme(legend.position = "bottom")

ggsave(file.path(.diag_dir, "DD_rootogram_knotgrid.png"), p,
       width = 15, height = 12, dpi = 150, limitsize = FALSE)

cat("\nwrote knot-grid rootogram comparison to", .diag_dir, "\n")
