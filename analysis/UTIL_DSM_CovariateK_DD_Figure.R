# ADB / Claude
# 2026-09-10
#
# Figure: the TWO separate defects in the common-dolphin soap model, and what
# fixing each is worth relative to the reported model.
#
# Reads the CSVs written by UTIL_DSM_CovariateK_DD.R -- no refitting.
#
# THE TWO DEFECTS ARE ORTHOGONAL, so a 2x2 separates them:
#   interior knot grid   41 (stored) vs 92 (denser)     -> the SPATIAL defect
#   covariate basis      k = 10 (default) vs k = 20     -> the COVARIATE defect
#
# In panel B, for each covariate:
#   the VERTICAL GAP between the two lines  = what fixing the knot grid buys
#   the SLOPE of a line                     = what raising covariate k buys
#   lines not parallel                      = interaction, i.e. the covariate
#       needed extra basis only because the surface was coarse (or vice versa)
#
# Everything is plotted as deltaAIC against dd.dsm.soap.season.year (6100.53),
# the model the report presents, so zero is "what is published now".
#
# OUTPUT  output/CommonDolphin/DSM/DD_two_defects.png

library(data.table)
library(ggplot2)

.diag_dir <- file.path("output", "CommonDolphin", "DSM")
grid2x2 <- fread(file.path(.diag_dir, "DD_covariate_2x2_grid.csv"))
basis   <- fread(file.path(.diag_dir, "DD_soap_spatial_basis_check.csv"))

AIC_REPORTED <- 6100.53
AIC_DENSE    <- 6076.34
D_GRID       <- round(AIC_DENSE - AIC_REPORTED, 2)     # -24.19

# ---- Panel A: the spatial defect on its own (no covariate in the model) ----
basis[, lab := factor(variant, levels = variant[order(AIC)])]
basis[, dAIC := round(AIC - AIC_REPORTED, 2)]
basis[, binding := grepl("denser", variant)]

pA <- ggplot(basis, aes(dAIC, lab)) +
  geom_vline(xintercept = 0, linewidth = .4, colour = "grey35") +
  geom_segment(aes(x = 0, xend = dAIC, yend = lab, colour = binding),
               linewidth = 1.4) +
  geom_point(aes(colour = binding), size = 3) +
  # label outside the bar on whichever side the bar is not
  geom_text(aes(label = sprintf("%+.2f", dAIC),
                hjust = ifelse(dAIC < 0, 1.25, -0.3)), size = 3.1) +
  scale_colour_manual(values = c(`FALSE` = "grey45", `TRUE` = "#1f6f8b"),
                      guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.16, 0.16))) +
  labs(title = "A. Defect 1 — the spatial term, with NO covariate in the model",
       subtitle = paste("Only the interior KNOT GRID binds. Raising the",
                        "boundary-film k does nothing.\nBaseline 0 = the",
                        "reported model dd.dsm.soap.season.year (AIC 6100.53)."),
       x = expression(Delta*"AIC vs the reported model"), y = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 8, colour = "grey30"),
        plot.title = element_text(face = "bold", size = 10))

# ---- Panel B: both defects at once, per covariate ----
d <- copy(grid2x2)
d[, grid_lab := factor(knots, levels = c(41, 92),
                       labels = c("41 knots (stored)", "92 knots (denser)"))]
# order facets by the best cell each covariate reaches
.ord <- d[, .(best = min(dAIC_vs_reported)), by = covariate][order(best), covariate]
d[, covariate := factor(covariate, levels = .ord)]
.best <- d[which.min(dAIC_vs_reported)]

pB <- ggplot(d, aes(factor(k), dAIC_vs_reported,
                    group = grid_lab, colour = grid_lab, shape = grid_lab)) +
  geom_hline(yintercept = 0, linewidth = .5, colour = "grey20") +
  geom_hline(yintercept = D_GRID, linetype = "22", linewidth = .45,
             colour = "#1f6f8b") +
  geom_line(linewidth = .9) +
  geom_point(size = 2.6, fill = "white", stroke = .8) +
  geom_text(aes(label = sprintf("%.1f", dAIC_vs_reported)),
            size = 2.5, vjust = -0.9, show.legend = FALSE) +
  facet_wrap(~ covariate, nrow = 2) +
  scale_colour_manual(values = c("41 knots (stored)"  = "grey40",
                                 "92 knots (denser)" = "#1f6f8b"), name = NULL) +
  scale_shape_manual(values = c(21, 19), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.16))) +
  labs(title = "B. Both defects, per environmental covariate",
       subtitle = paste0(
         "Vertical gap between the lines = fixing the knot grid.  ",
         "Slope = raising the covariate's k.\n",
         "Solid line at 0 = reported model.  Dashed line at ", D_GRID,
         " = base with the knot grid fixed and NO covariate."),
       x = "covariate basis dimension k",
       y = expression(Delta*"AIC vs the reported model")) +
  theme_bw(base_size = 10) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8, colour = "grey30"),
        plot.title = element_text(face = "bold", size = 10))

# ---- Panel C: one sequential path, to give the total a decomposition ----
# Order matters when the two defects interact, so this is ONE path of several;
# the caption says so. Path: fix the grid, then add sst, then free its k.
wf <- data.table(
  step = c("reported model\ns(x,y,so)+season+s(Ano)",
           "+ knot grid 41->92",
           "+ s(sst, k=10)",
           "+ free k to 20"),
  delta = c(0, D_GRID,
            d[covariate == "sst" & knots == 92 & k == 10, dAIC_vs_reported] - D_GRID,
            d[covariate == "sst" & knots == 92 & k == 20, dAIC_vs_reported] -
              d[covariate == "sst" & knots == 92 & k == 10, dAIC_vs_reported])
)
wf[, step := factor(step, levels = step)]
wf[, cum := cumsum(delta)]
wf[, start := shift(cum, fill = 0)]

pC <- ggplot(wf, aes(step)) +
  geom_hline(yintercept = 0, linewidth = .4, colour = "grey20") +
  geom_rect(aes(xmin = as.numeric(step) - .35, xmax = as.numeric(step) + .35,
                ymin = start, ymax = cum,
                fill = ifelse(delta <= 0, "improves", "worsens")),
            colour = "grey25", linewidth = .3) +
  geom_text(aes(y = (start + cum) / 2,
                label = ifelse(delta == 0, "0", sprintf("%+.2f", delta))),
            size = 3) +
  geom_text(aes(y = cum, label = sprintf("%.0f", AIC_REPORTED + cum)),
            vjust = 1.6, size = 2.7, colour = "grey25") +
  scale_fill_manual(values = c(improves = "#1f6f8b", worsens = "#c0392b"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.06))) +
  labs(title = "C. One route from the reported model to the best fit found",
       subtitle = paste("Cumulative", "ΔAIC; the small grey numbers are the",
                        "resulting AIC. The two defects interact, so this is one\n",
                        "ordering of several -- the total (-59.6) is what is",
                        "robust, not the split between the middle two bars."),
       x = NULL, y = expression("cumulative "*Delta*"AIC")) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 7.5),
        plot.subtitle = element_text(size = 8, colour = "grey30"),
        plot.title = element_text(face = "bold", size = 10))

# ---- assemble ----
.out <- file.path(.diag_dir, "DD_two_defects.png")
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p <- (pA / pB / pC) + plot_layout(heights = c(0.8, 2.1, 1.1))
  ggsave(.out, p, width = 10, height = 14, dpi = 150)
  cat("wrote", .out, "\n")
} else {
  ggsave(sub("[.]png$", "_A.png", .out), pA, width = 9, height = 3.2, dpi = 150)
  ggsave(sub("[.]png$", "_B.png", .out), pB, width = 10, height = 6, dpi = 150)
  ggsave(sub("[.]png$", "_C.png", .out), pC, width = 8, height = 4.5, dpi = 150)
  cat("patchwork not installed - wrote three separate PNGs\n")
}

cat("\n=== the numbers behind the figure ===\n")
print(dcast(d, covariate ~ knots + k, value.var = "dAIC_vs_reported"))
print(wf[, .(step = gsub("\n", " ", step), delta, cum,
             AIC = round(AIC_REPORTED + cum, 2))])
