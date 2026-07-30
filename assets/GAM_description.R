DESCRIPTION OF GAM USED IN DSMS

Here's a description of the four approaches, suitable to adapt into a methods section. All four are variants of the same underlying density surface model (Hedley & Buckland 2004; Miller et al. 2013): count per segment modelled as p_j * A_j * exp(β0 + Σ_k f_k(z_jk)), where p_j is the average detection probability and A_j the segment area (entering as an offset), and the f_k are smooth functions of covariates. They differ only in how the SPATIAL term and any TIME-VARYING behaviour are represented, and each makes a different trade-off between flexibility, boundary/edge-effect control, and statistical stability.

0. Thin-plate spatial smooth — the baseline / shared-surface family
Formula (progressively extended):
  s(x,y)
  s(x,y) + season
  s(x,y) + season + s(Ano)                     [primary model]
  s(x,y) + season + s(Ano) + s(env)             (each of 6 environmental covariates)
  s(x,y) + s(Ano)
  s(x,y) + season + s(env)                      (each of 6 environmental covariates)
  s(x,y) + s(Ano) + s(env)                      (each of 6 environmental covariates)

This is the baseline family that every other approach below extends or modifies. The spatial term s(x,y) is an isotropic thin-plate regression spline (Wood 2003) fitted to the WHOLE survey period at once: a single spatial surface, shared across all years and seasons. Season enters as a parametric factor (a categorical shift in the mean level), and year enters — where included — as a continuous thin-plate smooth s(Ano), allowing a flexible but still spatially-invariant trend in overall abundance through time (as distinct from the year_fac grouping factor used in the fs/by-year approaches below). Environmental covariates enter as additional additive smooth terms s(env), each layered on top of whatever season/year structure the candidate model already has, so their explanatory value can be assessed net of the spatial/temporal terms.

Because the spatial surface itself never varies by year or season, this family cannot represent genuine year-to-year or season-to-season REDISTRIBUTION of the population — only ADDITIVE shifts in overall level. It is the most stable and fastest to fit (all data support one shared surface, so there is no risk of a data-poor year or season leaving part of the surface unidentified), and every observation, regardless of year or season, legitimately informs the surface shown for any candidate in this family. This is the natural reference point against which the year-varying and boundary-respecting alternatives below are compared by AIC and deviance explained.

1. Factor-smooth ("fs") — year as a random effect, shrunk
Formula: count ~ s(x, y, year_fac, bs = "fs") + season

The spatial surface is allowed to differ by year through a factor-smooth basis, which treats year_fac as a grouping factor analogous to a random effect in a mixed model: every year's spatial deviation is penalized toward a common, shared pattern using a single smoothing parameter across all years. Per-year intercepts are absorbed automatically within the basis (no separate parametric year_fac term is required).

Because information is pooled ("borrowed") across years, this approach is comparatively stable and fast to fit, and years with limited survey coverage are shrunk toward the overall spatial pattern rather than being estimated in isolation. The cost is a constraint that all years share the same degree of spatial "wiggliness," and because year enters as a shrinkage term rather than a fixed effect, extracting an interpretable year-level effect requires a dedicated post-hoc extraction (area-averaging the fitted smooth per year, with a covariance-based confidence interval — implemented here as year_partial_effect()) rather than reading a model coefficient directly.

2. By-year smooth ("by=year_fac") — year as a fixed effect, unshrunk
Formula: count ~ s(x, y, by = year_fac) + year_fac + season

Here each year is fitted an entirely independent 2-D spatial surface, each with its own smoothing parameter, estimated without any pooling across years. Because a by-factor smooth is centered within each level, year_fac must also enter as a parametric term to carry the per-year mean level.

This is the most flexible option — it makes no assumption that years share a common spatial structure — but that flexibility comes at a real cost: with many years, the number of spatial parameters and per-year smoothing parameters grows substantially, and years with sparse survey coverage produce poorly identified, unstable surfaces that are prone to unrealistic extrapolation at the survey boundary ("edge effects") and to optimization difficulty (in practice, some of these models required a shrinkage penalty, bs = "ts", and a faster/more robust fitting engine, bam/fREML, to converge reliably).

3. Soap-film smoother — a single, boundary-respecting spatial surface
Formula: count ~ s(x, y, bs = "so") + season + s(Ano)

Rather than letting the spatial distribution vary by year, this model fixes one shared spatial surface for the whole study period (like the thin-plate baseline in item 0), but constructs it using a soap-film smoother (Wood, Bravington & Hedley, 2008), which fits the surface as a flexible membrane stretched across a specified two-dimensional domain — here, the survey/coastline boundary — and explicitly prevents smoothing across that boundary (e.g., through headlands or onto land), unlike a conventional thin-plate spline. Any temporal trend in overall abundance is captured separately through a smooth term on year, s(Ano), so the model allows the level of density to change over time while holding the spatial pattern fixed.

This directly targets boundary-driven artifacts that a standard thin-plate or factor-smooth surface can produce when extrapolating near a complex or sparsely-sampled coastline; predictions automatically return NA for locations outside the fitted domain rather than extrapolating uncontrolled values. The trade-off is that it cannot represent genuine year-to-year redistribution of the population (same limitation as item 0), and it requires more careful, and occasionally fiddly, setup: a boundary polygon buffered outward so that every observation lies strictly inside it, and a grid of interior knots placed away from the boundary edge.

Summary framing for the report: the thin-plate baseline (0) and soap-film (3) models hold the spatial pattern fixed for the whole study period and represent temporal change only as an overall level shift via s(Ano); the fs (1) and by-year (2) models instead let the spatial density surface itself change across years, differing in whether that variation is pooled (shrunk, fs) or estimated independently per year (unshrunk, by). Soap and thin-plate differ only in whether the spatial surface is constrained to respect the survey/coastline boundary. These are complementary rather than strictly nested strategies — a soap-film basis cannot currently be combined with a factor-smooth or by-year term, so all four are presented as alternative candidate models compared via AIC/deviance explained rather than one being a generalization of the others.

References:
Hedley, S.L., & Buckland, S.T. (2004). Spatial models for line transect sampling. Journal of Agricultural, Biological, and Environmental Statistics, 9, 181–199.
Miller, D.L., Burt, M.L., Rexstad, E.A., & Thomas, L. (2013). Spatial models for distance sampling data: recent developments and future directions. Methods in Ecology and Evolution, 4(11), 1001–1010.
Wood, S.N. (2003). Thin plate regression splines. Journal of the Royal Statistical Society: Series B, 65(1), 95–114.
Wood, S.N., Bravington, M.V., & Hedley, S.L. (2008). Soap film smoothing. Journal of the Royal Statistical Society: Series B, 70(5), 931–955.
