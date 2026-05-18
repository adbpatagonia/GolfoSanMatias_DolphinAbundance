practical advice from Len on how to deal with detection function having group size as covariate
you can do it 2 ways:
  1. complex bayesian model - he is doing for a paper that they will publish. This woud be the "right way" of doing it
  2. treat DD as effectively 2 clusters: 1 for small group sizes, a second one for larger gorup sizes. Find the spot where
     group size will not impact the detection function very much, try 75th quantile? Fit DSMs to each of the clusters,
     and then sum them. This is the "good enough" way


DD detection funciton
Beaufort
group 0-1, 2, 3+



  ---
  altenative models


dsm.xy.depth.1  <-  dsm(count ~ s(x,y, bs = "ts") +
                          -1 + season +
                          s(depth) +
                          s(year_fac, bs = "re"),
                        ddf.obj =  df.lo,
                        segment.data = segdata,
                        observation.data = obsdata_lo_mod,
                        family = Tweedie(p = 1.31),
                        method="REML")

dsm.xy.depth.2  <-  dsm(count ~ te(x,y, bs = "ts") +
                          -1 + season +
                          s(depth) +
                          s(year_fac, bs = "re"),
                        ddf.obj =  df.lo,
                        segment.data = segdata,
                        observation.data = obsdata_lo_mod,
                        family = Tweedie(p = 1.31),
                        method="REML")

dsm.xy.depth.3  <-  dsm(count ~ s(x,y, bs = "cs") +
                          -1 + season +
                          s(depth) +
                          s(year_fac, bs = "re"),
                        ddf.obj =  df.lo,
                        segment.data = segdata,
                        observation.data = obsdata_lo_mod,
                        family = Tweedie(p = 1.31),
                        method="REML")

dsm.xy.depth.4  <-  dsm(count ~ te(x,y, bs = "cs") +
                          -1 + season +
                          s(depth) +
                          s(year_fac, bs = "re"),
                        ddf.obj =  df.lo,
                        segment.data = segdata,
                        observation.data = obsdata_lo_mod,
                        family = Tweedie(p = 1.31),
                        method="REML")

dsm.xy.depth.5  <-  dsm(count ~ te(x,y, bs = "cs", m = 1) +
                          -1 + season +
                          s(depth) +
                          s(year_fac, bs = "re"),
                        ddf.obj =  df.lo,
                        segment.data = segdata,
                        observation.data = obsdata_lo_mod,
                        family = Tweedie(p = 1.31),
                        method="REML")


