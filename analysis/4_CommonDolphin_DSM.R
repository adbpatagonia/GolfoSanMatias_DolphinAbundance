# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the DSM file for common dolphins


# prepare data -----
obsdata_dd_mod <- copy(obsdata_dd)
obsdata_dd_mod <-   obsdata_dd_mod[distance <= trunc.dist.dd]


setDT(segdata)
setDT(detfun_dat_dd)
setkey(segdata, Sample.Label)
setkey(detfun_dat_dd, Sample.Label)

dd=merge(segdata,
      unique(detfun_dat_dd[,.(Sample.Label, size_sc, size, beaufort_fct)]),
      all.x = TRUE)


segdata

# Simple model ----

dsm.xy  <-  dsm(count~s(x,y),
                ddf.obj =  df.dd,
                segment.data = segdata,
                observation.data = obsdata_dd_mod,
                method="REML")

summary(dsm.xy)


dd.gam <- dsm(count ~ s(x ~ y),
    ddf.obj = dd.df.hr.trun.cp,
    segment.data = segdata,
    observation.data = obsdata_dd,
    method = "REML")

# try including
# depth
# slope

# hypothesis testing
# effect of season
# effect of year
# interaction?
