# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the Detection function file for common dolphins


# wrangle data ----
detfun_dat_dd <- copy(distdata_dd)
## eliminar Beaufort > 4 -----
detfun_dat_dd <- detfun_dat_dd[beaufort < 5]

## truncate ? -----


df.hr <- ds(detfun_dat_dd,
               max(detfun_dat_dd$distance),
               key = "hr",
               adjustment = NULL)

df.hn <- ds(detfun_dat_dd,
               max(detfun_dat_dd$distance),
               key = "hn",
               adjustment = NULL)
