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


detfun_dat_dd$beaufort_fct <- as.factor(detfun_dat_dd$beaufort)

# basic detection functions -----
dd.df.hr <- ds(detfun_dat_dd,
               max(detfun_dat_dd$distance),
               key = "hr",
               adjustment = NULL)

dd.df.hn <- ds(detfun_dat_dd,
               max(detfun_dat_dd$distance),
               key = "hn",
               adjustment = NULL)

# truncation ? -----
# Buckland et al. (2001, p. 16) suggest
# truncation where probability of detection is estimated to be around 0.15

# consider reducing the truncation distance, w, if more than 5% of the Pa(zi)
# are <0.2, or if any are less than 0.1


# detection probability for each observed distance
detfun_dat_dd$p_i <- mrds::detfct(distance = detfun_dat_dd$distance, dd.df.hr$ddf$ds$aux$ddfobj)

# many observations with p_i < 0.1
detfun_dat_dd[p_i < 0.1]
# 17% of observations with P_i <=0.2
detfun_dat_dd$bin <- cut(detfun_dat_dd$p_i, breaks = seq(0, 1, .2))
detfun_dat_dd %>%
  group_by(bin) %>%
  tally() %>%
  mutate(prop = n/nrow(detfun_dat_dd))

# we need to truncate
# find distance where probability of detection is estimated to be around 0.15

# the distance is ~ 300m
detfun_dat_dd[data.table::between(x = p_i, lower = 0.14, upper = 0.16)]

detfun_dat_dd %>%
  distinct(distance, p_i) %>%
  ggplot(.) +
  geom_line(aes(x = distance, y = p_i)) +
  geom_hline(yintercept = 0.15, col = "red") +
  geom_vline(xintercept = 300, col = "red")

# truncation at 300 m eliminates ~13% of the data
nrow(detfun_dat_dd[distance<=300])/nrow(detfun_dat_dd)

trunc.dist_dd <- 300

## truncated df ----
dd.df.hr.trun <- ds(data = detfun_dat_dd,
                    truncation = trunc.dist_dd,
                    key = "hr",
                    adjustment = NULL)


# rounding in data -----
# See p. 68 in Bucklnd et al 2015 - "The Montrave Case Study: Line Transect Sampling"
# there was evident rounding in distances that increased by 5 meters
# solution: use cutpints that avoid favoured distances
# favoured distances: 5, 15, 20, 25, 30, 35, 45, 50
# cutpoints: 0, 12.5, 22.5, 32.5, 42.5, 52.5, 62.5, 77.5, 95

# Here, we see favoured distances at 50, 100, 150, 200, 250, 300
plot(dd.df.hr.trun)

cutpoints_dd <- c(0, 25, 75, 125, 175, 225, 275, 300)
table(cut(detfun_dat_dd$distance, cutpoints_dd, include.lowest = TRUE))


## fit dfs ----
dd.df.hr.trun.cp <- ds(detfun_dat_dd,
                       truncation = trunc.dist_dd,
                       cutpoints = cutpoints_dd,
                       key = "hr",
                       adjustment = NULL)

plot(dd.df.hr.trun.cp)

dd.df.hn.trun.cp <- ds(detfun_dat_dd,
                       truncation = trunc.dist_dd,
                       cutpoints = cutpoints_dd,
                       key = "hn",
                       adjustment = NULL)

# adjustment terms ----
## cosine ----
### hr ----
dd.df.hr.trun.cp.cos <- ds(detfun_dat_dd,
                           truncation = trunc.dist_dd,
                           cutpoints = cutpoints_dd,
                           key = "hr",
                           adjustment = "cos")


### hn ----
dd.df.hn.trun.cp.cos <- ds(detfun_dat_dd,
                           truncation = trunc.dist_dd,
                           cutpoints = cutpoints_dd,
                           key = "hn",
                           adjustment = "cos")


## herm ----
### hr ----
dd.df.hr.trun.cp.herm <- ds(detfun_dat_dd,
                            truncation = trunc.dist_dd,
                            cutpoints = cutpoints_dd,
                            key = "hr",
                            adjustment = "herm")


### hn ----
dd.df.hn.trun.cp.herm <- ds(detfun_dat_dd,
                            truncation = trunc.dist_dd,
                            cutpoints = cutpoints_dd,
                            key = "hn",
                            adjustment = "herm")


## poly ----
### hr ----
dd.df.hr.trun.cp.poly <- ds(detfun_dat_dd,
                            truncation = trunc.dist_dd,
                            cutpoints = cutpoints_dd,
                            key = "hr",
                            adjustment = "poly")

### hn ----
dd.df.hn.trun.cp.poly <- ds(detfun_dat_dd,
                            truncation = trunc.dist_dd,
                            cutpoints = cutpoints_dd,
                            key = "hn",
                            adjustment = "poly")


## Model selection ------
# Several models cannot be distinguished from each other -- retain the simplest
# hazard rate
# no adjustment terms
AIC(dd.df.hr.trun.cp,
    dd.df.hn.trun.cp,
    dd.df.hr.trun.cp.cos,
    dd.df.hn.trun.cp.cos,
    dd.df.hr.trun.cp.poly,
    dd.df.hn.trun.cp.poly,
    dd.df.hr.trun.cp.herm,
    dd.df.hn.trun.cp.herm
) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(deltaAIC) %>%
  kable()

Distance::gof_ds(dd.df.hr.trun.cp)


plot(dd.df.hn.trun.cp.cos)
plot(dd.df.hr.trun.cp)

# Covariates ----
## ship ----
dd.df.hr.trun.cp.ship <- ds(detfun_dat_dd,
                            truncation = trunc.dist_dd,
                            cutpoints = cutpoints_dd,
                            key = "hr",
                            adjustment = NULL,
                            formula = ~ship)
## size ----
# standardize size
detfun_dat_dd[, size_sc := scale(size, center = TRUE, scale = TRUE)]
dd.df.hr.trun.cp.size <- ds(detfun_dat_dd,
                            truncation = trunc.dist_dd,
                            cutpoints = cutpoints_dd,
                            key = "hr",
                            adjustment = NULL,
                            formula = ~size_sc,
                            initial_values = dd.df.hr.trun.cp$ddf)


## beaufort ----
dd.df.hr.trun.cp.beauf <- ds(detfun_dat_dd,
                             truncation = trunc.dist_dd,
                             cutpoints = cutpoints_dd,
                             key = "hr",
                             adjustment = NULL,
                             formula = ~beaufort_fct)

## size + beaufort ----
dd.df.hr.trun.cp.sizebeauf <- ds(detfun_dat_dd,
                            truncation = trunc.dist_dd,
                            cutpoints = cutpoints_dd,
                            key = "hr",
                            adjustment = NULL,
                            formula = ~size_sc + beaufort_fct,
                            initial_values = dd.df.hr.trun.cp$ddf)

## size + ship ----
dd.df.hr.trun.cp.sizeship <- ds(detfun_dat_dd,
                                 truncation = trunc.dist_dd,
                                 cutpoints = cutpoints_dd,
                                 key = "hr",
                                 adjustment = NULL,
                                 formula = ~size_sc + ship,
                                 initial_values = dd.df.hr.trun.cp$ddf)

## beaufort + ship ----
dd.df.hr.trun.cp.shipbeauf <- ds(detfun_dat_dd,
                                truncation = trunc.dist_dd,
                                cutpoints = cutpoints_dd,
                                key = "hr",
                                adjustment = NULL,
                                formula = ~beaufort_fct + ship)
## Model selection ------
AIC(dd.df.hr.trun.cp,
    dd.df.hr.trun.cp.beauf,
    dd.df.hr.trun.cp.size,
    dd.df.hr.trun.cp.ship,
    dd.df.hr.trun.cp.sizebeauf,
    dd.df.hr.trun.cp.sizeship,
    dd.df.hr.trun.cp.shipbeauf
) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(deltaAIC) %>%
  kable()

## plot dfs ----

### size ----
plot(dd.df.hr.trun.cp.size$ddf,
     main = "Common dolphin",
     showpoints = TRUE)

size_vals <- quantile(detfun_dat_dd[distance <= trunc.dist_dd, .(size_sc)],
                      probs = c(0.1, 0.5, 0.9),
                      na.rm = TRUE)
size_vals_or <- quantile(detfun_dat_dd[distance <= trunc.dist_dd, .(size)],
                         probs = c(0.1, 0.5, 0.9),
                         na.rm = TRUE)


cols <- c("red", "darkgreen", "blue")

for(i in seq_along(size_vals)) {
  add_df_covar_line(
    dd.df.hr.trun.cp.size,
    data = data.frame(size_sc = size_vals[i]),
    col = cols[i],
    lwd = 2,
    lty = 1
  )
}

legend("bottomleft",
       legend = paste0("Size = ", round(size_vals_or, 1)),
       col = cols,
       lwd = 2)

### beaufort ----
plot(dd.df.hr.trun.cp.beauf, main="Common dolphin", showpoints=FALSE)
add_df_covar_line(dd.df.hr.trun.cp.beauf, data = data.frame(beaufort_fct=0), col='red', lwd=2, lty=1)
add_df_covar_line(dd.df.hr.trun.cp.beauf, data = data.frame(beaufort_fct=1), col='blue', lwd=2, lty=1)
add_df_covar_line(dd.df.hr.trun.cp.beauf, data = data.frame(beaufort_fct=2), col= "darkgreen", lwd=2, lty=1)
add_df_covar_line(dd.df.hr.trun.cp.beauf, data = data.frame(beaufort_fct=3), col='purple', lwd=2, lty=1)
add_df_covar_line(dd.df.hr.trun.cp.beauf, data = data.frame(beaufort_fct=4), col='orange', lwd=2, lty=1)
legend("topright", legend=c("Beaufort 0", "Beaufort 1", "Beaufort 2", "Beaufort 3", "Beaufort 4"),
       col=c("red", "blue", "darkgreen", "purple", "orange"), lwd=2)

# uneven sample size - most samples taken at beaufort 1 to 3
# patterns for those categories make sense, and the differences among categories are noticeable
# keep beaufort as covariate
detfun_dat_dd[distance <= trunc.dist_dd] %>%
  group_by(beaufort_fct) %>%
  tally()


### ship ----
plot(dd.df.hr.trun.cp.ship, main="Common dolphin", showpoints=FALSE)
add_df_covar_line(dd.df.hr.trun.cp.ship, data = data.frame(ship=1), col='red', lwd=2, lty=1)
add_df_covar_line(dd.df.hr.trun.cp.ship, data = data.frame(ship=2), col= "darkgreen", lwd=2, lty=1)
legend("topright", legend=c("Ship 1", "Ship 2"),
       col=c("red",  "darkgreen"), lwd=2)


ggplot(distdata_dd,
       aes(x = distance, y = size)) +
  geom_point( alpha = 0.5,
              position = position_jitter (width = 0.4)) +

  xlim(0, 300) +
  ylim(0, 20) +
  geom_smooth(method = "lm")

ggplot(distdata_dd[distance <= trunc.dist_dd]) +
  geom_histogram(aes(x = size), col = 'black', fill = 'gray90', breaks = seq(0, max(detfun_dat_dd$size), 10))




detfun_dat_dd$sizebin <- cut(detfun_dat_dd$size,
                             breaks = seq(0, max(detfun_dat_dd$size), 10))

detfun_dat_dd[distance <= trunc.dist_dd] %>%
  group_by(sizebin) %>%
  tally() %>%
  mutate(prop = n/sum(n))


# Final Detection Function ----
# Truncation at 300 m - discard ~13% of data
# cutpoints:  cutpoints_dd: c(0, 25, 75, 125, 175, 225, 275, 300)
# Hazard Rate
# No adjustment
# Include group size as covariate

## dd: delphinus delphi
## df: detection function
## hr: hazard rate
## trun: trnacated distance
## cp: use cutpoints to deal with grouped data
## size: group size as covariate to the detection function
## beauf: beaufort sea state as covariate

df.dd <- dd.df.hr.trun.cp.sizebeauf

qqdat <- qqplot.ddf(df.dd$ddf, plot = FALSE)
plot(qqdat$cdf)
qqplot.ddf(df.dd$ddf, plot = TRUE)



