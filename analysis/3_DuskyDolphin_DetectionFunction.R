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
detfun_dat_lo <- copy(distdata_lo)
## eliminar Beaufort > 3 -----
detfun_dat_lo <- detfun_dat_lo[beaufort < 4]


detfun_dat_lo$beaufort_fct <- as.factor(detfun_dat_lo$beaufort)

# basic detection functions -----
lo.df.hr <- ds(detfun_dat_lo,
               max(detfun_dat_lo$distance),
               key = "hr",
               adjustment = NULL)

lo.df.hn <- ds(detfun_dat_lo,
               max(detfun_dat_lo$distance),
               key = "hn",
               adjustment = NULL)

# truncation ? -----
# Buckland et al. (2001, p. 16) suggest
# truncation where probability of detection is estimated to be around 0.15

# consider reducing the truncation distance, w, if more than 5% of the Pa(zi)
# are <0.2, or if any are less than 0.1


# detection probability for each observed distance
detfun_dat_lo$p_i <- mrds::detfct(distance = detfun_dat_lo$distance, lo.df.hn$ddf$ds$aux$ddfobj)

# 10 observations with p_i < 0.1
detfun_dat_lo[p_i < 0.1]
# >8% of observations with P_i <=0.2
detfun_dat_lo$bin <- cut(detfun_dat_lo$p_i, breaks = seq(0, 1, .2))
detfun_dat_lo %>%
  group_by(bin) %>%
  tally() %>%
  mutate(prop = n/nrow(detfun_dat_lo)) %>%
  kable()


ggplot(detfun_dat_lo, aes(x = distance, y = size)) +
  geom_point() +
  geom_smooth(method = "lm")

# we need to truncate
# find distance where probability of detection is estimated to be around 0.15

# the distance is probably ~ 450m
detfun_dat_lo[data.table::between(x = p_i, lower = 0.09, upper = 0.25)]

detfun_dat_lo %>%
  distinct(distance, p_i) %>%
  ggplot(.) +
  geom_line(aes(x = distance, y = p_i)) +
  geom_hline(yintercept = 0.15, col = "red") +
  scale_x_continuous(breaks = seq(0, 1000, 50)) +
  geom_vline(xintercept = 450, col = "red")

nrow(detfun_dat_lo[distance<=450])/nrow(detfun_dat_lo)

trunc.dist_lo <- 450

## truncated df ----
lo.df.hn.trun <- ds(data = detfun_dat_lo,
                    truncation = trunc.dist_lo,
                    key = "hn",
                    adjustment = NULL)

lo.df.hr.trun <- ds(data = detfun_dat_lo,
                    truncation = trunc.dist_lo,
                    key = "hr",
                    adjustment = NULL)

# rounding in data -----
# See p. 68 in Bucklnd et al 2015 - "The Montrave Case Study: Line Transect Sampling"
# there was evident rounding in distances that increased by 5 meters
# solution: use cutpints that avoid favoured distances
# favoured distances: 5, 15, 20, 25, 30, 35, 45, 50
# cutpoints: 0, 12.5, 22.5, 32.5, 42.5, 52.5, 62.5, 77.5, 95

# Here, we see favoured distances at 0, 100,  200,  300
plot(lo.df.hn.trun)


cutpoints_lo <- c(0,  75, 125, 175, 225, 275, 325, 375, 425)
# tried different cutpoints here, and this is the one that seems to make most sense
cutpoints_lo <- c(0, 50, 150, 250, 350, 450)

table(cut(detfun_dat_lo$distance, cutpoints_lo, include.lowest = TRUE))

## fit dfs ----
lo.df.hr.trun.cp <- ds(detfun_dat_lo,
                       truncation = trunc.dist_lo,
                       cutpoints = cutpoints_lo,
                       key = "hr",
                       adjustment = NULL)

plot(lo.df.hr.trun.cp)

lo.df.hn.trun.cp <- ds(detfun_dat_lo,
                       truncation = trunc.dist_lo,
                       cutpoints = cutpoints_lo,
                       key = "hn",
                       adjustment = NULL)

plot(lo.df.hn.trun.cp)

# adjustment terms ----

## herm ----
### hn ----
lo.df.hn.trun.cp.herm <- ds(detfun_dat_lo,
                            truncation = trunc.dist_lo,
                            cutpoints = cutpoints_lo,
                            key = "hn",
                            adjustment = "herm")


## poly ----
### hr ----
# Error in model fitting, returning: hazard-rate key function with cosine(2) adjustments
# Number of parameters to estimate exceed number of distance bins minus 1
lo.df.hr.trun.cp.poly <- ds(detfun_dat_lo,
                            truncation = trunc.dist_lo,
                            cutpoints = cutpoints_lo,
                            key = "hr",
                            adjustment = "poly")



## Model selection ------
# Los modelos half-normal, con y sin termino de ajuste, no pueden distinguirse.
# Retener el modelo más sencillo.
# half normal
# no adjustment terms
AIC(lo.df.hr.trun.cp,
    lo.df.hn.trun.cp,
    lo.df.hr.trun.cp.poly,
    lo.df.hn.trun.cp.herm
) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(deltaAIC) %>%
  kable()

Distance::gof_ds(lo.df.hn.trun.cp)



plot(lo.df.hn.trun.cp)
plot(lo.df.hn.trun.cp.cos)

# Covariates ----
## ship ----
lo.df.hn.trun.cp.ship <- ds(detfun_dat_lo,
                            truncation = trunc.dist_lo,
                            cutpoints = cutpoints_lo,
                            key = "hn",
                            adjustment = NULL,
                            formula = ~ship)
## size ----
# standardize size
detfun_dat_lo[, size_sc := scale(size, center = TRUE, scale = TRUE)]
lo.df.hn.trun.cp.size <- ds(detfun_dat_lo,
                            truncation = trunc.dist_lo,
                            cutpoints = cutpoints_lo,
                            key = "hn",
                            adjustment = NULL,
                            formula = ~size_sc,
                            initial_values = lo.df.hn.trun.cp$ddf)


## beaufort ----
lo.df.hn.trun.cp.beauf <- ds(detfun_dat_lo,
                             truncation = trunc.dist_lo,
                             cutpoints = cutpoints_lo,
                             key = "hn",
                             adjustment = NULL,
                             formula = ~beaufort_fct)

## size + beaufort ----
lo.df.hn.trun.cp.sizebeauf <- ds(detfun_dat_lo,
                                 truncation = trunc.dist_lo,
                                 cutpoints = cutpoints_lo,
                                 key = "hn",
                                 adjustment = NULL,
                                 formula = ~size_sc + beaufort_fct,
                                 initial_values = lo.df.hn.trun.cp$ddf)

## size + ship ----
lo.df.hn.trun.cp.sizeship <- ds(detfun_dat_lo,
                                truncation = trunc.dist_lo,
                                cutpoints = cutpoints_lo,
                                key = "hn",
                                adjustment = NULL,
                                formula = ~size_sc + ship,
                                initial_values = lo.df.hn.trun.cp$ddf)

## beaufort + ship ----
lo.df.hn.trun.cp.shipbeauf <- ds(detfun_dat_lo,
                                 truncation = trunc.dist_lo,
                                 cutpoints = cutpoints_lo,
                                 key = "hn",
                                 adjustment = NULL,
                                 formula = ~beaufort_fct + ship)
## Model selection ------
# Several models have very similar AIC
# given the small effects of each of the covariates (see below), use the simplest model, i.e. no covariate
AIC(lo.df.hn.trun.cp,
    lo.df.hn.trun.cp.beauf,
    # lo.df.hn.trun.cp.size,
    lo.df.hn.trun.cp.ship,
    # lo.df.hn.trun.cp.sizebeauf,
    # lo.df.hn.trun.cp.sizeship,
    lo.df.hn.trun.cp.shipbeauf
) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(deltaAIC) %>%
  kable()

## plot dfs ----

### size ----
plot(lo.df.hn.trun.cp.size$ddf,
     main = "Common dolphin",
     showpoints = TRUE)

size_vals <- quantile(detfun_dat_lo[distance <= trunc.dist_lo, .(size_sc)],
                      probs = c(0.1, 0.5, 0.9),
                      na.rm = TRUE)
size_vals_or <- quantile(detfun_dat_lo[distance <= trunc.dist_lo, .(size)],
                         probs = c(0.1, 0.5, 0.9),
                         na.rm = TRUE)


cols <- c("red", "darkgreen", "blue")

for(i in seq_along(size_vals)) {
  add_df_covar_line(
    lo.df.hn.trun.cp.size,
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

# all groups are quite small
ggplot(distdata_lo[distance <= trunc.dist_lo]) +
  geom_histogram(aes(x = size), col = 'black', fill = 'gray90', breaks = seq(0, max(detfun_dat_lo$size), 1))

# do not consider as a covariate


### beaufort ----
# this pattern looks really odd - beaufort = 0 should have the best detection function, but it has the worst
plot(lo.df.hn.trun.cp.beauf, main="Common dolphin", showpoints=FALSE)
add_df_covar_line(lo.df.hn.trun.cp.beauf, data = data.frame(beaufort_fct=0), col='red', lwd=2, lty=1)
add_df_covar_line(lo.df.hn.trun.cp.beauf, data = data.frame(beaufort_fct=1), col='blue', lwd=2, lty=1)
add_df_covar_line(lo.df.hn.trun.cp.beauf, data = data.frame(beaufort_fct=2), col= "darkgreen", lwd=2, lty=1)
add_df_covar_line(lo.df.hn.trun.cp.beauf, data = data.frame(beaufort_fct=3), col='purple', lwd=2, lty=1)
# add_df_covar_line(lo.df.hn.trun.cp.beauf, data = data.frame(beaufort_fct=4), col='orange', lwd=2, lty=1)
legend("topright", legend=c("Beaufort 0", "Beaufort 1", "Beaufort 2", "Beaufort 3"),
       col=c("red", "blue", "darkgreen", "purple"), lwd=2)

# uneven sample size - most samples taken at beaufort 1 and 2
# patterns for those make sense, i.e. 1 is better than 2, but the differences are minimal
# drop beaufort as covariate
detfun_dat_lo[distance <= trunc.dist_lo] %>%
  group_by(beaufort_fct) %>%
  tally()

### ship ----
# no real differences between ships
# drop as covariate
plot(lo.df.hn.trun.cp.ship, main="Common dolphin", showpoints=FALSE)
add_df_covar_line(lo.df.hn.trun.cp.ship, data = data.frame(ship=1), col='red', lwd=2, lty=1)
add_df_covar_line(lo.df.hn.trun.cp.ship, data = data.frame(ship=2), col= "darkgreen", lwd=2, lty=1)
legend("topright", legend=c("Ship 1", "Ship 2"),
       col=c("red",  "darkgreen"), lwd=2)



ggplot(distdata_lo,
       aes(x = distance, y = size)) +
  geom_point( alpha = 0.5,
              position = position_jitter (width = 0.4)) +

  xlim(0, 300) +
  ylim(0, 20) +
  geom_smooth(method = "lm")





detfun_dat_lo$sizebin <- cut(detfun_dat_lo$size,
                             breaks = seq(0, max(detfun_dat_lo$size), 1))

detfun_dat_lo[distance <= trunc.dist_lo] %>%
  group_by(sizebin) %>%
  tally() %>%
  mutate(prop = n/sum(n))



# Final Detection Function ----
# Truncation at trunc.dist_lo 350 m - discard ~12% of data
# cutpoints:  cutpoints_lo: c(0, 50, 150, 250, 350)
# Half normal
# No adjustment
# No covariate

## lo: lagenorynchus obscurus
## df: detection function
## hn: half normal
## trun: trnacated distance
## cp: use cutpoints to deal with grouped data


df.lo <- lo.df.hn.trun.cp

qqplot.ddf(df.lo$ddf, plot = TRUE)
