# setwd("C:/Users/54280/Documents/manuscritos/Dusky and common abundance/Data")
# setwd("D:/Vana/Documentos/manuscritos/Dusky and common abundance/Data")



distdata_dd<-read.csv("distdata_ddwholesample.csv",sep=";")
dim(distdata_dd)

distdata_ddoption2<-read.csv("distdata_dd_todos2.csv",sep=";")
dim(distdata_ddoption2)

obsdata_dd<-read.csv("obsdata_dd.csv",sep=";")
dim(obsdata_dd)


distdata_lo<-read.csv("distdata_lowholesample.csv",sep=";")
dim(distdata_lo)


obsdata_lo<-read.csv("obsdata_lo.csv",sep=";")
dim(obsdata_lo)

segdata<- read.csv("segdata.csv",sep=";")

preddata<- read.csv("preddata.csv",sep=";")

head(distdata_dd)
head(distdata_lo)
head(obsdata_dd)
head(obsdata_lo)
head(segdata)
head(preddata)
#save(segdata, obsdata_dd,obsdata_lo, distdata_dd,distdata_lo, preddata,
    # file="../Data/dolphins.RData")

install.packages(c("dsm", "Distance", "knitr", "distill", "ggplot2", "sf",
                   "terra", "plyr", "tweedie"))

library(dsm)
library(ggplot2)
library(sp)
library(knitr)
library(terra)

# plotting options
gg.opts <- theme(panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.background=element_blank())

library(sf)#reemplaza a RGDAL
library(plyr)


#probando con archivo "Patagonia_completa.shp"
#Luego  creo el objeto survey.area porque no lo encuentra

patagonia <- st_read("C:/Users/54280/Documents/Pampa Azul GSJ/Patagonia_Completa.shp", quiet = TRUE)

patagonia <- st_read("D:/Vana/Documentos/Proyectos/Pampa Azul GSJ/Patagonia_Completa.shp", quiet = TRUE)


# Verificar datos
print(patagonia)


windows();plot(st_geometry(patagonia))
windows();plot(st_geometry(patagonia), xlim=c(-63.6,-63.59), ylim=c(-42.5,-40.5), col="lightgrey", axes=TRUE)



####con archivos shp de Nadia
survey.area <- st_read("survey.area.shp", quiet = TRUE)

# Verificar datos
print(survey.area)
windows();plot(st_geometry(survey.area), col="light blue", axes=TRUE)

#este archivo ya esta recortado

#idem pred.polys
pred.polys<-st_read("gridproy41.1.shp")
windows();plot(st_geometry(pred.polys))

#siguiendo vignete de Miller Mexico example##########
#######################siguiendo vignete https://distancesampling.org/dsm/articles/lines_gomex/mexico-analysis.html
library(sf)#reemplaza a RGDAL
library(plyr)

# tell R that the survey.area object is currently in lat/long
#no se si hace falta esto porque survey.area
#sp::proj4string(survey.area) <- sp::CRS("+proj=longlat +datum=WGS84")

#segun vignete survey.area y pred.polys son objetos

survey.area <- st_read("survey.area.shp", quiet = TRUE)

pred.polys<-st_read("gridproy41.1.shp")

predsf <- st_as_sf(pred.polys)

area.sf <- st_as_sf(survey.area)

st_crs(area.sf) <- "WGS84"
area.sf.proj <- st_transform(area.sf, crs = st_crs(predsf))

# Convert preddata to a spatial object
preddata_sf <- st_as_sf(preddata, coords=c("x", "y"))
st_crs(preddata_sf) <- st_crs(area.sf.proj)

# Perform the intersection
preddata_sf <- st_intersection(preddata_sf, area.sf.proj)


coords_preddata <- data.frame(st_coordinates(preddata_sf))

preddata_sf$x <- coords_preddata$X
preddata_sf$y <- coords_preddata$Y

#The below code generates this plot, which shows the survey area with the transect lines
#overlaid (using data from segdata).

segdata_sf <- st_as_sf(segdata, coords = c("x","y"))
st_crs(segdata_sf) <- st_crs(area.sf.proj)

# study area outline and segment centres
plot_segments <- ggplot() +
  geom_sf(data=survey.area, fill="lightblue", color = "blue", linewidth=.1) +
  geom_sf(data=segdata_sf, fill=NA, color="black", linewidth=.3) +
  labs(title="San Matias Gulf",
       subtitle = "Points are segment centres") +
  scale_fill_viridis_c(option = "magma", guide = "none")

windows();plot_segments


####para plotear grilla
# plot as projected
plot(st_geometry(predsf), axes=TRUE)


####################################fin vignete de Miller

####Siguiendo vignete de data formatting

coastline<-read_sf("C:/Users/54280/Documents/Pampa Azul GSJ/Patagonia_Completa.shp", quiet = TRUE)
coastline<-read_sf("D:/Vana/Documentos/Proyectos/Pampa Azul GSJ/Patagonia_Completa.shp", quiet = TRUE)
p <- ggplot() +
  # geom_sf knows what to do with spatial data
  geom_sf(data=coastline) +
  # chop to be on a better scale
  coord_sf(xlim=c(-65.6, -62.5), ylim=c(-42.5, -40.5), expand=FALSE) +
  # make the plot look simpler
  theme_minimal()

print(p)


####para agregar linea de costa al mapa del area y segmentos

plot_segments2 <- ggplot() +
  #geom_sf(data=survey.area, fill="lightblue", color = "blue", linewidth=.1) +#
  geom_sf(data=segdata_sf, fill=NA, color="black", linewidth=.3) +
  geom_sf(data=coastline)+
  labs(title="San Matias Gulf",
       subtitle = "Points are segment centres") +
  # chop to be on a better scale
  coord_sf(xlim=c(-65.6, -62.5), ylim=c(-42, -40.5), expand=FALSE) +#
  # make the plot look simpler
  theme_minimal()

windows();plot_segments2
####################################################fin

#Distance data


#Spatial data

prediction_grid <- st_make_grid(area.sf.proj, cellsize = c(9000,9000))
prediction_grid_sf <- st_sf(geometry = prediction_grid)
cropped_grid <- st_join(prediction_grid_sf, preddata_sf, join = st_nearest_feature)
cropped_grid <- st_intersection(cropped_grid, area.sf.proj)

Lo <- ggplot() +
      labs(title = "Dusky dolphins, San Matias Gulf",
       subtitle = "Depth in meters, size of detected dolphin groups") +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(aes(x, y, size=size), data=distdata_lo, colour="red",alpha=I(5)) +
  scale_fill_viridis_c(option = "viridis", direction = 1)

windows();Lo

#geom_sf(data=cropped_grid, aes(fill=depth), color=NA) +


dd <- ggplot() +
    labs(title = "Common dolphins, San Matias Gulf",
       subtitle = "Depth in meters, size of detected dolphin groups") +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(aes(x, y, size=size), data=distdata_dd, colour="green",alpha=I(0.5)) +
  scale_fill_viridis_c(option = "viridis", direction = 1)
windows();dd

ddoption2 <- ggplot() +
  labs(title = "Common dolphins, San Matias Gulf",
       subtitle = "Depth in meters, size of detected dolphin groups") +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(aes(x, y, size=size), data=distdata_ddoption2, colour="green",alpha=I(0.5)) +
  scale_fill_viridis_c(option = "viridis", direction = 1)
windows();ddoption2

#geom_sf(data=cropped_grid, aes(fill=depth), color=NA) +

###############################################################
###############################################################
###Lagenorhynchus obscurus Lo

#######Detection function

library(Distance)



dim(distdata_lo)
max(distdata_lo$distance)

#######analisis exploratorio de datos de distancia

windows(); hist(distdata_lo$distance, main = "Dusky dolphin n=119", breaks = 50, xlab = "Distance (m)")

###ananlisis exploratorio covariables

windows();par(mfrow = c(2, 2))
hist(distdata_lo$size, main = "Dusky dolphins", breaks= 10, xlab = "Cluster size")
# plots of distance vs. cluster size
plot(distdata_lo$distance, distdata_lo$size, main = "", xlab = "Distance (m)",
     ylab = "Group size", pch = 19, cex = 0.5, col = rgb(0.1, 0.1, 0.74, 0.7))
# lm fit
l.dat <- data.frame(distance = seq(0, 8000, len = 1000))
lo <- lm(size ~ distance, data = distdata_lo)

lines(l.dat$distance, as.vector(predict(lo, l.dat)))
plot(distdata_lo$distance, distdata_lo$beaufort, main = "", xlab = "Distance (m)",
     ylab = "Beaufort sea state", pch = 19, cex = 0.5, col = rgb(0.1, 0.1, 0.74, 0.7))
plot(distdata_lo$distance, distdata_lo$ship, main = "", xlab = "Distance (m)",
     ylab = "Vessel type", pch = 19, cex = 0.5, col = rgb(0.1, 0.1, 0.74, 0.7))



#Detection function Lagenorhynchus obscurus

#####truncado en 400
idx <- which(distdata_lo$distance>400)
distdata_lo400<- distdata_lo[idx,]
dim(distdata_lo400)

####el truncado a 400 mts elimina 10 observaciones sobre 119, representa 8,4% de los datos

cutpoints400<-c(0,100,200,300,400)



##############################

####considerando binned data
detfc.hr.nulllo_400bin<-ds(distdata_lo, truncation = 400, key="hr",  cutpoints= cutpoints400)

detfc.hn.nulllo_400bin<-ds(distdata_lo, truncation = 400, key="hn", cutpoints= cutpoints400)

detfc.unif.nulllo_400bin<-ds(distdata_lo, truncation = 400, key="unif",  cutpoints= cutpoints400)


AIC(detfc.hr.nulllo_400bin, detfc.hn.nulllo_400bin, detfc.unif.nulllo_400bin)

windows();par(mfrow = c(2, 2))
plot(detfc.hr.nulllo_400bin, showpoints=FALSE, pl.den=0, lwd=2, main="Hazard rate 0")
plot(detfc.hn.nulllo_400bin, showpoints=FALSE, pl.den=0, lwd=2, main="Half normal 0")
plot(detfc.unif.nulllo_400bin, showpoints=FALSE, pl.den=0, lwd=2, main = "Uniform")

ddf.gof(detfc.hr.nulllo_400bin$ddf)
ddf.gof(detfc.hn.nulllo_400bin$ddf)
ddf.gof(detfc.unif.nulllo_400bin$ddf)

#####Tabla resumen
knitr::kable(summarize_ds_models(detfc.hr.nulllo_400bin, detfc.hn.nulllo_400bin,detfc.unif.nulllo_400bin), digits=3,  caption="K+A Model comparison table for dusky dolphins data")


#best model selected for Lagenorhynchus
###con adjustment= NULL da lo mismo ya que no mejora con serie de cosenos

detfc.hr.nulllo_400bin<-ds(distdata_lo, truncation = max(cutpoints400), key="hr",  cutpoints= cutpoints400)
summary(detfc.hr.nulllo_400bin)

windows();par(mfrow=c(1,2))

plot(detfc.hr.nulllo_400bin, showpoints=FALSE, pl.den=0, lwd=2)
gof_ds(detfc.hr.nulllo_400bin, plot=TRUE, ks= FALSE)
plot(detfc.hr.nulllo_400bin)

#para obtner qq plot pero no lo logre
qqplot.ddf(detfc.hr.nulllo_400bin, nboot=100, ks=TRUE)
ddf.gof(detfc.hr.nulllo_400bin, qq=TRUE, ks=TRUE)


####agregando covariables ambientales
#ship
#Beaufort
#group size


###detection function with covariates


detfc.hr.lo_400binbeau <- ds(distdata_lo, truncation= 400, cutpoints = cutpoints400,
                             formula = ~as.factor(beaufort), key = "hr", adjustment = NULL)
summary(detfc.hr.lo_400binbeau)


detfc.hr.lo_400binship <- ds(distdata_lo, truncation= 400, cutpoints = cutpoints400,
                             formula = ~as.factor(ship), key = "hr", adjustment = NULL)

detfc.hr.lo_400binGsize <- ds(distdata_lo, truncation= 400, cutpoints = cutpoints400,
                              formula = ~size, key = "hr", adjustment = NULL)
detfc.hr.lo_400binGsize_ship <- ds(distdata_lo, truncation= 400, cutpoints = cutpoints400,
                                   formula = ~size + as.factor (ship), key = "hr", adjustment = NULL)

AIC(detfc.hr.nulllo_400bin, detfc.hr.lo_400binbeau, detfc.hr.lo_400binship, detfc.hr.lo_400binGsize,detfc.hr.lo_400binGsize_ship)



knitr::kable(summarize_ds_models(detfc.hr.nulllo_400bin, detfc.hr.lo_400binbeau, detfc.hr.lo_400binship, detfc.hr.lo_400binGsize,detfc.hr.lo_400binGsize_ship),digits=3,
             caption="Model comparison table for Dusky dolphins data")

####el modelo con group size tiene el menor AIC 222,9562,
#####pero la diferencia con el modelo sin covariates (224,84) es menor a 2


summary(detfc.hr.lo_400binGsize)

ddf.gof(detfc.hr.lo_400binGsize$ddf)
ddf.gof(detfc.hr.nulllo_400bin$ddf, plot = TRUE, main = "Hazarda rate")

cdf.ds(detfc.hr.nulllo_400bin, newdata= NULL)

#############Density surface modelling
###################fitting a dsm

obsdata_lo<-read.csv("obsdata_lo.csv",sep=";")
dim(obsdata_lo)

####segdata y preddata son los mismos archivos para ambas especies

###modelo seleccionado para detection function (se puede correr desde aca o pasar a Fitting a DSM,
#si ya se cuenta con la funcion de deteccion)

cutpoints400<-c(0,100,200,300,400)
detfc.hr.nulllo_400bin<-ds(distdata_lo, truncation = max(cutpoints400), key="hr",  cutpoints= cutpoints400)
summary(detfc.hr.nulllo_400bin)

detfc.lo<-detfc.hr.nulllo_400bin


#Fitting a DSM
#By setting group=TRUE, the abundance of clusters/groups rather than individuals can be estimated
#Note we set method="REML" to ensure that smooth terms are estimated reliably.

dsm.xy <- dsm(count~s(x,y), detfc.lo, segdata, obsdata_lo, method="REML")

summary(dsm.xy)


vis.gam(dsm.xy, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100)

#adding another environmental covariate to the spatial model

dsm.xy.depth <- dsm(count~s(x,y,k=10) + s(depth,k=20), detfc.lo, segdata, obsdata_lo, method="REML")
summary(dsm.xy.depth)

vis.gam(dsm.xy.depth, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100)

plot(dsm.xy.depth, select=2)
gam.check(dsm.xy.depth)

#Randomised quantile residuals
#In the top right panel of the above gam.check plots the residuals vs. linear predictor plot
#includes a odd line of predictions. These are an artifact of the link function, showing the exact zeros in the data.
#These can be misleading and distracting, making it difficult to see whether residuals show heteroskedasticity.

#Randomised quantile residuals (Dunn & Smyth, 1996) avoid this issue by transforming the residuals to be exactly normally distributed.
#This makes the residuals vs. linear predictor plot much easier to interpret as it therefore doesn't include the artifacts generated by the link function.
#These plots can be produced using rqgam.check in dsm:

#Only negative binomial and Tweedie response distributions are supported.
rqgam_check(dsm.xy.depth)

#sacando variables
###full model
###covariates:dist.coast, slope, depth,sst, clo, grad, dist.up, Ano, Mes

model.lo1<-dsm.lo.full <- dsm(count~s(x,y,k=20)+
                     s(dist.coast, k = 20)+
                     s(slope, k = 20) +
                     s(depth, k = 20)+
                     s(sst, k = 20)+
                     s(clo, k =20)+
                     s(grad, k = 20)+
                     s(dist.up, k = 20)+
                     s(Ano, k = 10)+
                     s(Mes_n, k = 10),
                   detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")

summary(dsm.lo.full)

###collinearity

concurvity(dsm.lo.full, full=FALSE)$estimate

#####segun tabla
##Dist.coast y depth
##6.564505e-01
##5.581719e-01

##Sst y mes
##7.938289e-01
##5.349435e-01

###tambien hay  variables que dan edf=1,
#s(dist.coast) 1.000  1.000  4.015 0.04512 *
#s(sst)        1.000  1.000 47.713 < 2e-16 ***
#s(clo)        1.000  1.000  0.150  0.6983
#s(dist.up)    1.000  1.000  7.722 0.00546 **
#s(Mes_n)      1.000  1.000 44.454 < 2e-16 ***

###diagnostic
windows();par(mfrow=c(2,2))
gam.check(dsm.lo.full)

####autocorrelacion espacial: pareciera no ser importante
windows();par(mfrow=c(1,1))
dsm_cor(dsm.lo.full, Segment.Label= "Sample.Label", max=10)

##sacando Depth, que es una de las covariates que mostro colinearidad con dist.coast,
#y considerando lineal a la relacion con variables con edf=1

model.lo2<-dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes <- dsm(count~s(x,y,k=20)+
                                                            s(slope, k = 20) +
                                                            dist.coast +
                                                            sst+
                                                            clo+
                                                            s(grad, k = 20)+
                                                            dist.up+
                                                            s(Ano, k = 10)+
                                                            Mes_n,
                                                          detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes)


concurvity(dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, full= FALSE)$estimate

AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes)

###sacando dist.coast, y dejando depth, que es la otra covariate con colinearidad

model.lo3<-dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes <- dsm(count~s(x,y,k=20)+
                                                        s(slope, k = 20) +
                                                        s(depth, k = 20)+
                                                        sst+
                                                        clo+
                                                        s(grad, k = 20)+
                                                        dist.up+
                                                        s(Ano, k = 10)+
                                                        Mes_n,
                                                      detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes)
concurvity(dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes, full= FALSE)$estimate

AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes)

###sacando ambas depth y dist.coast

model.lo4<-dsm.lo.slope_sst_clo_grad_distup_Ano_Mes <- dsm(count~s(x,y,k=20)+
                                                  s(slope, k = 20) +
                                                  sst+
                                                  clo+
                                                  s(grad, k = 20)+
                                                  dist.up+
                                                  s(Ano, k = 10)+
                                                  Mes_n,
                                                detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.slope_sst_clo_grad_distup_Ano_Mes)
AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.slope_sst_clo_grad_distup_Ano_Mes)

concurvity(dsm.lo.slope_sst_clo_grad_distup_Ano_Mes, full=FALSE)$estimate

####dejando depth y volviendo mes con spline

model.lo5<-dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2 <- dsm(count~s(x,y,k=20)+
                                                         s(slope, k = 20) +
                                                         s(depth, k = 20)+
                                                         sst+
                                                         clo+
                                                         s(grad, k = 20)+
                                                         dist.up+
                                                         s(Ano, k = 10)+
                                                         s(Mes_n, k = 10),
                                                       detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2)
AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2 )

concurvity(dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2, full=FALSE)$estimate
###no hay diferencia si se considera el mes con spline, so se deja como lineal

###sacando sst, otra de las que dio colinearidad con Mes

model.lo6<-dsm.lo.depth_slope_clo_grad_distup_Ano_Mes <- dsm(count~s(x,y,k=20)+
                                                    s(slope, k = 20) +
                                                    s(depth, k = 20)+
                                                    clo+
                                                    s(grad, k = 20)+
                                                    dist.up+
                                                    s(Ano, k = 10)+
                                                    Mes_n,
                                                  detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_slope_clo_grad_distup_Ano_Mes)
AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2,dsm.lo.depth_slope_clo_grad_distup_Ano_Mes)

###sacando mes y dejando sst
model.lo7<-dsm.lo.depth_slope_sst_clo_grad_distup_Ano <- dsm(count~s(x,y,k=20)+
                                                    s(slope, k = 20) +
                                                    s(depth, k = 20)+
                                                    sst+
                                                    clo+
                                                    s(grad, k = 20)+
                                                    dist.up+
                                                    s(Ano, k = 10),
                                                  detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")



summary(dsm.lo.depth_slope_sst_clo_grad_distup_Ano)


AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2,dsm.lo.depth_slope_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_clo_grad_distup_Ano)


#####sacando mes y sst
model.lo8<-dsm.lo.depth_slope_clo_grad_distup_Ano <- dsm(count~s(x,y,k=20)+
                                                s(slope, k = 20) +
                                                s(depth, k = 20)+
                                                clo+
                                                s(grad, k = 20)+
                                                dist.up+
                                                s(Ano, k = 10),
                                              detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_slope_clo_grad_distup_Ano)

AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2,dsm.lo.depth_slope_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_clo_grad_distup_Ano,dsm.lo.depth_slope_clo_grad_distup_Ano)
AIC(model.lo1, model.lo2, model.lo3, model.lo4, model.lo5, model.lo6, model.lo7, model.lo8)
###el Mes parece ser la covariate con mayor influencia, no asi la sst

#Ver tabla de AIC
#                                                      df      AIC
#dsm.lo.full                                        30.17169 1091.511
#dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes 25.51525 1104.156
#dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes     23.05923 1092.911##partimos de aca para seguir sacando variables
#dsm.lo.slope_sst_clo_grad_distup_Ano_Mes           19.41419 1097.441
#dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2    23.06521 1092.923
#dsm.lo.depth_slope_clo_grad_distup_Ano_Mes         15.92741 1136.693
#dsm.lo.depth_slope_sst_clo_grad_distup_Ano         15.55844 1138.722
#dsm.lo.depth_slope_clo_grad_distup_Ano             15.45919 1147.944


##consultar antes de seguir
##definir nuevo modelo sin las variables que no dieron significativas, dejando solo el mes?




# make a data.frame to print out
mod_results <- data.frame("Model name" = c("`model.lo1`",
                                           "`model.lo2`",
                                           "`model.lo3`",
                                           "`model.lo4`",
                                           "`model.lo5`",
                                           "`model.lo6`",
                                           "`model.lo7`",
                                           "`model.lo8`"),
                          "Description" = c("location, depth, distance to coast, bottom slope, sst, clo, sst grad, distance to upwelling, Year, Month, quasipoisson",
                                           "location, distance to coast, bottom slope, sst, clo, sst grad, distance to upwelling, Year, Month, quasipoisson",
                                           "location, depth, bottom slope, sst, clo, sst grad, distance to upwelling, Year, Month, quasipoisson",                                            "Soap film smooth of location, smooth of depth, Tweedie",
                                           "location, bottom slope, sst, clo, sst grad, distance to upwelling, Year, Month, quasipoisson",
                                           "location, depth, bottom slope, sst, clo, sst grad, distance to upwelling, Year, Month as spline, quasipoisson",
                                           "location, depth, bottom slope, clo, sst grad, distance to upwelling, Year, Month, quasipoisson",
                                           "location, depth, bottom slope, sst, clo, sst grad, distance to upwelling, Year, quasipoisson",
                                           "location, depth, bottom slope, clo, sst grad, distance to upwelling, Year, quasipoisson"),
                          "Deviance explained" = c(unlist(lapply(list(model.lo1,
                                                                      model.lo2,
                                                                      model.lo3,
                                                                      model.lo4,
                                                                      model.lo5,
                                                                      model.lo6,
                                                                      model.lo7,
                                                                      model.lo8),function(x){paste0(round(summary(x)$dev.expl*100)),"%"}))))


kable(mod_results, col.names=c("Model name", "description", "Deviance explained"))

#c("`dsm.lo.full`",
  "`dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes`",
  "`dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes`",
  "`dsm.lo.slope_sst_clo_grad_distup_Ano_Mes`",
  "`dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes2`",
  "`dsm.lo.depth_slope_clo_grad_distup_Ano_Mes`",
  "`dsm.lo.depth_slope_sst_clo_grad_distup_Ano`",
  "`dsm.lo.depth_slope_clo_grad_distup_Ano`"),


#partiendo del modelo
summary(dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes)

AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes)

#sacando clorofila
dsm.lo.depth_slope_sst_grad_distup_Ano_Mes <- dsm(count~s(x,y,k=20)+
                                                    s(slope, k = 20) +
                                                    s(depth, k = 20)+
                                                    sst+
                                                    s(grad, k = 20)+
                                                    dist.up+
                                                    s(Ano, k = 10)+
                                                    Mes_n,
                                                  detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_slope_sst_grad_distup_Ano_Mes)


AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes)

#sacando grad y clo
dsm.lo.depth_slope_sst_distup_Ano_Mes <- dsm(count~s(x,y,k=20)+
                                               s(slope, k = 20) +
                                               s(depth, k = 20)+
                                               sst+
                                               dist.up+
                                               s(Ano, k = 10)+
                                               Mes_n,
                                             detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_slope_sst_distup_Ano_Mes)


AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_distup_Ano_Mes)


###dejando gradiente y considerando slope como lineal

dsm.lo.depth_slope_sst_grad_distup_Ano_Mes2 <- dsm(count~s(x,y,k=20)+
                                                     slope +
                                                     s(depth, k = 20)+
                                                     sst+
                                                     s(grad, k = 20)+
                                                     dist.up+
                                                     s(Ano, k = 10)+
                                                     Mes_n,
                                                   detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_slope_sst_grad_distup_Ano_Mes2)

AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes2)

###sacando gradiente
dsm.lo.depth_slope_sst_distup_Ano_Mes2 <- dsm(count~s(x,y,k=20)+
                                                slope +
                                                s(depth, k = 20)+
                                                sst+
                                                dist.up+
                                                s(Ano, k = 10)+
                                                Mes_n,
                                              detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_slope_sst_distup_Ano_Mes2)

AIC(dsm.lo.full,  dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes2,dsm.lo.depth_slope_sst_distup_Ano_Mes2)

####dejando gradiente y sacando slope
dsm.lo.depth_sst_grad_distup_Ano_Mes2 <- dsm(count~s(x,y,k=20)+
                                               s(depth, k = 20)+
                                               sst+
                                               s(grad, k = 20)+
                                               dist.up+
                                               s(Ano, k = 10)+
                                               Mes_n,
                                             detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_sst_grad_distup_Ano_Mes2)

AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes2,dsm.lo.depth_sst_grad_distup_Ano_Mes2 )


####dejando gradiente y sacando slope y el año

dsm.lo.depth_sst_grad_distup_Mes2 <- dsm(count~s(x,y,k=20)+
                                           s(depth, k = 20)+
                                           sst+
                                           s(grad, k = 20)+
                                           dist.up+
                                           Mes_n,
                                         detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_sst_grad_distup_Mes2)

AIC(dsm.lo.full, dsm.lo.distcoast_slope_sst_clo_grad_distup_Ano_Mes, dsm.lo.depth_slope_sst_clo_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes,dsm.lo.depth_slope_sst_grad_distup_Ano_Mes2,dsm.lo.depth_sst_grad_distup_Ano_Mes2,dsm.lo.depth_sst_grad_distup_Mes2 )


####mejor modelo
dsm.lo.depth_sst_grad_distup_Ano_Mes2 <- dsm(count~s(x,y,k=20)+
                                               s(depth, k = 20)+
                                               sst+
                                               s(grad, k = 20)+
                                               dist.up+
                                               s(Ano, k = 10)+
                                               Mes_n,
                                             detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.lo.depth_sst_grad_distup_Ano_Mes2)

windows();par(mfrow=c(2,2))
gam.check(dsm.lo.depth_sst_grad_distup_Ano_Mes2)

windows();par(mfrow=c(1,1))
plot(dsm.lo.depth_sst_grad_distup_Ano_Mes2)

###Segun el script "lo_mes" de Nadia
###modelo final=count ~  summary(dsm.nb.lo_xy_s_sst_dup_2_m)

dsm.nb.lo_xy_s_sst_dup_2_m<- dsm(count~s(x,y,k=20)+
                                   slope+
                                   s(sst, k = 20)+
                                   dist.up+
                                   s(Mes_n, k = 10),
                                 detfc.lo, segdata, obsdata_lo, family=nb(), method="REML")
summary(dsm.nb.lo_xy_s_sst_dup_2_m)
AIC(dsm.nb.lo_xy_s_sst_dup_2_m)
plot(dsm.nb.lo_xy_s_sst_dup_2_m,select=3, ylim=c(-15,15),xlab="Mes", ylab="s(Mes, 7.06)")
gam.check(dsm.nb.lo_xy_s_sst_dup_2_m)





##############################################
##############################################

####Delphinus delphis


# histograms distances whole sample

distdata_dd<-distdata_dd2
dim(distdata_dd)
windows();par(mfrow = c(1, 1))
hist(distdata_dd$distance, main = "Common dolphin n=718", breaks= 80, xlab = "Distance (m)")

###en el grafico se puede ver el redondeo en clases de a 100
###ademas posiblemente la distancia 0 esta sobrerepresentada, posiblemente por acercamiento de los animales
#####Segun Bucckland et al 2001 sections 4.5 y 7.4.1.2, agrupar los datos de distancia son una solucion para ambos problemas
###por lo tanto se van a tratar como datos de distancias redondeadas en lugar de exactas, esto se hara incluyendo la opcion "cutpoints"


#####exploratory analysis of covariates to be added to the detection function
######este analisis se hace con todos los datos sin truncar y sin redondear



windows();par(mfrow = c(2, 2))
hist(distdata_dd$size, breaks = 50, main="", xlab = "Cluster size")

# plots of distance vs. cluster size
plot(distdata_dd$distance, distdata_dd$size, main = "", xlab = "Distance (m)",
     ylab = "Group size", pch = 19, cex = 0.5, col = rgb(0.1, 0.1, 0.74, 0.7))
# lm fit
l.dat <- data.frame(distance = seq(0, 8000, len = 1000))
lo <- lm(size ~ distance, data = distdata_dd)

lines(l.dat$distance, as.vector(predict(lo, l.dat)))

#Beaufort
plot(distdata_dd$distance, distdata_dd$beaufort, main = "", xlab = "Distance (m)",
     ylab = "Beaufort sea state", pch = 19, cex = 0.5, col = rgb(0.1, 0.1, 0.74, 0.7))

#Vessel type
plot(distdata_dd$distance, distdata_dd$ship, main = "", xlab = "Distance (m)",
     ylab = "vessel type", pch = 19, cex = 0.5, col = rgb(0.1, 0.1, 0.74, 0.7))

#####en los graficos se observa una tendencia a mayor numero de observaciones a mayor distancia en el Vesseltype #1, particularmente las observaciones entre 500 y 1000 mts
#####tambien se observa grupos mas grandes a mayor distancia

##########################################################
#######Estimating detection function

###en este script, el truncado es a 400 mts, lo cual elimina aprox 8% de los datos, 56 observaciones en el caso de delphinus sobre un total de 717

idx <- which(distdata_dd$distance>400)
distdata_dd400<- distdata_dd[idx,]
dim(distdata_dd400)

#########################
####funciones de deteccion usando el archivo completo, truncando a 400 y usando cutpoints para tratar las distancias exactas como binned data

cutpoints400<-c(0,100,200,300,400)

##Hazard rate
detfc.hr.nulldd_400bin2<-ds(distdata_dd,  key="hr",  cutpoints=cutpoints400, truncation=400)
detfc.hr.nulldd_400bin1<-ds(distdata_dd,  key="hr",  cutpoints=cutpoints400, truncation=400, nadj=1)
detfc.hr.nulldd_400bin0<-ds(distdata_dd,  key="hr",  cutpoints=cutpoints400, truncation=400, adjustment = NULL)

##Half normal
detfc.hn.nulldd_400bin2<-ds(distdata_dd, key="hn", cutpoints= cutpoints400, truncation = 400)
detfc.hn.nulldd_400bin1<-ds(distdata_dd, key="hn", cutpoints= cutpoints400, truncation = 400, nadj=1)
detfc.hn.nulldd_400bin0<-ds(distdata_dd, key="hn", cutpoints= cutpoints400, truncation = 400, adjustment = NULL)

##Uniform
detfc.unif.nulldd_400bin<-ds(distdata_dd, key="unif",  cutpoints = cutpoints400, truncation=400)

##Seleccion de funcion de deteccion
##Para seleccionar el modelo de function de deteccion se miran los graficos,
##esperando una function con un hombro, minimo AIC, y bondad de ajuste

windows();par(mfrow = c(2, 3))
plot(detfc.unif.nulldd_400bin, main="Uniform")
plot(detfc.hn.nulldd_400bin0, main="half normal 0")
plot(detfc.hn.nulldd_400bin1, main="half normal 1")
plot(detfc.hn.nulldd_400bin2, main="half normal 2")
plot(detfc.hr.nulldd_400bin0, main="hazard rate 0")
plot(detfc.hr.nulldd_400bin1, main="hazard rate 1")

ddf.gof(detfc.unif.nulldd_400bin)
ddf.gof(detfc.hn.nulldd_400bin0)
ddf.gof(detfc.hn.nulldd_400bin1)
ddf.gof(detfc.hn.nulldd_400bin2)
ddf.gof(detfc.hr.nulldd_400bin0)
ddf.gof(detfc.hr.nulldd_400bin1)

AIC(detfc.unif.nulldd_400bin, detfc.hn.nulldd_400bin0, detfc.hn.nulldd_400bin1,
    detfc.hn.nulldd_400bin2, detfc.hr.nulldd_400bin1, detfc.hr.nulldd_400bin0)

#####Tabla resumen
knitr::kable(summarize_ds_models(detfc.hn.nulldd_400bin1, detfc.hn.nulldd_400bin2,detfc.hr.nulldd_400bin1, detfc.hr.nulldd_400bin0), digits=3,             caption="K+A Model comparison table for common dolphins data")

###Selecciono hazard rate sin terminos de ajuste

####agregando covariables ambientales
#ship
#Beaufort
#group size



detfc.hr.dd_400binship <- ds(distdata_dd, truncation= 400, cutpoints = cutpoints400,
                             formula = ~as.factor(ship), key = "hr", adjustment = NULL)
detfc.hr.dd_400binbeauf <- ds(distdata_dd, truncation= 400, cutpoints = cutpoints400,
                              formula = ~as.factor(beaufort), key = "hr", adjustment = NULL)
detfc.hr.dd_400binGsize <- ds(distdata_dd, truncation= 400, cutpoints = cutpoints400,
                              formula = ~size, key = "hr", adjustment = NULL)

detfc.hr.dd_400binbeauf_size <- ds(distdata_dd, truncation= 400, cutpoints = cutpoints400,
                                   formula = ~as.factor(beaufort)+ size, key = "hr", adjustment = NULL)


detfc.hr.dd_400binGsize_ship <- ds(distdata_dd, truncation= 400, cutpoints = cutpoints400,
                                   formula = ~size + as.factor (ship), key = "hr", adjustment = NULL)




AIC(detfc.hr.nulldd_400bin0, detfc.hr.dd_400binship, detfc.hr.dd_400binbeauf,
    detfc.hr.dd_400binGsize, detfc.hr.dd_400binbeauf_size, detfc.hr.dd_400binGsize_ship)

#####Tabla resumen
knitr::kable(summarize_ds_models(detfc.hr.nulldd_400bin0, detfc.hr.dd_400binGsize,detfc.hr.dd_400binbeauf_size),digits=3,
             caption="K+A Model comparison table for common dolphins data")

summary(detfc.hr.dd_400binbeauf_size)

#####################################################
#####################################################
######################################################


#####Delphinus delphis DSM

######Density surface modelling
#####Fitting a density surface model

dim(obsdata_dd)
#####este archivo tiene 659 datos en lugar de 661
