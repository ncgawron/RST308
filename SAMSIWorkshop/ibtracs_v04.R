
rm(list=ls())

setwd("/Users/hangnguyen/Documents/ibtracs")


### load library to read .nc files
library(ncdf4)
library(ggplot2)
library(plyr)
### load all storms available in the IBTrACS record
#storms <- nc_open("IBTrACS.ALL.v04r00.nc")
storms = nc_open("IBTrACS.since1980.v04r00.nc")
#print(storms)
#nc_close(storms)

### Storm name
name = ncvar_get(storms, "name")

nstorms = length(name)

### Year based on season
season = ncvar_get(storms, "season")
count = as.numeric(table(season))
year = as.numeric(names(table(season)))

### setup ggplot theme
theme_mat = theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=14),
                  axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size=14))
### Storm counts
df.count = data.frame(year=year, count=count)
p1 = ggplot(df.count) + 
  geom_line(aes(x=year, y=count)) + 
  geom_point(aes(x=year, y=count), colour="red") 
p1 + theme_mat


### Type of storm
nature = ncvar_get(storms, "nature")
df.nature = as.data.frame(round(table(nature)[2:7]/360))
p2 = ggplot(data=df.nature, aes(x=nature, y=Freq, fill=nature)) + 
  geom_bar(stat="identity") + xlab("storm type") + ylab("count") + 
  theme(legend.position = "none")
p2 + theme_mat

png("./figures/storm_type.png", width=800, height=600, 
    units="px", pointsize=14)

p2+theme_mat

dev.off()

### Storm at each basin
basin = ncvar_get(storms, "basin")
#barplot(round(table(basin)[2:8]/360))
df.basin = as.data.frame(round(table(basin)[2:8]/360))
p3 = ggplot(data=df.basin, aes(x=basin, y=Freq, fill=basin)) + 
  geom_bar(stat="identity") + xlab("basin") + ylab("count") + 
  theme(legend.position = "none")
p3 + theme_mat




### Storm center 
Lat = ncvar_get(storms, "lat")
Lon = ncvar_get(storms, "lon")

### Distance to land: km
dist2land = ncvar_get(storms, "dist2land")

### Maximum sustained wind speed
mws = ncvar_get(storms, "wmo_wind") * 0.514444  # kt to m/s

summary(c(mws))

df.mws = data.frame(mws=c(mws))
p4 = ggplot(df.mws, aes(mws)) + 
  geom_histogram(bins=100, fill="#697bb3", color="gray") + 
  xlab("m/s") + ggtitle("Maximum Sustained Wind Speed")
p4 + theme_mat



### Saffir-Simpson hurricane wind scale (in m/s)
# https://en.wikipedia.org/wiki/Saffir–Simpson_scale

tstep = dim(mws)[1]

# Categorizing hurricanes 
SSHS = matrix(NA, tstep, nstorms)
SSHS[mws<33] = 0
SSHS[mws>=33 & mws<43] = 1
SSHS[mws>=43 & mws<50] = 2
SSHS[mws>=50 & mws<58] = 3
SSHS[mws>=58 & mws<70] = 4
SSHS[mws>=70] = 5


### Global map for MWS
library(mapproj)
library(fields)
library(scales)
library("viridis")

mws.df = data.frame(long=c(Lon), lat=c(Lat), value=c(mws))
mws.df$long[mws.df$long>180] = NA
mws.df = mws.df[complete.cases(mws.df), ]


proj.type = "mollweide"
proj.orient = c(90, 0, 0)

png("./figures/global_map_mws.png", width=800, height=400, 
    units="px", pointsize=14)

parset = par(mar=c(0,0,0,5)+1)
map("world", mar=rep(0,4), 
    proj=proj.type, orient=proj.orient,
    col=alpha("gray50", 0.8), fill=T, lwd=0.05)
map.grid(col="gray", labels=F, lty=2)
image.plot(legend.only=T, zlim=c(0,95), col=viridis(6),
           legend.line=0.5)
xyproj = mapproject(x=mws.df$long, y=mws.df$lat, proj=proj.type, orient=proj.orient)
val = color.scale(c(mws.df$value), col=alpha(viridis(6), 1))
points(xyproj$x, xyproj$y, col=val, pch=19, cex=0.25)
title("Maximum sustained wind speed")
par(parset)
dev.off()





### Global map for SSHS
library(mapproj)
library(fields)
library(scales)
library("viridis")

sshs.df = data.frame(long=c(Lon), lat=c(Lat), value=c(SSHS))
sshs.df$long[sshs.df$long>180] = NA
sshs.df = sshs.df[complete.cases(sshs.df), ]

proj.type = "mollweide"
proj.orient = c(90, 0, 0)

png("./figures/global_map_sshs.png", width=800, height=400, 
    units="px", pointsize=14)
parset = par(mar=c(0,0,0,5)+1)
map("world", mar=rep(0,4), 
    proj=proj.type, orient=proj.orient,
    col=alpha("gray50", 0.95), fill=T, lwd=0.05)
map.grid(col="gray50", labels=F, lty=2)
image.plot(legend.only=T, zlim=c(0,5), col=viridis(6))
xyproj = mapproject(x=sshs.df$long, y=sshs.df$lat, proj=proj.type, orient=proj.orient)
val = color.scale(sshs.df$value+1, col=alpha(viridis(6), 1))
points(xyproj$x, xyproj$y, col=val, pch=15, cex=0.25)
title("Saffir-Simpson hurricane scale")
par(parset)
dev.off()





### MWS against year 

library(dplyr)
library(ggplot2)
library(sp)
library(sf)
library(scico)
library(scales)

# Convert to matrix
theme_mat = theme(#plot.title = element_text(size = rel(1.2)),
  #panel.background = element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  legend.key.size = unit(1.0, "cm"),
  legend.key.width = unit(0.2,"cm"),
  legend.box.spacing = unit(0.1, "cm"),
  legend.box.margin = margin(0, 0, 0, 0, "cm"),
  plot.margin = margin(-0.95, 0.05, -0.02, -0.01, "cm"),
  axis.text=element_text(size=14),
  axis.title=element_text(size=14),
  axis.title.x=element_text(size=14),
  axis.text.x=element_text(size=14),
  axis.title.y=element_text(size=14),
  axis.text.y=element_text(size=14),
  legend.text=element_text(size=14),
  legend.title=element_text(size=14))


colnames(mws) = season
mws.df = as.data.frame(as.table(mws))
mws.df = mws.df %>% 
  dplyr::rename(
    mws = Freq,
    year = Var2
  )

colnames(Lat) = season
Lat.df = as.data.frame(as.table(Lat))
colnames(Lon) = season
Lon.df = as.data.frame(as.table(Lon))

mws.df$lat = Lat.df$Freq
mws.df$long = Lon.df$Freq 
mws.df = mws.df[complete.cases(mws.df), ] # Remove empty data

# North Atlantic 
window = (mws.df$long>-100 & mws.df$long<=-60 & 
            mws.df$lat>=15 & mws.df$lat<=50)

mws.df.NA = mws.df[window, ]

mws.df.NA$long[mws.df.NA$long>180] = NA
mws.df.NA = mws.df.NA[complete.cases(mws.df.NA), ]

# Dot plot of Max Wind Speeds 
g.mws.NA = ggplot(mws.df.NA) + 
  geom_point(aes(x=year, y=mws), size=1) + 
  theme(axis.text.x = element_text(angle=90))  
print(g.mws.NA + theme_mat)


### Spatial Map of MWS 
mws.sf = mws.df.NA
sp::coordinates(mws.sf) = ~long+lat
sf.df = sf::st_as_sf(mws.sf)
sf::st_crs(sf.df) = 4326

g = ggplot() + 
  geom_sf(data=sf.df, mapping=aes(color=mws), size=1, 
          inherit.aes = FALSE) + 
  scale_color_gradientn(colours=scico::scico(50, direction=-1, 
                                             palette="roma"),
                        name="m/s", limits=c(20, 80),
                        oob=scales::squish, breaks=seq(20,80,length.out=4)) + 
  scale_x_continuous(breaks=seq(-100,-60, length.out=5)) + 
  scale_y_continuous(breaks=seq(15, 50, length.out=6))

print(g)

# Start of Hang's code

#Linear Regression 
#probably not correct, for kicks and giggles mostly
lm(count ~ year, data = df.count)

# ARIMA Regression 
library(lmtest)
library(FitAR)
library(forecast)
library(lubridate)

# Convert into time series data
tsobject = ts(df.count[,2], start = 1980, end= 2021, frequency=1)

# Fit model
acf(tsobject, lag.max=30)
pacf(tsobject, lag.max=30)
adf.test(tsobject, alternative = 'stationary', k=1)
fitARIMA <- arima(tsobject, order=c(1,1,1),seasonal = list(order = c(1,1,0), period = 12),method="ML") #Change the order for a different model 
coeftest(fitARIMA)

# Checking the residuals for fit 
acf(fitARIMA$residuals)
boxresult <- LjungBoxTest(fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)

# Plot the prediction 
forecast(fitARIMA,5)
plot(forecast(fitARIMA,5, level = c(95)))

#Best ARIMA fit according to model criteria
fitARIMAmax <- auto.arima(tsobject, method= "ML", trace= TRUE, ic= "bic")
summary(fitARIMAmax)
plot(forecast(fitARIMAmax,5, level = c(95)))

# Checking the residuals for fit 
acf(fitARIMAmax$residuals)
boxresult2 <- LjungBoxTest(fitARIMAmax$residuals,k=2,StartLag=1)
plot(boxresult2[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMAmax$residuals)
qqline(fitARIMAmax$residuals)

### Remove outlier and interpolate 
tsobject2 = ts(df.count$count[1:41], start = 1980, end= 2020, frequency=1)

# Fit model
acf(tsobject2, lag.max=30)
pacf(tsobject2, lag.max=30)
adf.test(tsobject2, alternative = 'stationary', k=1)
fitARIMA2 <- arima(tsobject2, order=c(1,1,1),seasonal = list(order = c(1,1,0), period = 12),method="ML") #Change the order for a different model 
coeftest(fitARIMA2)

# Checking the residuals for fit 
acf(fitARIMA2$residuals)
boxresultmax <- LjungBoxTest(fitARIMA2$residuals,k=2,StartLag=1)
plot(boxresultmax[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA2$residuals)
qqline(fitARIMA2$residuals)

# Plot the prediction 
forecast(fitARIMA2,6)
plot(forecast(fitARIMA2,6, level = c(95)))

#Best ARIMA fit according to model criteria (AIC)
fitARIMAmax2 <- auto.arima(tsobject2, method= "ML", trace= TRUE)
summary(fitARIMAmax2)
plot(forecast(fitARIMAmax2,6, level = c(95)))

# Checking the residuals for fit 
acf(fitARIMAmax2$residuals)
boxresultmax2 <- LjungBoxTest(fitARIMAmax$residuals,k=2,StartLag=1)
plot(boxresultmax2[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMAmax2$residuals)
qqline(fitARIMAmax2$residuals)

# Plot the prediction 
forecast(fitARIMAmax2,6)
plot(forecast(fitARIMAmax2,6, level = c(95)))
