library(oce)
library(ocedata)

rm(list=ls())
#Typhoon data from Japan Meteological Agency

d2019<-read.csv("typhoon_table2019.csv",h=T,fileEncoding ='cp932',stringsAsFactors=F)
d2017<-read.csv("typhoon_table2017.csv",h=T,fileEncoding ='cp932',stringsAsFactors=F)

yes<-unique(d[d$Landfall==1,]$Typhoon.name)
# zy 
zy<-length(yes)

#Lambert's conformal conic projection
#Popular mid-latitude projections
data(coastlineWorldFine, package="ocedata")

mapPlot(coastlineWorldFine,
           longitudelim=c(138,141), latitudelim=c(34.7, 35.8),
           projection="+proj=lcc +lat_0=2 +lat_1=65 +lon_0=140", col="lightgray")
# FAXAI in 2019
{
  lon=d2019[d2019$Typhoon.name== "FAXAI" ,]$Long
  lat=d2019[d2019$Typhoon.name== "FAXAI"  ,]$Lat  
  rank=(d2019[d2019$Typhoon.name== "FAXAI"  ,]$Class%%7)%%6
  mapLines(lon, lat, col='orange', lwd=1)
  mapPoints(lon,lat,col="orange",cex=rank/3,pch=16)
  mapText(lon, lat, d2019[d2019$Typhoon.name== "FAXAI" ,]$Time, col = "orange", pos = 4, offset = 1)
  mapText(lon, lat, d2019[d2019$Typhoon.name== "FAXAI" ,]$pressure, col = "orange", pos = 3, offset = 1)}

# HAGIBIS in 2019
{
  lon=d2019[d2019$Typhoon.name== "HAGIBIS" ,]$Long
  lat=d2019[d2019$Typhoon.name== "HAGIBIS"  ,]$Lat  
  rank=(d2019[d2019$Typhoon.name== "HAGIBIS"  ,]$Class%%7)%%6
  mapPoints(139.14406,35.16138,col="black",cex=1.5,pch=16)#調査地
  mapPoints(138.9531,34.64667,col="black",cex=1.5,pch=17)#下田NOWPHAS
  mapLines(lon, lat, col='red', lwd=1)
  mapPoints(lon,lat,bg="red",col="red",cex=rank/3,pch=16)
  mapText(lon, lat, d2019[d2019$Typhoon.name== "HAGIBIS" ,]$Time, col = "red", pos = 2, offset = 1)
  mapText(lon, lat, d2019[d2019$Typhoon.name== "HAGIBIS" ,]$pressure, col = "red", pos = 3, offset = 1)}


png(file="typhoon_tracks2019_1.png",
    width=600, height=400, res=100)


