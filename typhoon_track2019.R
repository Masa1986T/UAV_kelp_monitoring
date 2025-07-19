library(oce)
library(ocedata)

rm(list=ls())
#On Oct 18, csv file includes data until typhoon 13
#10月18日現在、csvに含まれているのは2019年台風第13号までのデータです。
d2019<-read.csv("typhoon_table2019.csv",h=T,fileEncoding ='cp932',stringsAsFactors=F)
d2017<-read.csv("typhoon_table2017.csv",h=T,fileEncoding ='cp932',stringsAsFactors=F)

#tail(d,1)
#     年 月 日 時.UTC. 台風番号 台風名 階級 緯度  経度 中心気圧 最大風速
#418 2019  8 31       0     1912  PODUL    2 17.9 102.1     1000        0
#    X50KT長径方向 X50KT長径 X50KT短径 X30KT長径方向 X30KT長径 X30KT短径 上陸
#418             0         0         0             0         0         0    0
# 台風ごとの中心気圧のmin
#with(d, tapply(中心気圧, list(台風名), min, na.rm = TRUE))
# 上陸した台風は１、上陸しなかった台風は０
#with(d, tapply(上陸, list(台風名), max, na.rm = TRUE))
# 台風名
#names<-as.vector(unique(d$台風名))
# 上陸(landing)した台風名
yes<-unique(d[d$上陸==1,]$台風名)
# zy : legendでbquote()関数で表示
zy<-length(yes)

#ランベルト正角円錐図法(Lambert's conformal conic projection)
#Popular mid-latitude projections
data(coastlineWorldFine, package="ocedata")

mapPlot(coastlineWorldFine,
           longitudelim=c(138,141), latitudelim=c(34.7, 35.8),
           projection="+proj=lcc +lat_0=2 +lat_1=65 +lon_0=140", col="lightgray")
# Typhoon FAXAI in 2019
{
  lon=d2019[d2019$台風名== "FAXAI" ,]$経度
  lat=d2019[d2019$台風名== "FAXAI"  ,]$緯度  
  rank=(d2019[d2019$台風名== "FAXAI"  ,]$階級%%7)%%6
  mapLines(lon, lat, col='orange', lwd=1)
  mapPoints(lon,lat,col="orange",cex=rank/3,pch=16)
  mapText(139.5,35.1,"Faxai",col="orange",cex=1.3)#Faxai
  mapText(lon, lat, d2019[d2019$台風名== "FAXAI" ,]$Time, col = "orange", pos = 4, offset = 1)
  mapText(lon, lat, d2019[d2019$台風名== "FAXAI" ,]$pressure, col = "orange", pos = 3, offset = 1)}

# Typhoon HAGIBIS in 2019
{
  lon=d2019[d2019$台風名== "HAGIBIS" ,]$経度
  lat=d2019[d2019$台風名== "HAGIBIS"  ,]$緯度  
  rank=(d2019[d2019$台風名== "HAGIBIS"  ,]$階級%%7)%%6
  mapText(138,35.9,"(b)",col="black",cex=1.4)#図の番号
  mapText(139,35.25,"Study site",col="black",cex=1.2)#調査地
  mapPoints(139.14406,35.16138,col="black",cex=1.5,pch=16)#調査地
  mapText(139.2,34.55,"NOWPHAS",col="black",cex=1.2)#NOWPHAS
  mapPoints(138.959,34.675,col="black",cex=1.5,pch=17)#下田NOWPHAS
  mapText(138.8,34.9,"Hagibis",col="red",cex=1.3)#Hagibis
  mapLines(lon, lat, col='red', lwd=1)
  mapPoints(lon,lat,bg="red",col="red",cex=rank/3,pch=16)
  mapText(lon, lat, d2019[d2019$台風名== "HAGIBIS" ,]$Time, col = "red", pos = 2, offset = 1)
  mapText(lon, lat, d2019[d2019$台風名== "HAGIBIS" ,]$pressure, col = "red", pos = 3, offset = 1)}

#Saved as 650*500

png(file="typhoon_tracks2019_0408.png",width=600, height=400, res=300)
svg(file="typhoon_tracks2019_0408.svg",
    width=6, height=4)

# Typhoon LAN  in 2017 not used for Sato et al. (2025)
{lon=d2017[d2017$台風名== "LAN" ,]$経度
  lat=d2017[d2017$台風名== "LAN"  ,]$緯度  
  rank=(d2017[d2017$台風名== "LAN"  ,]$階級%%7)%%6
  mapLines(lon, lat, col='blue', lwd=1)
  mapPoints(lon,lat,col="blue",cex=rank/3,pch=16)
  mapText(lon, lat, d2017[d2017$台風名== "LAN" ,]$Time, col = "blue", pos = 2, offset = 1)
}




# bquote()内では、.(変数)とすることで、変数を受け取ることができる
legend("topleft",legend=c("[上陸したか否か]", #legend title: pt.cex=lwd=0
                          paste("上陸した台風 ::",bquote(.(zy))),
                          "上陸しなかった台風",
                          "[階級]",        ##legend title: pt.cex=lwd=0
                          "温帯低気圧",
                          "熱帯低気圧:最大風速が",
                          "　17m/s未満",
                          "　17m/s以上、25m/s未満（台風）",
                          "　25m/s以上、33m/s未満（台風）",
                          "　33m/s以上（台風）"),
       bty = "o",box.lty =1,box.lwd =1, bg ="white",y.intersp =1.2,
       pch=21,lty=c(0,1,1,0,1,0,rep(1,4)),
       pt.bg="blue",pt.cex=c(rep(0,6),2,3,4,5)/3,pt.lwd=c(0,1,1,0,1,0,rep(1,4)),
       col=c("black","red","blue",rep("black",7)))
title(paste0("台風の進路（",year,"年 台風第13号までのデータ）"))
#showtext_end()
#dev.off()