# Remember it is good coding technique to add additional packages to the top of
# your script 
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(dplyr)
library(ggpubr)
library(ggbreak)
rm(list=ls())

#2008-2020 using 2hours interval data
Shimoda_wave2008_2020<- read.csv("Shimoda_wave_2008-2020.csv")
Shimoda_wave2008_2020$DateTime<-as.POSIXct(Shimoda_wave2008_2020$DateTime, tz="Asia/Tokyo")

# Line plot
plot_Shimoda_wave_2008_2020<-ggplot(Shimoda_wave2008_2020, aes(DateTime, Significant_wave))+
  xlab("Year") + ylab("Significant wave height (m)")+
  geom_line(aes(x=DateTime, y=Significant_wave),linewidth =1)+
  geom_vline(xintercept = as.POSIXct("2019-07-25"), linetype = 2, color = "#619CFF", linewidth = 1)+
  geom_vline(xintercept = as.POSIXct("2019-09-06"), linetype = 2, color = "green", linewidth = 1)+
  geom_vline(xintercept = as.POSIXct("2020-06-01"), linetype = 2, color = "#619CFF", linewidth = 1)+
  geom_vline(xintercept = as.POSIXct("2020-10-28"), linetype = 2, color = "green", linewidth = 1)+
  annotate("text",x = as.POSIXct("2019-07-25"),y=5,label="UVC",color = "#619CFF",size=6,angle =90,vjust= -0.4,hjust=0)+
  annotate("text",x = as.POSIXct("2019-09-06"),y=8.9,label="UAV",color = "green",size=6,angle =90,vjust= 1.2,hjust=0)+
  annotate("text",x = as.POSIXct("2020-06-01"),y=5,label="UVC",color = "#619CFF",size=6,angle =90,vjust= -0.3,hjust=0)+
  annotate("text",x = as.POSIXct("2020-10-28"),y=8.9,label="UAV",color = "green",size=6,angle =90,vjust= 1.2,hjust=0)+
  annotate("text",x = as.POSIXct("2008-01-01"),y=9.7,label="(a)",color = "black",size=8.5,vjust= 0,hjust=0.5)+
  theme_bw()+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(),text = element_text(size = 18)) 

plot_Shimoda_wave_2008_2020
ggsave("plot_Shimoda_wave_2008_2020_0408_1.png", width = 8, height = 4, dpi = 300)

# Point plot
plot_Shimoda_wave_2008_2020<-ggplot(Shimoda_wave2008_2020, aes(DateTime, Significant_wave))+
  xlab("Year") + ylab("Significant wave height (m)")+
  geom_point(aes(x=DateTime, y=Significant_wave))+
  geom_point(xintercept = as.POSIXct("2019-10-12 18:00:00"), yintercept = 8.77, color = "Red")+
  geom_vline(xintercept = as.POSIXct("2019-07-25"), linetype = 2, color = "#619CFF", linewidth = 1)+
  geom_vline(xintercept = as.POSIXct("2019-09-06"), linetype = 2, color = "green", linewidth = 1)+
  geom_vline(xintercept = as.POSIXct("2020-06-01"), linetype = 2, color = "#619CFF", linewidth = 1)+
  geom_vline(xintercept = as.POSIXct("2020-10-28"), linetype = 2, color = "green", linewidth = 1)+
  annotate("text",x = as.POSIXct("2019-07-25"),y=5,label="UVC",color = "#619CFF",size=6,angle =90,vjust= -0.4,hjust=0)+
  annotate("text",x = as.POSIXct("2019-09-06"),y=8.9,label="UAV",color = "green",size=6,angle =90,vjust= 1.2,hjust=0)+
  annotate("text",x = as.POSIXct("2020-06-01"),y=5,label="UVC",color = "#619CFF",size=6,angle =90,vjust= -0.3,hjust=0)+
  annotate("text",x = as.POSIXct("2020-10-28"),y=8.9,label="UAV",color = "green",size=6,angle =90,vjust= 1.2,hjust=0)+
  annotate("text",x = as.POSIXct("2008-01-01"),y=9.7,label="(a)",color = "black",size=8.5,vjust= 0,hjust=0.5)+
  theme_bw()+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(),text = element_text(size = 18)) 

plot_Shimoda_wave_2008_2020


ggsave("pointplot_Shimoda_wave_2008_2020_0408.png", width = 8, height = 4, dpi = 300)
svg("pointplot_Shimoda_wave_2008_2020_0408.svg", width = 8, height = 4)#

#svgは時間がかかる
svg("plot_Shimoda_wave_2008_2020_0408.svg", width = 8, height = 4)#

cowplot::plot_grid(plot_Shimoda_wave_2008_2020,    # (1, 1)
                   typhoonmap,                    # (2, 1)
                   align = "h",
                   rel_heights = c(.4, .6),
                   labels = c("a","b"), label_x = .1, label_y = .95) 

ggpubr::ggarrange(plot_Shimoda_wave_2008_2020, typhoonmap,ncol = 1, nrow = 2, heights = c(.4,.6),labels = LETTERS,align = c("v"))


#Maximum significant wave height 
Maxwave2008_2020<- read.csv("max_sigwave_2008_2020.csv")

t.test (Maxwave2008_2020$Max_sigwave_height,mu=8.77)


###boxplot##
Shimoda_wave2008_2020$Year<-factor(Shimoda_wave2008_2020$Year,
                                   levels = c(2008,2009,2010,2011,
                                              2012,2013,2014,2015,
                                        2016,2017,2018,2019,2020))
  
Shimoda_wave2008_2020$Year<-factor(Shimoda_wave2008_2020$Year)
Shimoda_wave2008_2020$Significant_wave

wave_boxplot<- ggplot(Shimoda_wave2008_2020, aes(x = Year,y=Significant_wave,fill=Year))+
  ylab("Significant wave height (m)") + xlab("Year")+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  theme(axis.title.x = element_blank(),text = element_text(size = 14),legend.position = "none")
wave_boxplot


###histgram##
#年間の比較…年ごとのヒストグラムを同じグラフ内で表示
wave_boxplot<- ggplot(Shimoda_wave2008_2020, aes(x = Significant_wave,fill=factor(Year)))+
  xlab("Year") + ylab("Significant wave height (m)")+
  geom_histogram(position="identity", alpha=0.6)
wave_hist1

wave_hist<-wave_hist+
  geom_vline(xintercept = 8.77, linetype = 2, color = "red", linewidth = .5)+
  geom_vline(xintercept = 4.34, linetype = 2, color = "orange", linewidth = .5)
wave_hist
plot(g)


#年間の比較…年ごとのるヒストグラムを異なるグラフで表示
wave_hist2<- ggplot(Shimoda_wave2008_2020, aes(x = Significant_wave))+
  xlab("Significant wave height (m)") + ylab("Count")+
  geom_histogram(position="identity", alpha=0.6)+
  facet_grid(factor(Year) ~ .)
wave_hist2


Shimoda_wave2008<-subset(Shimoda_wave2008_2020,Year==2008)
Shimoda_wave2009<-subset(Shimoda_wave2008_2020,Year==2009)
Shimoda_wave2010<-subset(Shimoda_wave2008_2020,Year==2010)
Shimoda_wave2011<-subset(Shimoda_wave2008_2020,Year==2011)
Shimoda_wave2012<-subset(Shimoda_wave2008_2020,Year==2012)
Shimoda_wave2013<-subset(Shimoda_wave2008_2020,Year==2013)
Shimoda_wave2014<-subset(Shimoda_wave2008_2020,Year==2014)
Shimoda_wave2015<-subset(Shimoda_wave2008_2020,Year==2015)
Shimoda_wave2016<-subset(Shimoda_wave2008_2020,Year==2016)
Shimoda_wave2017<-subset(Shimoda_wave2008_2020,Year==2017)
Shimoda_wave2018<-subset(Shimoda_wave2008_2020,Year==2018)
Shimoda_wave2019<-subset(Shimoda_wave2008_2020,Year==2019)
Shimoda_wave2020<-subset(Shimoda_wave2008_2020,Year==2020)

#Outliner extraction
q3_2008 <- quantile(Shimoda_wave2008$Significant_wave, 0.75,na.rm=TRUE)#2008
iqr2008 <- IQR(Shimoda_wave2008$Significant_wave,na.rm=TRUE)#2008
upper_bound2008 <- q3_2008 + 1.5*iqr2008#1.665
outliers_upper2008 <- Shimoda_wave2008$Significant_wave[Shimoda_wave2008$Significant_wave > upper_bound2008]
dat2008 <- data.frame(Year = 2008, Significant_wave = outliers_upper2008)

q3_2009 <- quantile(Shimoda_wave2009$Significant_wave, 0.75,na.rm=TRUE)#2009
iqr2009 <- IQR(Shimoda_wave2009$Significant_wave,na.rm=TRUE)#2009
upper_bound2009 <- q3_2009 + 1.5*iqr2009#
outliers_upper2009 <- Shimoda_wave2009$Significant_wave[Shimoda_wave2009$Significant_wave > upper_bound2009]
dat2009 <- data.frame(Year = 2009, Significant_wave = outliers_upper2009)

q3_2010 <- quantile(Shimoda_wave2010$Significant_wave, 0.75,na.rm=TRUE)#2010
iqr2010 <- IQR(Shimoda_wave2010$Significant_wave,na.rm=TRUE)#2010
upper_bound2010 <- q3_2010 + 1.5*iqr2010#
outliers_upper2010 <- Shimoda_wave2010$Significant_wave[Shimoda_wave2010$Significant_wave > upper_bound2010]
dat2010 <- data.frame(Year = 2010, Significant_wave = outliers_upper2010)

q3_2011 <- quantile(Shimoda_wave2011$Significant_wave, 0.75,na.rm=TRUE)#2011
iqr2011 <- IQR(Shimoda_wave2011$Significant_wave,na.rm=TRUE)#2011
upper_bound2011 <- q3_2011 + 1.5*iqr2011#
outliers_upper2011 <- Shimoda_wave2011$Significant_wave[Shimoda_wave2011$Significant_wave > upper_bound2011]
dat2011 <- data.frame(Year = 2011, Significant_wave = outliers_upper2011)

q3_2012 <- quantile(Shimoda_wave2012$Significant_wave, 0.75,na.rm=TRUE)#2012
iqr2012 <- IQR(Shimoda_wave2012$Significant_wave,na.rm=TRUE)#2012
upper_bound2012 <- q3_2012 + 1.5*iqr2012#
outliers_upper2012 <- Shimoda_wave2012$Significant_wave[Shimoda_wave2012$Significant_wave > upper_bound2012]
dat2012 <- data.frame(Year = 2012, Significant_wave = outliers_upper2012)

q3_2013 <- quantile(Shimoda_wave2013$Significant_wave, 0.75,na.rm=TRUE)#2013
iqr2013 <- IQR(Shimoda_wave2013$Significant_wave,na.rm=TRUE)#2013
upper_bound2013 <- q3_2013 + 1.5*iqr2013#
outliers_upper2013 <- Shimoda_wave2013$Significant_wave[Shimoda_wave2013$Significant_wave > upper_bound2013]
dat2013 <- data.frame(Year = 2013, Significant_wave = outliers_upper2013)

q3_2014 <- quantile(Shimoda_wave2014$Significant_wave, 0.75,na.rm=TRUE)#2014
iqr2014 <- IQR(Shimoda_wave2014$Significant_wave,na.rm=TRUE)#2014
upper_bound2014 <- q3_2014 + 1.5*iqr2014#
outliers_upper2014 <- Shimoda_wave2014$Significant_wave[Shimoda_wave2014$Significant_wave > upper_bound2014]
dat2014 <- data.frame(Year = 2014, Significant_wave = outliers_upper2014)

q3_2015 <- quantile(Shimoda_wave2015$Significant_wave, 0.75,na.rm=TRUE)#2015
iqr2015 <- IQR(Shimoda_wave2015$Significant_wave,na.rm=TRUE)#2015
upper_bound2015 <- q3_2015 + 1.5*iqr2015#
outliers_upper2015 <- Shimoda_wave2015$Significant_wave[Shimoda_wave2015$Significant_wave > upper_bound2015]
dat2015 <- data.frame(Year = 2015, Significant_wave = outliers_upper2015)

q3_2016 <- quantile(Shimoda_wave2016$Significant_wave, 0.75,na.rm=TRUE)#2016
iqr2016 <- IQR(Shimoda_wave2016$Significant_wave,na.rm=TRUE)#2016
upper_bound2016 <- q3_2016 + 1.5*iqr2016#
outliers_upper2016 <- Shimoda_wave2016$Significant_wave[Shimoda_wave2016$Significant_wave > upper_bound2016]
dat2016 <- data.frame(Year = 2016, Significant_wave = outliers_upper2016)

q3_2017 <- quantile(Shimoda_wave2016$Significant_wave, 0.75,na.rm=TRUE)#2016
iqr2017 <- IQR(Shimoda_wave2017$Significant_wave,na.rm=TRUE)#2017
upper_bound2017 <- q3_2017 + 1.5*iqr2017#
outliers_upper2017 <- Shimoda_wave2017$Significant_wave[Shimoda_wave2017$Significant_wave > upper_bound2017]
dat2017 <- data.frame(Year = 2017, Significant_wave = outliers_upper2017)

q3_2018 <- quantile(Shimoda_wave2016$Significant_wave, 0.75,na.rm=TRUE)#2016
iqr2018 <- IQR(Shimoda_wave2018$Significant_wave,na.rm=TRUE)#2018
upper_bound2018 <- q3_2018 + 1.5*iqr2018#
outliers_upper2018 <- Shimoda_wave2018$Significant_wave[Shimoda_wave2018$Significant_wave > upper_bound2018]
dat2018 <- data.frame(Year = 2018, Significant_wave = outliers_upper2018)

q3_2019 <- quantile(Shimoda_wave2016$Significant_wave, 0.75,na.rm=TRUE)#2016
iqr2019 <- IQR(Shimoda_wave2019$Significant_wave,na.rm=TRUE)#2019
upper_bound2019 <- q3_2019 + 1.5*iqr2019#
outliers_upper2019 <- Shimoda_wave2019$Significant_wave[Shimoda_wave2019$Significant_wave > upper_bound2019]
dat2019 <- data.frame(Year = 2019, Significant_wave = outliers_upper2019)

q3_2020 <- quantile(Shimoda_wave2016$Significant_wave, 0.75,na.rm=TRUE)#2016
iqr2020 <- IQR(Shimoda_wave2020$Significant_wave,na.rm=TRUE)#2020
upper_bound2020 <- q3_2020 + 1.5*iqr2020#
outliers_upper2020 <- Shimoda_wave2020$Significant_wave[Shimoda_wave2020$Significant_wave > upper_bound2020]
dat2020 <- data.frame(Year = 2020, Significant_wave = outliers_upper2020)

outliner2008_2020<-rbind(dat2008,dat2009,dat2010,dat2011,dat2012,dat2013,dat2014,
                          dat2015,dat2016,dat2017,dat2018,dat2019,dat2020)
write.csv(outliner2008_2020,"sigwave_outliner2008_2020.csv")

#
outliner2008_2020<- na.omit(outliner2008_2020)
outliner2008_2020$Year<-factor(outliner2008_2020$Year)

outliner_wave_boxplot<- ggplot(outliner2008_2020, 
                               aes(x = Year,y=Significant_wave,fill=Year))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))+
  ylab("Significant wave height (m)") + xlab("Year")+
  theme(text = element_text(size = 14),legend.position = "none")
outliner_wave_boxplot

outliner2008_2020_ex2019<-subset(outliner2008_2020,Year!=2019)

t.test (dat2019$Significant_wave,outliner2008_2020_ex2019$Significant_wave)

outliner_comp<-glm(Significant_wave~Year,data=outliner2008_2020) 

#Mean and variance for outliner
library(dplyr)
group_year_mean_sd <- outliner2008_2020 %>%
  group_by(Year) %>%
  summarize(mean=mean(Significant_wave, na.rm=T), sd=sd(Significant_wave, na.rm=T), n=n(), se=sd/sqrt(n))
group_year_mean_sd

ggplot(data=group_year_mean_sd, aes(x = Year, y = mean, group = Year, color = Year)) +
  labs(x = "Year", y = "Significant wave height (m)") +
  geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), width = 0.2) +
  geom_point(aes(color = Year), size = 4)

# Shimoda wave data 10 minutes interval
Shimoda_wave <- read.csv("Shimoda_wave_2019_2020.csv")
Shimoda_wave$DateTime<-as.POSIXct(Shimoda_wave$DateTime, tz="Asia/Tokyo")




# plot wave Data 
# across 2019-2020 using 2hours interval data
Shimoda_wave2019_2020<-ggplot(Shimoda_wave, aes(DateTime, Significant_wave)) +
  xlab("Month") + ylab("Significant wave height (m)")+
  geom_line(aes(x=DateTime, y=Significant_wave),linewidth=1)+
  geom_vline(xintercept = as.POSIXct("2019-07-25"), linetype = 2, color = "blue", linewidth = 0.5)+
  geom_vline(xintercept = as.POSIXct("2019-09-06"), linetype = 2, color = "green", linewidth = 0.5)+
  geom_vline(xintercept = as.POSIXct("2020-06-01"), linetype = 2, color = "blue", linewidth = 0.5)+
  geom_vline(xintercept = as.POSIXct("2020-10-28"), linetype = 2, color = "green", linewidth = 0.5)+
  geom_text(mapping=aes(x = as.POSIXct("2019-07-25"),y=11,label="Underwater visual census"),color = "blue",size=5,angle =90,vjust= -0.5,hjust=1)+
  geom_text(mapping=aes(x = as.POSIXct("2019-09-06"),y=11,label="UAV photo"),color = "green",size=5,angle =90,vjust= -0.5,hjust=1)+
  geom_text(mapping=aes(x = as.POSIXct("2020-06-01"),y=11,label="Underwater visual census"),color = "blue",size=5,angle =90,vjust= -0.5,hjust=1)+
  geom_text(mapping=aes(x = as.POSIXct("2020-10-28"),y=11,label="UAV photo"),color = "green",size=5,angle =90,vjust= -0.5,hjust=1)+  
  scale_x_datetime(date_breaks = "2 month",  date_labels="%m-%Y") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),text = element_text(size = 16)) 

Shimoda_wave2019_2020


# plot current velocity Data across 2019-2020 using 10 minutes interval data
Enoura_current2019_2020<-ggplot(Enoura_current, aes(DateTime, Vel)) +xlab("Month") + ylab("Current velocity (m/s)")+
  geom_line(aes(x=DateTime, y=Vel, group = 1),size=1)+
  geom_vline(xintercept = as.POSIXct("2019-09-06"), linetype = 2, color = "blue", size = 0.5)+
  geom_vline(xintercept = as.POSIXct("2020-10-28"), linetype = 2, color = "blue", size = 0.5)+
  scale_x_datetime(date_breaks = "1 month",  date_labels="%m-%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),text = element_text(size = 20))
Enoura_current2019_2020

# Summarizing data by daily temperature using dplyr 
Enoura_wtemp_mean <-
  Enoura_wtemp %>%
  group_by(Date) %>%
  summarize(mean_wtemp = mean(WaterTemp, na.rm = TRUE))


time.chr <- as.character(seq(as.POSIXct("2022-09-20 00:00:00"),as.POSIXct("2022-09-30 00:00:00"),"1 sec"))
system.time(time <- as.POSIXct(time.chr))
