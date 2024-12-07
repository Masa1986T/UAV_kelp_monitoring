# Remember it is good coding technique to add additional packages to the top of
# your script 
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(dplyr)
library(ggpubr)


# Enoura water temperature data by 10 minutes interval
Enoura_wtemp <- read.csv("Enoura_wtemp5m_2008-2020.csv")
Enoura_wtemp$Date <- as.Date(Enoura_wtemp$DateTime)

# plot Air Temperature Data across 2009-2011 using daily data
ggplot(Enoura_wtemp, aes(Date, WaterTemp)) +xlab("Year") + ylab("Water Temperature (C)")+
  geom_point(na.rm=TRUE)

ggplot(Enoura_wtemp, aes(DateTime, WaterTemp)) +xlab("Year") + ylab("Water Temperature (C)")+
  geom_point(na.rm=TRUE)

# Summarizing data by daily temperature using dplyr 
Enoura_wtemp_mean <-
  Enoura_wtemp %>%
  group_by(Date) %>%
  summarize(mean_wtemp = mean(WaterTemp, na.rm = TRUE))

plot_timeseries <- ggplot(Enoura_wtemp_mean, aes(Date, mean_wtemp)) +
  geom_hline(yintercept = 28, linetype = 2, color = "red", size = 0.5)+
  geom_vline(xintercept = as.Date("2019-07-25"), linetype = 2, color = "blue",linewidth = 0.5)+
  geom_vline(xintercept = as.Date("2019-09-06"), linetype = 2, color = "green",linewidth = 0.5)+
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = 2, color = "blue", linewidth = 0.5)+
  geom_vline(xintercept = as.Date("2020-10-28"), linetype = 2, color = "green", linewidth= 0.5)+
  geom_text(mapping=aes(x = as.Date("2019-07-25"),y=11,label="UVC"),color = "blue",size=5,angle =90,vjust= -0.4,hjust=0)+
  geom_text(mapping=aes(x = as.Date("2019-09-06"),y=11,label="UAV photo"),color = "green",size=5,angle =90,vjust= 0.8,hjust=0)+
  geom_text(mapping=aes(x = as.Date("2020-06-01"),y=11,label="UVC"),color = "blue",size=5,angle =90,vjust= -0.4,hjust=0)+
  geom_text(mapping=aes(x = as.Date("2020-10-28"),y=11,label="UAV photo"),color = "green",size=5,angle =90,vjust= 0.8,hjust=0)+
  geom_point(size=1,na.rm=TRUE)+
  xlab("Year") + ylab("Water temperature (°C)")+
  theme_bw()+
  theme(axis.title.x = element_blank(),text = element_text(size = 18)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(12, 30, by = 2))
plot_timeseries

# Max water temperature
Enoura_wtemp_max <- read.csv("Enoura_maxTemp_hist.csv")


plot_htemp_day<- ggplot(data=Enoura_wtemp_max, aes(x=Year, y=Day, fill=Temp)) +
  xlab("Year") + ylab("Accumulated time (day)")+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  geom_text(mapping=aes(x = 2016,y=0,label="NA"),size=5,angle =0,vjust=-0.5,hjust=0.5)+
  scale_x_continuous(breaks = seq(2008, 2020, by = 1))+ 
  theme_classic()+  theme(axis.title.x = element_blank(),text = element_text(size = 18)) +
  scale_fill_manual(values=c('#999999','black'))
plot_htemp_day<-plot_htemp_day+theme(legend.position = c(0.8, 0.8))
plot_htemp_day

# 16 C water temperature
Enoura_wtemp_16C <- read.csv("Enoura_16cTemp_hist.csv")

plot_16c_day<- ggplot(data=Enoura_wtemp_16C, aes(x=factor(Year), y=Day, fill=Temp)) +
  xlab("Year") + ylab("Accumulated time (day)")+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_classic()+scale_fill_manual(values=c('#999999','black'))
plot_16c_day<-plot_16c_day+theme(legend.position = c(0.8, 0.8))
plot_16c_day

Enoura_wtemp_16C_01<-subset(Enoura_wtemp_16C,Temp=="T>=16C")

plot_16c01_day<- ggplot(data=Enoura_wtemp_16C_01, aes(x=factor(Year), y=Day, fill=Temp)) +
  xlab("Year") + 
  ylab("Accumulated time (day)")+
  geom_bar(stat="identity", color='black')+scale_y_continuous(limits=c(100,180),oob = rescale_none)+
  annotate("text", x="2018-2019", y=100, label="NA",size=5,)+
  theme_classic()+scale_fill_manual(values=c('#999999'))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),text = element_text(size = 18),legend.position = 'none')
plot_16c01_day

ylab(expression(atop("Accumulated", paste("time (day)"))))

ggarrange(plot_timeseries, plot_htemp_day,plot_16c01_day,ncol = 1, nrow = 3)##
ggsave("Figure2_Water_temp.tif", width = 7.5, height = 10, dpi = 300)
