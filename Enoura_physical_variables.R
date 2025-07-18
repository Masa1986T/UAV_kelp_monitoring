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
names(Enoura_wtemp_mean)
modt<-glm(mean_wtemp~Date,data=Enoura_wtemp_mean)
summary(modt)
write.csv(Enoura_wtemp_mean,"Enoura_wtemp_mean.csv")

#Model
dummy_tempday<- expand.grid(Date=seq(as.Date("2008-01-01"),
                                     as.Date("2020-12-31"),length=4668))
pred_tempday<- predict(modt,newdata=dummy_tempday,se.fit=T)
dummy_tempday$wtemp<-pred_tempday$fit


#Model
Enoura_wtemp <- read.csv("Enoura_wtemp_mean.csv")
modt1<-glm(mean_wtemp~ElapsedDay,data=Enoura_wtemp)
summary(modt1)

dummy_tempday<- expand.grid(ElapsedDay=seq(min(Enoura_wtemp$ElapsedDay),
                                             max(Enoura_wtemp$ElapsedDay),length=10000))
pred_tempday<- predict(modt1,newdata=dummy_tempday,se.fit=T)
dummy_tempday$wtemp<-pred_tempday$fit




plot_timeseries <- ggplot(Enoura_wtemp_mean, aes(Date, mean_wtemp)) +
  geom_point(size=1,na.rm=TRUE)+
  geom_hline(yintercept = 28, linetype = 2, color = "red", linewidth = 1)+
  geom_hline(yintercept = 16, linetype = 2, color = "blue", linewidth = 1)+
  geom_vline(xintercept = as.Date("2019-07-25"), linetype = 2, color = "#619CFF",linewidth = 1)+
  geom_vline(xintercept = as.Date("2019-09-06"), linetype = 2, color = "green",linewidth = 1)+
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = 2, color = "#619CFF", linewidth = 1)+
  geom_vline(xintercept = as.Date("2020-10-28"), linetype = 2, color = "green", linewidth= 1)+
  annotate("text",x = as.Date("2019-07-25"),y=22,label="UVC",color = "#619CFF",size=7,angle =90,vjust= -0.4,hjust=0)+
  annotate("text",x = as.Date("2019-09-06"),y=11,label="UAV",color = "green",size=7,angle =90,vjust= 1.1,hjust=0)+
  annotate("text",x = as.Date("2020-06-01"),y=22,label="UVC",color = "#619CFF",size=7,angle =90,vjust= -0.4,hjust=0)+
  annotate("text",x = as.Date("2020-10-28"),y=11,label="UAV",color = "green",size=7,angle =90,vjust= 1.1,hjust=0)+
  annotate("text",x = as.Date("2008-01-01"),y=28.2,label="(a)",color = "black",size=7,vjust= 0,hjust=0.5)+
  geom_line(data=dummy_tempday, aes(x=Date, y=wtemp),linewidth=1.5,color="black",inherit.aes = FALSE)+
  xlab("Year") + ylab("Water temperature (°C)")+
  theme_bw()+
  theme(axis.title.x = element_blank(),text = element_text(size = 18)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(12, 30, by = 2))
plot_timeseries




# Lethal high temperature for Ecklonia cava 
Enoura_wtemp_28c <- read.csv("Enoura_kelp_28cTemp_hist.csv")#This was used for the final paper


plot_htemp_day<- ggplot(data=Enoura_wtemp_28c , aes(x=Year, y=Day, fill=Temp)) +
  xlab("Year") + ylab("Accumulated time (day)")+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  annotate("text",x = 2016,y=0.3,label="NA",color = "black",size=7,vjust=0 ,hjust=0.5)+
  annotate("text",x = 2007.5,y=13,label="(b)",color = "black",size=7,vjust=0 ,hjust=0.5)+
  scale_x_continuous(breaks = seq(2008, 2020, by = 1))+ 
  theme_classic()+  theme(axis.title.x = element_blank(),text = element_text(size = 18),legend.position = "none") +
  scale_fill_manual(values=c('#999999','black'))
plot_htemp_day

plot_htemp_day<-plot_htemp_day+theme(legend.position.inside = c(0.7, 0.7))

Enoura_wtemp_28c_ex2019<-Enoura_wtemp_28c[-12,]#excluding 2019 data
t.test(Enoura_wtemp_28c_ex2019$Day,mu=4.6736111)#2019

Enoura_wtemp_28c_ex2020<-Enoura_wtemp_28c[-12,] #excluding 2020 data                       
t.test(Enoura_wtemp_28c_ex2020$Day,mu=9.1388889)#2020

# 16 C water temperature
Enoura_wtemp_16C_01<-subset(Enoura_wtemp_16C,Temp=="T>=16C")

plot_16c01_day<- ggplot(data=Enoura_wtemp_16C_01, aes(x=factor(Year), y=Day, fill=Temp)) +
  xlab("Year") + 
  ylab("Accumulated time (day)")+
  geom_bar(stat="identity", color='black')+scale_y_continuous(limits=c(100,180),oob = rescale_none)+
  annotate("text", x="2018-2019", y=102, label="NA",size=7)+
  annotate("text", x="2008-2009", y=175, label="(c)",size=7,vjust=0 ,hjust=0.5)+
  theme_classic()+scale_fill_manual(values=c('#999999'))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),text = element_text(size = 18),legend.position = 'none')
plot_16c01_day

t.test(Enoura_wtemp_16C_01$Day,mu=153.03)#all data

Enoura_wtemp_16C_01ex2019_2020<-Enoura_wtemp_16C_01[-12,] #excluding 2019-2020 data                       
t.test(Enoura_wtemp_16C_01ex2019_2020$Day,mu=153.03)

ylab(expression(atop("Accumulated", paste("time (day)"))))

ggarrange(plot_timeseries, plot_htemp_day,plot_16c01_day,ncol = 1, nrow = 3,heights = c(.3, .3,.4),align = c("v"))##
ggsave("Figure2_Water_temp.jpg", width = 9, height = 12, dpi = 300)
