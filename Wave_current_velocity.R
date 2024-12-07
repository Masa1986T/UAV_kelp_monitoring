# Remember it is good coding technique to add additional packages to the top of
# your script 
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(dplyr)
library(ggpubr)

rm(list=ls())

# across 2008-2020 using 2hours interval data
Shimoda_wave2008_2020<- read.csv("Shimoda_wave_2008-2020.csv")
Shimoda_wave2008_2020$DateTime<-as.POSIXct(Shimoda_wave2008_2020$DateTime, tz="Asia/Tokyo")


plot_Shimoda_wave_2008_2020<-ggplot(Shimoda_wave2008_2020, aes(DateTime, Significant_wave))+
  xlab("Year") + ylab("Significant wave height (m)")+
  geom_line(aes(x=DateTime, y=Significant_wave),linewidth =1)+
  geom_vline(xintercept = as.POSIXct("2019-07-25"), linetype = 2, color = "blue", linewidth = 0.5)+
  geom_vline(xintercept = as.POSIXct("2019-09-06"), linetype = 2, color = "green", linewidth = 0.5)+
  geom_vline(xintercept = as.POSIXct("2020-06-01"), linetype = 2, color = "blue", linewidth = 0.5)+
  geom_vline(xintercept = as.POSIXct("2020-10-28"), linetype = 2, color = "green", linewidth = 0.5)+
  geom_text(mapping=aes(x = as.POSIXct("2019-07-25"),y=0,label="UVC"),color = "blue",size=4,angle =90,vjust= -0.3,hjust=0)+
  geom_text(mapping=aes(x = as.POSIXct("2019-09-06"),y=0,label="UAV photo"),color = "green",size=4,angle =90,vjust= 0.8,hjust=0)+
  geom_text(mapping=aes(x = as.POSIXct("2020-06-01"),y=0,label="UVC"),color = "blue",size=4,angle =90,vjust= -0.2,hjust=0)+
  geom_text(mapping=aes(x = as.POSIXct("2020-10-28"),y=0,label="UAV photo"),color = "green",size=4,angle =90,vjust= 0.9,hjust=0)+  
  theme_bw()+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(),text = element_text(size = 18)) 

plot_Shimoda_wave_2008_2020

cowplot::plot_grid(plot_Shimoda_wave_2008_2020,    # (1, 1)
                   typhoonmap,                    # (2, 1)
                   align = "h",
                   rel_heights = c(.4, .6),
                   labels = c("a","b"), label_x = .1, label_y = .95) 

ggpubr::ggarrange(plot_Shimoda_wave_2008_2020, typhoonmap, labels = LETTERS)

ggsave("plot_Shimoda_wave_2008_2020.png", width = 8, height = 4, dpi = 300)




