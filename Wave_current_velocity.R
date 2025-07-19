# Remember it is good coding technique to add additional packages to the top of
# your script 
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(ggpubr)
getwd()
setwd("~/data")

#2008-2020 using 2hours interval data
Shimoda_wave2008_2020<- read.csv("Shimoda_wave_2008-2020.csv")
Shimoda_wave2008_2020$DateTime<-as.POSIXct(Shimoda_wave2008_2020$DateTime, tz="Asia/Tokyo")

# Line plot used for Sato et al. (2025)
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

# Point plot not used for Sato et al. (2025)
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

#Making svg plot, wichi takes time
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








