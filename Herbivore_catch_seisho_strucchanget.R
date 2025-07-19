library(strucchange)
rm(list=ls())

herbivore<-read.csv("Herbivore_setnet1.csv")

herbivore$Species <- factor(herbivore$Species, 
                            levels = c("Calotomus japonicus", "Siganus fuscescens",
                                       "Prionurus scalprum", "Girella punctata") )
herbivore_2sp<-subset(herbivore,Species=="Calotomus japonicus" | Species=="Siganus fuscescens")



#Siganus change points #
herbivore_aigo<-subset(herbivore, Species=="Siganus fuscescens")
plot(Catch~Year,data=herbivore_aigo)

aigo_data <- ts(herbivore_aigo$Catch,start=2000,end=2020,frequency = 1)
lin.trend <- 1:length(aigo_data)            #Trend variable
aigo_ts <- breakpoints(aigo_data ~ lin.trend) #Estimating change points
plot(aigo_ts)
summary(aigo_ts)
coef(aigo_ts)                                #Intercept and coefficients
aigofm <- lm(aigo_data ~ breakfactor(Aigo_ts)/(lin.trend)-1) #Estimates
aigoci_ts <- confint(aigo_ts)
plot(aigo_data) 
lines(aigo_ts) 
lines(aigoci_ts) 
lines(ts(fitted(aigofm), start = 2000), col = 4) #Describing correlated lines (blue)

#Calotomus change points #
herbivore_budai<-subset(herbivore, Species=="Calotomus japonicus")
plot(Catch~Year,data=herbivore_budai)

budai_data <- ts(herbivore_budai$Catch,start=2000,end=2020,frequency = 1)
lin.trend <- 1:length(budai_data)            #Trend variable
budai_ts <- breakpoints(budai_data ~ lin.trend) #Estimating change points
plot(budai_ts)
summary(budai_ts)
coef(budai_ts)
budaifm <- lm(budai_data ~ breakfactor(budai_ts)/(lin.trend)-1) #Estimates
budaifm
budaici_ts <- confint(budai_ts)
plot(budai_data) 
lines(budai_ts) 
lines(budaici_ts) 
lines(ts(fitted(budaifm), start = 2000), col = 4) #Describing correlated lines (blue)



### Test using data "Nile" #####
### Not related to Sato et al. (2025)###

data("Nile") 
plot(Nile)
## F statistics indicate one breakpoint 
fs.nile <- Fstats(Nile ~ 1) 
plot(fs.nile) 
breakpoints(fs.nile) 
lines(breakpoints(fs.nile))

fm0 <- lm(Nile ~ 1) 
fm1 <- lm(Nile ~ breakfactor(bp.nile, breaks = 1)) 
plot(Nile) 
lines(ts(fitted(fm0), start = 1871), col = 3) 
lines(ts(fitted(fm1), start = 1871), col = 4) 
lines(bp.nile)

## or 
bp.nile <- breakpoints(Nile ~ 1)
summary(bp.nile)

## the BIC also chooses one breakpoint 
plot(bp.nile) 
breakpoints(bp.nile)

## fit null hypothesis model and model with 1 breakpoint 
fm0 <- lm(Nile ~ 1) 
fm1 <- lm(Nile ~ breakfactor(bp.nile, breaks = 1)) 
plot(Nile) 
lines(ts(fitted(fm0), start = 1871), col = 3) 
lines(ts(fitted(fm1), start = 1871), col = 4) 
lines(bp.nile)

ci.nile <- confint(bp.nile) 
ci.nile 
lines(ci.nile)
