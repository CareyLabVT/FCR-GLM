#*****************************************************************                                                           *
#* TITLE:   Falling Creek Reservoir Sediment Flux Calculations   *
#* AUTHORS:  C.C. Carey                                          *
#* DATE:   Originally developed 28 May 2021; Last modified 10 Sept 2021                            
#* NOTES:  Goal of this script is to analyze the sediment flux 
#*          chamber data measured for NH4, NO3, SRP, and DOC to 
#*          complement the Fe and Mn data already published in 
#*          Krueger et al. 2020 Water Research. 
#*****************************************************************

library(tidyverse)
library(lubridate)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")

data<-read.csv('./field_data/SedimentChambersFluxes.csv', header=T)

#area of sediment cores is 0.27 m^2, volume is 64.86 L (Kruger et al. 2020 WR)

data1 <- data %>% 
  unite(Exp_Chamber, c(Experiment, Chamber)) %>% 
  mutate(NH4_mmol = NH4_ugL*0.001*(1/18.04)*64.86, #need to convert ug/L to mmol
         NO3_mmol = NO3NO2_ugL*0.001*(1/62.00)*64.86,
         SRP_mmol = SRP_ugL*0.001*(1/94.9714)*64.86,
         DOC_mmol = DOC_mgL*(1/12.01)*64.86) %>% 
  select(Exp_Chamber:Day, NH4_mmol:DOC_mmol)

experiments<-unique(data1$Exp_Chamber)

chamber_no<-rep(NA, length(unique(data1$Exp_Chamber)))
nh4flux<-rep(NA, length(unique(data1$Exp_Chamber)))
no3flux<-rep(NA, length(unique(data1$Exp_Chamber)))
srpflux<-rep(NA, length(unique(data1$Exp_Chamber)))
docflux<-rep(NA, length(unique(data1$Exp_Chamber)))

for(i in 1:length(unique(data1$Exp_Chamber))){
      temp<-subset(data1, data1$Exp_Chamber==experiments[i])
      chamber_no[i] = temp$Exp_Chamber[1]
      nh4flux[i]= as.numeric(lm(temp$NH4_mmol ~ temp$Day)$coefficients[2])
      no3flux[i]= as.numeric(lm(temp$NO3_mmol ~ temp$Day)$coefficients[2])
      srpflux[i]= as.numeric(lm(temp$SRP_mmol ~ temp$Day)$coefficients[2])
      docflux[i]= as.numeric(lm(temp$DOC_mmol ~ temp$Day)$coefficients[2])
    }

newdata<-cbind.data.frame(chamber_no,nh4flux,no3flux,srpflux,docflux, stringsAsFactors=F)

statsreport<- newdata %>% 
  mutate(nh4flux_moles = nh4flux*(1/0.27), #need to correct 
         no3flux_moles = no3flux*(1/0.27),
         srpflux_moles = srpflux*(1/0.27),
         docflux_moles = docflux*(1/0.27)) %>% 
  select(chamber_no,nh4flux_moles:docflux_moles)
  
median(statsreport$nh4flux_moles)
mean(statsreport$nh4flux_moles)
sd(statsreport$nh4flux_moles)/(8^.5)
max(statsreport$nh4flux_moles)

median(statsreport$no3flux_moles)
mean(statsreport$no3flux_moles)
sd(statsreport$no3flux_moles)/(8^.5)
min(statsreport$no3flux_moles)

median(statsreport$srpflux_moles)
mean(statsreport$srpflux_moles)
sd(statsreport$srpflux_moles)/(8^.5)
max(statsreport$srpflux_moles)

median(statsreport$docflux_moles)
mean(statsreport$docflux_moles)
sd(statsreport$docflux_moles)/(8^.5)
max(statsreport$docflux_moles)

