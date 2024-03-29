#*****************************************************************                                                           *
#* TITLE:   Falling Creek Reservoir GLM-AED Oxygenation Scenarios
#*           input file preparation                              *
#* AUTHORS:  C.C. Carey                                          *
#* DATE:   Originally developed 16 July 2020; Last modified 8 Sept 2021                            
#* NOTES:  Goal of this script is to create input file scenarios for oxic vs anoxic 
#*         summer conditions in FCR. CCC subsequently edited on 1 June 2020 
#*         and made tidy, with subsequent tweaks to annotation in summer 2021. 
#*         SSS = side stream supersaturation, another way of describing
#*         the hypolimnetic oxygenation system (HOx)
#*****************************************************************


#load packages
library(zoo)
library(tidyverse)
library(lubridate)


setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
sim_folder <- getwd()

#####baseline scenario: based on observed SSS oxygenation practices
baseline_file <- file.path(sim_folder, 'output/output_2013_2019.nc')

#####anoxic scenario: no SSS activation throughout
#Simply turn off the SSS inflow and outflow in the glm.nml file
anoxic_file <- file.path(sim_folder, 'output/output_anoxic.nc')

####oxic scenario: SSS on in summer May 15-Nov 15 at full level
#take CTD observed temp for SSS inflows (same as for observed)

#pull in SSS operations file 
inflowoxy<-read.csv("inputs/Calc_HOX_flow_DO_OXIC_20200828.csv", header=T) %>%
  #updated this file so that the eductor nozzles increase flow rate by a factor of 4, so 227 LPM = 1135 LPM
  select(time,SSS_m3.day,mmol.O2.m3.day) %>%
  mutate(SSS_m3.day = SSS_m3.day * (1/86400))  %>%
  #convert m3/day to m3/second as needed by GLM
  mutate(SALT = rep(0,length(inflowoxy$time))) %>%
  #add salt column as needed by GLM
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  rename(FLOW = SSS_m3.day, OXY_oxy=mmol.O2.m3.day)

#some diagnostic plots
plot(inflowoxy$time, inflowoxy$FLOW, type = "o")
plot(inflowoxy$time, inflowoxy$OXY_oxy, type = "l", col = "red", ylab="mmol O2/m3/d added per day",
     xlab="Time")

#now, need to inject high levels of O2 with low flow rate
highox <- inflowoxy %>% 
  mutate(load = FLOW*OXY_oxy) %>% 
  mutate(FLOW_lo = rep(1e-08, length(highox$FLOW))) %>% 
  mutate(OXY_oxy_lo = load/FLOW_lo) %>% 
  select(time, FLOW_lo, SALT, OXY_oxy_lo) %>% 
  rename(FLOW = FLOW_lo, OXY_oxy = OXY_oxy_lo) %>% 
  mutate(NIT_amm = rep(0, length(highox$time))) %>% 
  mutate(NIT_nit = rep(0, length(highox$time))) %>% 
  mutate(PHS_frp = rep(0, length(highox$time))) %>% 
  mutate(OGM_doc = rep(0, length(highox$time))) %>% 
  mutate(OGM_docr = rep(0, length(highox$time))) %>% 
  mutate(OGM_poc = rep(0, length(highox$time))) %>% 
  mutate(OGM_don = rep(0, length(highox$time))) %>% 
  mutate(OGM_donr = rep(0, length(highox$time))) %>% 
  mutate(OGM_pon = rep(0, length(highox$time))) %>% 
  mutate(OGM_dop = rep(0, length(highox$time))) %>% 
  mutate(OGM_dopr = rep(0, length(highox$time))) %>% 
  mutate(OGM_pop = rep(0, length(highox$time))) %>% 
  mutate(CAR_dic = rep(0, length(highox$time))) %>% 
  mutate(CAR_ch4 = rep(0, length(highox$time))) %>% 
  mutate(SIL_rsi = rep(0, length(highox$time))) 

#CTD<-read.csv("inputs/CTD_final_2013_2019.csv", header=TRUE) #now need to get temp at 8m for inflow
CTD8 <- CTD %>%
  select(Reservoir:Temp_C) %>%
  filter(Reservoir=="FCR") %>%
  filter(Site==50) %>%
  rename(time=Date, TEMP=Temp_C) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth_m = round(Depth_m, digits=0)) %>%
  group_by(time) %>% 
  filter(Depth_m==8) %>%
  summarise(TEMP=mean(TEMP)) %>%
  filter(TEMP<17) #remove crazy outlier from 2014

#diagnostic plot to check for 8m water temp
plot(CTD8$time, CTD8$TEMP, type = "o")

#make final SSS inflow file, setting Dec 31 of each year to 4oC in lieu of CTD data for interpolation
SSS_inflowALL<-merge(highox,CTD8, by="time",all.x=TRUE)
SSS_inflowALL$TEMP[231]<-4
SSS_inflowALL$TEMP[596]<-4
SSS_inflowALL$TEMP[961]<-4
SSS_inflowALL$TEMP[1326]<-4
SSS_inflowALL$TEMP[1691]<-4
SSS_inflowALL$TEMP[2056]<-4
SSS_inflowALL$TEMP[2422]<-4 #set last row as 4oC in prep for freezing
SSS_inflowALL$TEMP<-na.fill(na.approx(SSS_inflowALL$TEMP),"extend")
#SSS_inflowALL$CAR_ch4 <-na.fill(na.approx(SSS_inflowALL$CAR_ch4), "extend")
plot(SSS_inflowALL$time, SSS_inflowALL$TEMP, type = "o")

#get everything in order
SSS_inflowALL<-SSS_inflowALL %>%
  select(time, FLOW, TEMP, SALT:SIL_rsi) #get all of the columns in order

SSS_inflowALL[which(duplicated(SSS_inflowALL$time)),] #identify if there are repeated dates

SSS_inflowALL <- SSS_inflowALL[(!duplicated(SSS_inflowALL$time)),] #remove repeated dates

#et voila! the final inflow file for the SSS for 2 pools of DOC
write.csv(SSS_inflowALL, "inputs/FCR_SSS_inflow_2013_2019_20200829_OXIC_allfractions_2DOCpools.csv", row.names = FALSE)
#this is the file that needs to be substituted into the glm.nml file to make it run for the oxic scenario!

#run the model with the oxic SSS driver file, then
oxic_file <- file.path(sim_folder, 'output/output_oxic.nc')