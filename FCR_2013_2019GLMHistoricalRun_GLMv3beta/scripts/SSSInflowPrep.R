#written by CCC on 16 July 2018 to develop SSS inflow file for FCR GLM
#updated 1 June 2020 to become "tidy" and update inflow nutrient fractions for 2013-2019

setwd("../inputs")

#load packages
library(zoo)
library(tidyverse)
library(lubridate)


#pull in FCR chem data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/6/2b3dc84ae6b12d10bd5485f1c300af13" 
infile1 <- paste0(getwd(),"/chem.csv")
download.file(inUrl1,infile1,method="curl")

FCRchem <- read.csv("chem.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  filter(Reservoir=="FCR") %>%
  filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time = DateTime) %>%
  select(time:DIC_mgL)

#pull in SSS file (currently not on EDI)
inflowoxy<-read.csv("Calc_HOX_flow_DO_20190916.csv", header=T) %>%
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

#read in lab dataset of dissolved silica, measured by Jon in summer 2014 only
silica <- read.csv("FCR2014_Chemistry.csv", header=T) %>%
  select(Date, Depth, DRSI_mgL) %>%
  mutate(Date = as.POSIXct(strptime(Date, "%Y-%m-%d", tz="EST"))) %>%
  filter(Depth == 9) %>% #9 = only hypolimnetic depth at which Si was measured
  select(Date, DRSI_mgL) %>%
  rename(time = Date)

#read in lab dataset of GHGs, measured 2015-2019
ghg <- read.csv("Dissolved_GHG_data_FCR_BVR_site50_inf_wet_15_19_not_final.csv", header=T) %>%
  filter(Reservoir == "FCR") %>%
  filter(Depth_m==8) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  group_by(DateTime, Depth_m) %>% 
  summarise(ch4_umolL=mean(ch4_umolL)) %>%
  rename(CAR_ch4 = ch4_umolL, time=DateTime) %>%
  select(time, CAR_ch4)
plot(ghg$time, ghg$CAR_ch4, ylim=c(0,20))

datelist2<-seq.Date(as.Date(first(ghg$time)),as.Date(last(ghg$time)), "days")
datelist2<-as.data.frame(datelist2)
colnames(datelist2)=c("time")
datelist2$time<-as.POSIXct(strptime(datelist2$time, "%Y-%m-%d", tz="EST"))

ghg1 <- merge(datelist2, ghg, by="time", all.x=TRUE) 
ghg1$CAR_ch4 <- na.fill(na.approx(ghg1$CAR_ch4), "extend")
plot(ghg1$time, ghg1$CAR_ch4) 

#diagnostic plot of silica
plot(silica$time, silica$DRSI_mgL)
hist(silica$DRSI_mgL)
median(silica$DRSI_mgL)#this is going to be used to set the SSS Si inflow data in lieu of other years of data

#need to merge the inflow OXY file with 8m chemistry to create SSS inflows
FCRchem_trun <- FCRchem %>%
  filter(Depth_m==8)

#merge observed chem at 8m with SSS to develop SSS submerged inflow with chem
alldata<-merge(inflowoxy, FCRchem_trun, by="time", all.x=TRUE)

#some cool long-term plots of 8m chemistry
plot(alldata$time, alldata$SRP_ugL)
plot(alldata$time, alldata$DOC_mgL)
plot(alldata$time, alldata$NO3NO2_ugL)
plot(alldata$time, alldata$NH4_ugL)
plot(alldata$time, alldata$TN_ugL)
plot(alldata$time, alldata$TP_ugL)
plot(alldata$time, alldata$DIC_mgL)

#remove high DOC outlier
alldata$DOC_mgL[which(alldata$DOC_mgL>10)] <-NA

#now need to interpolate missing values in chem; setting 1st and last value in time series as medians
  #then linearly interpolating the middle missing values
alldata$TN_ugL[1]<-median(na.exclude(alldata$TN_ugL))
alldata$TN_ugL[2424]<-median(na.exclude(alldata$TN_ugL)) #2424 is last observation in time series- edit this!
alldata$TN_ugL<-na.fill(na.approx(alldata$TN_ugL),"extend")

alldata$TP_ugL[1]<-median(na.exclude(alldata$TP_ugL))
alldata$TP_ugL[2424]<-median(na.exclude(alldata$TP_ugL))
alldata$TP_ugL<-na.fill(na.approx(alldata$TP_ugL),"extend")

alldata$NH4_ugL[1]<-median(na.exclude(alldata$NH4_ugL))
alldata$NH4_ugL[2424]<-median(na.exclude(alldata$NH4_ugL))
alldata$NH4_ugL<-na.fill(na.approx(alldata$NH4_ugL),"extend")

alldata$NO3NO2_ugL[1]<-median(na.exclude(alldata$NO3NO2_ugL))
alldata$NO3NO2_ugL[2424]<-median(na.exclude(alldata$NO3NO2_ugL))
alldata$NO3NO2_ugL<-na.fill(na.approx(alldata$NO3NO2_ugL),"extend")

alldata$SRP_ugL[1]<-median(na.exclude(alldata$SRP_ugL))
alldata$SRP_ugL[2424]<-median(na.exclude(alldata$SRP_ugL))
alldata$SRP_ugL<-na.fill(na.approx(alldata$SRP_ugL),"extend")

alldata$DOC_mgL[1]<-median(na.exclude(alldata$DOC_mgL))
alldata$DOC_mgL[2424]<-median(na.exclude(alldata$DOC_mgL))
alldata$DOC_mgL<-na.fill(na.approx(alldata$DOC_mgL),"extend")

alldata$DIC_mgL[1]<-median(na.exclude(alldata$DIC_mgL))
alldata$DIC_mgL[2424]<-median(na.exclude(alldata$DIC_mgL))
alldata$DIC_mgL<-na.fill(na.approx(alldata$DIC_mgL),"extend")

alldata<- merge(alldata, ghg1, by="time", all.x=TRUE)

#need to convert mass observed data into mmol/m3 units for two DOC pools
SSS_inflow <- alldata %>% 
  select(time, FLOW, SALT, TN_ugL:CAR_ch4, OXY_oxy) %>%
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10) %>% #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
  mutate(OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90) %>% #assuming 90% of total DOC is in labile DOC pool
  mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
  mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
  mutate(DRSI_mgL = rep(median(silica$DRSI_mgL),length(SSS_inflow$time))) %>% #setting inflow to median 9m concentration from 2014
  mutate(OGM_poc = 0.1*(OGM_doc+OGM_docr)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
  mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.1) %>% #DON is ~5x greater than PON (Wetzel page 220)
  mutate(OGM_donr = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.9) %>% 
  mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>%
  mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)*0.1) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
  mutate(OGM_dopr = 0.3*(TP_ugL-PHS_frp)*0.9) %>% #using this pool instead of frp_ads to keep stoichiometric balance between recalcitrant & labile pools of CNP
  mutate(OGM_pop = 0.7*(TP_ugL-PHS_frp)) %>% 
  #mutate(PHS_frp_ads = PHS_frp) %>% #Following Farrell et al. 2020 EcolMod
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
      #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
  mutate(SIL_rsi = DRSI_mgL*1000*(1/60.08))  #setting the Silica concentration to the median 2014 inflow concentration for consistency
    
#reality check of mass balance: these histograms should be at zero minus rounding errors
hist(SSS_inflow$TP_ugL - (SSS_inflow$PHS_frp + SSS_inflow$OGM_dop + SSS_inflow$OGM_dopr + SSS_inflow$OGM_pop))
hist(SSS_inflow$TN_ugL - (SSS_inflow$NIT_amm + SSS_inflow$NIT_nit + SSS_inflow$OGM_don + SSS_inflow$OGM_donr + SSS_inflow$OGM_pon))

SSS_inflow <- SSS_inflow %>%
  select(time:SALT, OXY_oxy:CAR_dic, CAR_ch4, SIL_rsi)

#now need to get water temperature at 8m to set as the inflow SSS temp
#need to pull in from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/10/2461524a7da8f1906bfc3806d594f94c" 
infile1 <- paste0(getwd(),"/CTD_final_2013_2019.csv")
download.file(inUrl1,infile1,method="curl")

CTD<-read.csv("CTD_final_2013_2019.csv", header=TRUE) #now need to get temp at 8m for inflow
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

#make final SSS inflow file, setting 12/31 of each year to 4oC in lieu of CTD data for interpolation
SSS_inflowALL<-merge(SSS_inflow,CTD8, by="time",all.x=TRUE)
SSS_inflowALL$TEMP[231]<-4
SSS_inflowALL$TEMP[596]<-4
SSS_inflowALL$TEMP[961]<-4
SSS_inflowALL$TEMP[1329]<-4
SSS_inflowALL$TEMP[1694]<-4
SSS_inflowALL$TEMP[2059]<-4
SSS_inflowALL$TEMP[2424]<-4 #set last row as 4oC in prep for freezing
SSS_inflowALL$TEMP<-na.fill(na.approx(SSS_inflowALL$TEMP),"extend")
#SSS_inflowALL$CAR_ch4 <-na.fill(na.approx(SSS_inflowALL$CAR_ch4), "extend")
plot(SSS_inflowALL$time, SSS_inflowALL$TEMP, type = "o")

#need to fill in missing data for earlier part of 2013-2015 CH4 data
#in lieu of having data, setting concentrations to mean observations during 2015-2019
#first thing first, need to figure out what mean concentration is on May 15 (day 135)
missingdata <- ghg1 %>% 
  mutate(DOY = yday(time)) %>%
  group_by(DOY) %>%
  summarise(CAR_ch4=mean(CAR_ch4))
plot(missingdata$DOY, missingdata$CAR_ch4)

#setting missing 2013-2014 data as mean values observed at 8m during 2015-2019; 
#I recognize this isn't perfect but AR1 & mechanistic models don't do a good job here either :(
for(i in 1:length(SSS_inflowALL$time)){
  if(is.na(SSS_inflowALL$CAR_ch4[i])){
    SSS_inflowALL$CAR_ch4[i] = missingdata$CAR_ch4[which(missingdata$DOY==yday(SSS_inflowALL$time[i]))]
  }
}  

plot(SSS_inflowALL$time, SSS_inflowALL$CAR_ch4) #not great, but not horrible and keeps pattern

SSS_inflowALL<-SSS_inflowALL %>%
  select(time, FLOW, TEMP, SALT:OGM_docr, OGM_poc:SIL_rsi)  %>% #get all of the columns in order
  mutate_if(is.numeric, round, 4) #round to 4 digits 

SSS_inflowALL[which(duplicated(SSS_inflowALL$time)),] #identify if there are repeated dates

SSS_inflowALL <- SSS_inflowALL[(!duplicated(SSS_inflowALL$time)),] #remove repeated dates

#et voila! the final inflow file for the SSS for 2 pools of DOC
write.csv(SSS_inflowALL, "FCR_SSS_inflow_2013_2019_20200624_allfractions_2DOCpools.csv", row.names = FALSE)



#NOW NEED TO MAKE SSS inflow file for 1 pool of DOC
#need to convert mass observed data into mmol/m3 units for two DOC pools
SSS_inflow <- alldata %>% 
  select(time, FLOW, SALT, TN_ugL:CAR_ch4, OXY_oxy) %>%
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)) %>% 
  mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
  mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
  mutate(DRSI_mgL = rep(median(silica$DRSI_mgL),length(SSS_inflow$time))) %>% #setting inflow to median 9m concentration from 2014
  mutate(OGM_poc = 0.1*(OGM_doc)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
  mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>% #DON is ~5x greater than PON (Wetzel page 220)
  mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>%
  mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
  mutate(OGM_pop = 0.7*(TP_ugL-PHS_frp)) %>% 
  mutate(PHS_frp_ads = PHS_frp) %>% #Following Farrell et al. 2020 EcolMod
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
  #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
  mutate(SIL_rsi = DRSI_mgL*1000*(1/60.08))  #setting the Silica concentration to the median 2014 inflow concentration for consistency

#reality check of mass balance: these histograms should be at zero minus rounding errors
hist(SSS_inflow$TP_ugL - (SSS_inflow$PHS_frp + SSS_inflow$OGM_dop + SSS_inflow$OGM_pop))
hist(SSS_inflow$TN_ugL - (SSS_inflow$NIT_amm + SSS_inflow$NIT_nit + SSS_inflow$OGM_don + SSS_inflow$OGM_pon))

SSS_inflow <- SSS_inflow %>%
  select(time:SALT, OXY_oxy:CAR_dic, CAR_ch4, SIL_rsi)

#now need to get water temperature at 8m to set as the inflow SSS temp
#need to pull in from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/10/2461524a7da8f1906bfc3806d594f94c" 
infile1 <- paste0(getwd(),"/CTD_final_2013_2019.csv")
download.file(inUrl1,infile1,method="curl")

CTD<-read.csv("CTD_final_2013_2019.csv", header=TRUE) #now need to get temp at 8m for inflow
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

#make final SSS inflow file, setting 12/31 of each year to 4oC in lieu of CTD data for interpolation
SSS_inflowALL<-merge(SSS_inflow,CTD8, by="time",all.x=TRUE)
SSS_inflowALL$TEMP[231]<-4
SSS_inflowALL$TEMP[596]<-4
SSS_inflowALL$TEMP[961]<-4
SSS_inflowALL$TEMP[1329]<-4
SSS_inflowALL$TEMP[1694]<-4
SSS_inflowALL$TEMP[2059]<-4
SSS_inflowALL$TEMP[2424]<-4 #set last row as 4oC in prep for freezing
SSS_inflowALL$TEMP<-na.fill(na.approx(SSS_inflowALL$TEMP),"extend")
#SSS_inflowALL$CAR_ch4 <-na.fill(na.approx(SSS_inflowALL$CAR_ch4), "extend")
plot(SSS_inflowALL$time, SSS_inflowALL$TEMP, type = "o")

#need to fill in missing data for earlier part of 2013-2015 CH4 data
#in lieu of having data, setting concentrations to mean observations during 2015-2019
#first thing first, need to figure out what mean concentration is on May 15 (day 135)
missingdata <- ghg1 %>% 
  mutate(DOY = yday(time)) %>%
  group_by(DOY) %>%
  summarise(CAR_ch4=mean(CAR_ch4))
plot(missingdata$DOY, missingdata$CAR_ch4)

#setting missing 2013-2014 data as mean values observed at 8m during 2015-2019; 
#I recognize this isn't perfect but AR1 & mechanistic models don't do a good job here either :(
for(i in 1:length(SSS_inflowALL$time)){
  if(is.na(SSS_inflowALL$CAR_ch4[i])){
    SSS_inflowALL$CAR_ch4[i] = missingdata$CAR_ch4[which(missingdata$DOY==yday(SSS_inflowALL$time[i]))]
  }
}  

plot(SSS_inflowALL$time, SSS_inflowALL$CAR_ch4) #not great, but not horrible and keeps pattern

SSS_inflowALL<-SSS_inflowALL %>%
  select(time, FLOW, TEMP, SALT:OGM_doc, OGM_poc:SIL_rsi)  %>% #get all of the columns in order
  mutate_if(is.numeric, round, 4) #round to 4 digits 

SSS_inflowALL[which(duplicated(SSS_inflowALL$time)),] #identify if there are repeated dates

SSS_inflowALL <- SSS_inflowALL[(!duplicated(SSS_inflowALL$time)),] #remove repeated dates

#et voila! the final inflow file for 2 DOC pools
write.csv(SSS_inflowALL, "FCR_SSS_inflow_2013_2019_20200607_allfractions_1DOCpool.csv", row.names = FALSE)


##################################################################
#now make SSS outflow file
SSSoutflow<-SSS_inflowALL[,c(1,2)]
write.csv(SSSoutflow, "FCR_SSS_outflow_2013_2019_20200601.csv", row.names = FALSE)
