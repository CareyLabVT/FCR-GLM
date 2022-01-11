#*****************************************************************                                                           *
#* TITLE:   Falling Creek Reservoir GLM-AED stream inflow file 
#*          preparation                                          *
#* AUTHORS:  C.C. Carey                                          *
#* DATE:   Originally developed 16 July 2018; Last modified 26 Sept 2021                            
#* NOTES:  CCC developed to estimate reservoir inflows for FCR; 
#*         CCC subsequently edited on 1 June 2020 and made tidy,
#*         with subsequent tweaks to annotation in summer 2021
#*****************************************************************


setwd("FCR_2013_2019GLMHistoricalRun_GLMv3beta/inputs")
sim_folder <- getwd()

#load packages
library(dplyr)
library(zoo)
library(EcoHydRology)
library(rMR)
library(tidyverse)
library(lubridate)
library(dplyr)

#first read in FCR weir inflow file from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/202/6/96bdffa73741ec6b43a98f2c5d15daeb" 
infile1 <- paste0(getwd(),"/inflow_for_EDI_2013_06Mar2020.csv")
download.file(inUrl1,infile1,method="curl", extra = '-k')

inflow<-read_csv("inflow_for_EDI_2013_06Mar2020.csv") %>% 
  dplyr::select(DateTime, WVWA_Flow_cms, WVWA_Temp_C) %>% 
  rename(time=DateTime, FLOW=WVWA_Flow_cms, TEMP=WVWA_Temp_C) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  dplyr::filter(time < "2020-01-01") %>%
  group_by(time) %>% 
  summarise(FLOW=mean(FLOW), TEMP=mean(TEMP)) #gives averaged daily flow per day in m3/s
 
#diagnostic plot
plot(inflow$time, inflow$FLOW)

#creating new dataframe with list of all dates
datelist<-seq.Date(as.Date("2013/05/15"),as.Date("2019/12/31"), "days")
datelist<-as.data.frame(datelist)
colnames(datelist)=c("time")
datelist$time<-as.POSIXct(strptime(datelist$time, "%Y-%m-%d", tz="EST"))

#merge inflow file with datelist to make sure that we have all days covered 
  #interpolating the few missing days
weir <- merge(datelist, inflow, by="time", all.x=TRUE) %>%
  mutate(FLOW = na.fill(na.approx(FLOW),"extend")) %>%
  mutate(TEMP = na.fill(na.approx(TEMP),"extend")) %>%
  mutate(SALT = rep(0,length(datelist)))

#some diagnostic plots of inflow weir
plot(weir$time, weir$FLOW, type = "o")
#par(new = T)
plot(weir$time, weir$TEMP, type = "l", col = "red")

#now let's merge with chemistry
#first pull in FCR chem data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/9/fe500aac19d1a0d78bb2cb1d196cdbd7" 
infile1 <- paste0(getwd(),"/chemistry_2013_2020.csv")
download.file(inUrl1,infile1,method="curl", extra='-k')

FCRchem <- read.csv("chemistry_2013_2020.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==100) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time = DateTime) %>%
  filter(TP_ugL < 100) %>% #remove outliers
  select(time:DIC_mgL) 

#read in lab dataset of dissolved silica, measured in summer 2014 only
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/542/1/791ec9ca0f1cb9361fa6a03fae8dfc95" 
infile1 <- paste0(getwd(),"/silica_master_df.csv")
download.file(inUrl1,infile1,method="curl", extra='-k')

silica <- read.csv("silica_master_df.csv", header=T) %>%
  dplyr::filter(Reservoir == "FCR") %>% 
  dplyr::filter(Site == 100) %>% #100 = weir inflow site
  select(DateTime, DRSI_mgL) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time = DateTime)
  
#diagnostic plot of silica
plot(silica$time, silica$DRSI_mgL)
hist(silica$DRSI_mgL)
median(silica$DRSI_mgL) #this median concentration is going to be used to set as 
#the constant Si inflow conc in both wetland & weir inflows

alldata<-merge(weir, FCRchem, by="time", all.x=TRUE)

#read in dataset of CH4 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/551/5/38d72673295864956cccd6bbba99a1a3" 
infile1 <- paste0(getwd(),"/Dissolved_CO2_CH4_Virginia_Reservoirs.csv")
download.file(inUrl1,infile1,method="curl", extra='-k')

ghg <- read.csv("Dissolved_CO2_CH4_Virginia_Reservoirs.csv", header=T) %>%
  dplyr::filter(Reservoir == "FCR") %>%
  dplyr::filter(Site == 100) %>% #weir inflow
  select(DateTime, ch4_umolL) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time = DateTime, CAR_ch4 = ch4_umolL) %>%
  group_by(time) %>%
  drop_na %>% 
  summarise(CAR_ch4 = mean(CAR_ch4)) %>%
  dplyr::filter(CAR_ch4<0.2) #remove outliers
plot(ghg$time, ghg$CAR_ch4)

datelist2<-seq.Date(as.Date(first(ghg$time)),as.Date(last(ghg$time)), "days")
datelist2<-as.data.frame(datelist2)
colnames(datelist2)=c("time")
datelist2$time<-as.POSIXct(strptime(datelist2$time, "%Y-%m-%d", tz="EST"))

ghg1 <- merge(datelist2, ghg, by="time", all.x=TRUE) 
ghg1$CAR_ch4 <- na.fill(na.approx(ghg1$CAR_ch4), "extend")
plot(ghg1$time, ghg1$CAR_ch4) #decent coverage 2015-2019, but need to develop 2013-2014 data that keeps temporal pattern of data
#so, need to average value among years per day of year

missingdata <- ghg1 %>% 
  mutate(DOY = yday(time)) %>%
  group_by(DOY) %>%
  summarise(CAR_ch4=mean(CAR_ch4))
plot(missingdata$DOY, missingdata$CAR_ch4)
#gives us DOY concentrations averaged across 2015-2019
#now, need to apply this averaged DOY concentration to 2013-2014 missing dates

ghg2 <- merge(datelist, ghg1, by="time", all.x=TRUE) %>% 
  mutate(DOY = yday(time))

for(i in 1:length(ghg2$time)){
  if(is.na(ghg2$CAR_ch4[i])){
    ghg2$CAR_ch4[i] = missingdata$CAR_ch4[which(missingdata$DOY==ghg2$DOY[i])]
  }
}  
plot(ghg2$time, ghg2$CAR_ch4)
#check to make sure it all works: each day's CH4 concentration during
#2013-early 2015 is the mean daily data for 2015-2019


#some other cool long-term plots
plot(alldata$time, alldata$SRP_ugL)
plot(alldata$time, alldata$DOC_mgL)
plot(alldata$time, alldata$NO3NO2_ugL)
plot(alldata$time, alldata$NH4_ugL)
plot(alldata$time, alldata$TN_ugL)
plot(alldata$time, alldata$TP_ugL)
plot(alldata$time, alldata$DIC_mgL)

alldata<-merge(alldata, ghg2, by="time", all.x=TRUE)
alldata <- alldata[(!duplicated(alldata$time)),]#remove duplicated dates

#now need to interpolate missing values in chem; setting 1st and last value in time series as medians
#then linearly interpolating the middle missing values
alldata$TN_ugL[1]<-median(na.exclude(alldata$TN_ugL))
alldata$TN_ugL[2422]<-median(na.exclude(alldata$TN_ugL)) #2423 = last row
alldata$TN_ugL<-na.fill(na.approx(alldata$TN_ugL),"extend")

alldata$TP_ugL[1]<-median(na.exclude(alldata$TP_ugL))
alldata$TP_ugL[2422]<-median(na.exclude(alldata$TP_ugL))
alldata$TP_ugL<-na.fill(na.approx(alldata$TP_ugL),"extend")

alldata$NH4_ugL[1]<-median(na.exclude(alldata$NH4_ugL))
alldata$NH4_ugL[2422]<-median(na.exclude(alldata$NH4_ugL))
alldata$NH4_ugL<-na.fill(na.approx(alldata$NH4_ugL),"extend")

alldata$NO3NO2_ugL[1]<-median(na.exclude(alldata$NO3NO2_ugL))
alldata$NO3NO2_ugL[2422]<-median(na.exclude(alldata$NO3NO2_ugL))
alldata$NO3NO2_ugL<-na.fill(na.approx(alldata$NO3NO2_ugL),"extend")

alldata$SRP_ugL[1]<-median(na.exclude(alldata$SRP_ugL))
alldata$SRP_ugL[2422]<-median(na.exclude(alldata$SRP_ugL))
alldata$SRP_ugL<-na.fill(na.approx(alldata$SRP_ugL),"extend")

alldata$DOC_mgL[1]<-median(na.exclude(alldata$DOC_mgL))
alldata$DOC_mgL[2422]<-median(na.exclude(alldata$DOC_mgL))
alldata$DOC_mgL<-na.fill(na.approx(alldata$DOC_mgL),"extend")

alldata$DIC_mgL[1]<-median(na.exclude(alldata$DIC_mgL))
alldata$DIC_mgL[2422]<-median(na.exclude(alldata$DIC_mgL))
alldata$DIC_mgL<-na.fill(na.approx(alldata$DIC_mgL),"extend")

alldata <- alldata[(!duplicated(alldata$time)),]#remove duplicated dates

#need to convert mass observed data into mmol/m3 units for two pools of organic carbon
weir_inflow <- alldata %>% 
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10) %>% #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
  mutate(OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90) %>% #assuming 90% of total DOC is in recalcitrant DOC pool
  mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
  mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
  mutate(OGM_poc = 0.1*(OGM_doc+OGM_docr)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
  mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.10) %>% #DON is ~5x greater than PON (Wetzel page 220)
  mutate(OGM_donr = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.90) %>% #to keep mass balance with DOC, DONr is 90% of total DON
  mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>% #detemined by subtraction
  mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)*0.10) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
  mutate(OGM_dopr = 0.3*(TP_ugL-PHS_frp)*0.90) %>% #to keep mass balance with DOC & DON, DOPr is 90% of total DOP
  mutate(OGM_pop = TP_ugL-(OGM_dop+OGM_dopr+PHS_frp)) %>% # #In lieu of having the adsorbed P pool activated in the model, need to have higher complexed P
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) #Long-term median pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
#given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)

  
#reality check of mass balance: these histograms should be at zero minus rounding errors
hist(weir_inflow$TP_ugL - (weir_inflow$PHS_frp + weir_inflow$OGM_dop + weir_inflow$OGM_dopr + weir_inflow$OGM_pop))
hist(weir_inflow$TN_ugL - (weir_inflow$NIT_amm + weir_inflow$NIT_nit + weir_inflow$OGM_don + weir_inflow$OGM_donr + weir_inflow$OGM_pon))

#creating OXY_oxy column using RMR package, assuming that oxygen is at 100% saturation in this very well-mixed stream
for(i in 1:length(weir_inflow$TEMP)){
  weir_inflow$OXY_oxy[i]<-(temp.C= Eq.Ox.conc(weir_inflow$TEMP[i], elevation.m = 506,
                                  bar.press = NULL, bar.units = NULL,
                                  out.DO.meas = "mg/L",
                                  salinity = 0, salinity.units = "pp.thou"))*1000*(1/32)
}

#clean it up and get vars in order
weir_inflow <- weir_inflow %>%
  select(time, FLOW, TEMP, SALT, OXY_oxy, NIT_amm:CAR_dic, CAR_ch4) %>% 
  mutate(SIL_rsi = rep(median(silica$DRSI_mgL),length(weir_inflow$time))) %>%
  mutate(SIL_rsi = SIL_rsi*1000*(1/60.08)) %>% #setting the Silica concentration to the median 2014 inflow concentration for consistency
  mutate_if(is.numeric, round, 4) #round to 4 digits 

#write file for inflow for the weir, with 2 pools of OC (DOC + DOCR)  
#write.csv(weir_inflow, "FCR_weir_inflow_2013_2019_20200624_allfractions_2poolsDOC.csv", row.names = F)
write.csv(weir_inflow, "FCR_weir_inflow_2013_2019_20220104_allfractions_2poolsDOC.csv", row.names = F)

#copying dataframe in workspace to be used later
alltdata = alldata

########SKIP THIS STEP IF YOU WANT TO USE 2 POOLS OF OC! 
# #The code below is for the 1 pool option
# #note, because there is no recalcitrant DOC in this inflow, there similarly isn't any recalcitrant DON or DOP
# #This is making the weir inflow with only *1* pool of DOC, DON, or DOP
# #need to convert mass observed data into mmol/m3 units for ONE pool of organic carbon
# weir_inflow <- alldata %>% 
#   mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
#   mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
#   mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
#   mutate(OGM_doc = DOC_mgL*1000*(1/12.01)) %>% 
#   mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
#   mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
#   mutate(OGM_poc = 0.1*(OGM_doc)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
#   mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>% #DON is ~5x greater than PON (Wetzel page 220)
#   mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>%
#   mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
#   mutate(OGM_pop = 0.7*(TP_ugL-PHS_frp)) %>% 
#   mutate(PHS_frp_ads = PHS_frp) %>% #Following Farrell et al. 2020 EcolMod
#   mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
# #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
# 
# #reality check of mass balance: these histograms should be at zero minus rounding errors
# hist(weir_inflow$TP_ugL - (weir_inflow$PHS_frp + weir_inflow$OGM_dop + weir_inflow$OGM_pop))
# hist(weir_inflow$TN_ugL - (weir_inflow$NIT_amm + weir_inflow$NIT_nit + weir_inflow$OGM_don + weir_inflow$OGM_pon))
# 
# #creating OXY_oxy column using RMR package, assuming that oxygen is at 100% saturation in this very well-mixed stream
# for(i in 1:length(weir_inflow$TEMP)){
#   weir_inflow$OXY_oxy[i]<-(temp.C= Eq.Ox.conc(weir_inflow$TEMP[i], elevation.m = 506,
#                                               bar.press = NULL, bar.units = NULL,
#                                               out.DO.meas = "mg/L",
#                                               salinity = 0, salinity.units = "pp.thou"))*1000*(1/32)
# }
# 
# weir_inflow <- weir_inflow %>%
#   select(time, FLOW, TEMP, SALT, OXY_oxy, NIT_amm:CAR_dic, CAR_ch4) %>% 
#   mutate(SIL_rsi = rep(median(silica$DRSI_mgL),length(weir_inflow$time))) %>%
#   mutate(SIL_rsi = SIL_rsi*1000*(1/60.08)) %>% #setting the Silica concentration to the median 2014 inflow concentration for consistency
#   mutate_if(is.numeric, round, 4) #round to 4 digits 
# 
# write.csv(weir_inflow, "FCR_weir_inflow_2013_2019_20200607_allfractions_1poolDOC.csv", row.names = F)
# 

##############################################################
##############################################################
#Now we make the inflow file for the wetland stream, based on observed comparisons of discharge.
#The general approach is that we're going to first divide total discharge at the weir into
#precipitation-driven flow and baseflow. We will use intermittent observations of baseflow from 
#Falling Creek to determine a constant baseflow rate based on a ratio of Tunnel Branch:Falling
#Creek baseflow, and then add that to the precipitation-driven flow calculated in the 
#"WetlandInflowPrep.R" script in the "field_data" directory via the Schuler eqn method (Ward et al. 2020).

#Tunnel Branch = weir inflow
#Falling Creek = wetland inflow

#First let's read in the wetland pricipitation-driven flow (runoff) calculated from the Schuler equation
# as output of the WetlandInflowPrep.R script.
runoff <- read.csv("WetlandWeir_Runoff_FCR_20200615.csv", header=T) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time=DateTime)
#this gives us the precip-driven discharge at the wetland; we now need to add this to baseflow

#so next, let's read in EDI discharge file to compare *baseflow* weir and wetland inflow levels
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/454/4/a18421fd2e95c15d6f97009d5fef3e59" 
infile1 <- paste0(getwd(),"/2019_Continuum_Discharge.csv")
download.file(inUrl1,infile1,method="curl", extra='-k')

inflow<-read_csv("inflow_for_EDI_2013_06Mar2020.csv") %>% 
  dplyr::select(DateTime, WVWA_Flow_cms, WVWA_Temp_C) %>% 
  rename(time=DateTime, FLOW=WVWA_Flow_cms, TEMP=WVWA_Temp_C) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  dplyr::filter(time < "2020-01-01") %>%
  group_by(time) %>% 
  summarise(FLOW=mean(FLOW), TEMP=mean(TEMP)) #gives averaged daily flow per day in m3/s

#creating new dataframe with list of all dates
datelist<-seq.Date(as.Date("2013/05/15"),as.Date("2019/12/31"), "days")
datelist<-as.data.frame(datelist)
colnames(datelist)=c("time")
datelist$time<-as.POSIXct(strptime(datelist$time, "%Y-%m-%d", tz="EST"))

#merge inflow file with datelist to make sure that we have all days covered
weir_inflow <- merge(datelist, inflow, by="time", all.x=TRUE) %>%
  mutate(FLOW = na.fill(na.approx(FLOW),"extend"))

#some diagnostic plots of inflow weir
plot(weir_inflow$time, weir_inflow$FLOW, type = "o")

#next, subtract the Schuler-derived weir precipitation runoff from the measured (gauged) inflow
weir_data <- merge(runoff, weir_inflow, by="time") %>% 
  mutate(baseflow_subtracted = FLOW-weir_runoff_m3s) 

plot(weir_data$time, weir_data$FLOW, type="l", ylab="m3/s")
  lines(weir_data$time, weir_data$baseflow_subtracted, col="red")

#now we need to determine the ratio of baseflows from weir:wetland  
discharge <- read_csv("2019_Continuum_Discharge.csv") %>%
  dplyr::filter(Reservoir=="FCR")  %>%
  dplyr::filter(Site == 200) %>% #wetland site = 200; weir site = 100
  rename(time=Date, wetlandFLOW=Flow_cms) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  select(time, wetlandFLOW)

#merged in wetland discharge + weir inflow
data <- merge(inflow,discharge, by="time", all=FALSE)  %>%
  mutate(ratio = FLOW/wetlandFLOW) %>% 
  na.omit()  %>%
  dplyr::filter(ratio < 140) #remove outlier
data <- data[which(is.finite(data$ratio)),] #remove INFs

#comparing weir vs. wetland inflows
mean(data$ratio) #1.73
hist(data$ratio)
plot(data$time, data$ratio)
#going with ratio of weir:wetland = 1.73 from n=19 days for purpose of this analysis, can be edited here
Flowratio <- mean(data$ratio)

#now let's finalize the water budget for the wetland inflow
wetland_inf <- weir_data %>% 
  mutate(total = wetland_runoff_m3s + baseflow_subtracted*1/Flowratio) %>% 
  #this gives us the sum of the overland precip-runoff + baseflow for the wetland
  select(time, total, TEMP) %>% 
  rename(FLOW = total)

#now let's merge with chemistry
#first pull in FCR chem data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/6/2b3dc84ae6b12d10bd5485f1c300af13" 
infile1 <- paste0(getwd(),"/chem.csv")
download.file(inUrl1,infile1,method="curl",extra='-k')

#now we need to compare the wetland and weir chemistry data to figure out ratios to multiply by the weir inflow
FCRchem <- read.csv("chem.csv", header=T) %>% 
  select(Reservoir:DIC_mgL) %>%
  filter(Reservoir=="FCR") %>%
  filter(Site==100 | Site==200) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time = DateTime) %>%
  filter(time > "2019-01-01") %>% #remove data prior to 2019, when wetland site wasn't sampled
  select(Site, time, TN_ugL:DIC_mgL) %>%
  pivot_wider(names_from = Site, values_from = c(TN_ugL, TP_ugL, NH4_ugL, NO3NO2_ugL, SRP_ugL, DOC_mgL, DIC_mgL)) %>%
  na.omit() %>% #24 days in total with wetland and weir chemistry!
  mutate(TN = TN_ugL_100/TN_ugL_200) %>%
  mutate(TP = TP_ugL_100/TP_ugL_200) %>%
  mutate(NH4 = NH4_ugL_100/NH4_ugL_200) %>%
  mutate(NO3 = NO3NO2_ugL_100/NO3NO2_ugL_200) %>%
  mutate(SRP = SRP_ugL_100/SRP_ugL_200) %>%
  mutate(DOC = DOC_mgL_100/DOC_mgL_200) %>%
  mutate(DIC = DIC_mgL_100/DIC_mgL_200)

#diagnostic plots of water chem ratio of Weir:Wetland from 24 days measured in Feb-Oct 2019
plot(FCRchem$time, FCRchem$TN) #one very high point in March
plot(FCRchem$time, FCRchem$TP) #one very high point in March
plot(FCRchem$time, FCRchem$NH4) #one very high point in Feb
plot(FCRchem$time, FCRchem$NO3) #2 very high points in April/June
plot(FCRchem$time, FCRchem$SRP) #no outliers
plot(FCRchem$time, FCRchem$DOC) #no outliers
plot(FCRchem$time, FCRchem$DIC) #no outliers

TNratio <- max(FCRchem$TN)
TPratio <- max(FCRchem$TP)
NH4ratio <- 10*max(FCRchem$NH4)#range is from 0.111 to 1.8; median = 0.32, mean = 0.399; [23] = 0.9
NO3ratio <- 10*max(FCRchem$NO3) #median = 0.766, max = 14.666
SRPratio <- median(FCRchem$SRP)#range is from 0.33 to 4.75, median = 1.75, mean = 1.98
DOCratio <- sort(FCRchem$DOC)[6]#note that this ratio emerged from model tuning
DICratio <- median(FCRchem$DIC)

#read in dataset of CH4 from 2015-2019
ghg_inflows <- read.csv("Dissolved_CO2_CH4_Virginia_Reservoirs.csv", header=T) %>%
  filter(Reservoir == "FCR") %>%
  filter(Depth_m==100 | Depth_m==200) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  filter(DateTime > "2019-01-01") %>%
  select(DateTime, Depth_m, ch4_umolL) %>%
  group_by(DateTime, Depth_m) %>% 
  summarise(ch4_umolL=mean(ch4_umolL)) %>%
  rename(CAR_ch4 = ch4_umolL) %>%
  pivot_wider(names_from = Depth_m, values_from = CAR_ch4, names_prefix="x.") %>%
  na.omit() %>%
  mutate(CH4 = x.100/x.200)
CH4ratio <- median(ghg_inflows$CH4)

#now, merge in correct water budget for the wetland with the weir chemistry
data1 <- merge(wetland_inf, alltdata, by="time") %>% 
  select(-c(FLOW.y, TEMP.y, TEMP.x, Depth_m, DOY)) %>% 
  rename(FLOW = FLOW.x)

#now we create the wetland file as a multiplier of the weir inflow chem, then convert to molar values
#to get 2 pools of OC first!
wetland_inflow <- data1 %>% #using weir chemistry in workspace for multiplying ratios
  mutate(TN_ugL = TN_ugL/TNratio) %>% 
  mutate(TP_ugL = TP_ugL/TPratio) %>% 
  mutate(NH4_ugL = NH4_ugL/NH4ratio) %>% 
  mutate(NO3NO2_ugL = NO3NO2_ugL/NO3ratio) %>% 
  mutate(SRP_ugL = SRP_ugL/SRPratio) %>% 
  mutate(DOC_mgL = DOC_mgL/DOCratio) %>% 
  mutate(DIC_mgL = DIC_mgL/DICratio) %>% 
  mutate(DRSI_mgL = rep(median(silica$DRSI_mgL),length(data1$time))) %>%
  mutate(CAR_ch4 = CAR_ch4/CH4ratio) %>%
  select(time, FLOW, SALT, TN_ugL:CAR_ch4, DRSI_mgL) %>% 
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10) %>% #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
  mutate(OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90) %>% #assuming 90% of total DOC is in labile DOC pool
  mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
  mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
  mutate(OGM_poc = 0.1*(OGM_doc+OGM_docr)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
  mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.10) %>% #DON is ~5x greater than PON (Wetzel page 220)
  mutate(OGM_donr = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.90) %>% 
  mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>%
  mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)*0.1) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
  mutate(OGM_dopr = 0.3*(TP_ugL-PHS_frp)*0.9) %>%
  mutate(OGM_pop = TP_ugL-(OGM_dopr+OGM_dop+PHS_frp)) %>% #0.7*(TP_ugL-PHS_frp)) %>% 
 # mutate(PHS_frp_ads = PHS_frp) %>% #Following Farrell et al. 2020 EcolMod
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
#given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
  mutate(SIL_rsi = DRSI_mgL*1000*(1/60.08))  #setting the Silica concentration to the median 2014 inflow concentration for consistency
  
#reality check: these distributions should be at 0 or near zero due to rounding errors
hist(wetland_inflow$TP_ugL - (wetland_inflow$PHS_frp + wetland_inflow$OGM_dop + wetland_inflow$OGM_dopr + wetland_inflow$OGM_pop))
hist(wetland_inflow$TN_ugL - (wetland_inflow$NIT_amm + wetland_inflow$NIT_nit + wetland_inflow$OGM_don + wetland_inflow$OGM_donr + wetland_inflow$OGM_pon))

#now need to get temp for the inflow; setting to 0.1 m water temp at site 50 using CTD
#checked site 50 vs site 10 and they are highly correlated
#need to pull in from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/10/2461524a7da8f1906bfc3806d594f94c" 
infile1 <- paste0(getwd(),"/CTD_final_2013_2019.csv")
download.file(inUrl1,infile1,method="curl", extra='-k')

CTD<-read.csv("CTD_final_2013_2019.csv", header=TRUE) #now need to get temp at 8m for inflow
CTD10 <- CTD %>% #CTD surface water temp at site 10 
  select(Reservoir:Temp_C) %>%
  filter(Reservoir=="FCR") %>%
  filter(Site==10) %>%
  rename(time=Date, TEMP=Temp_C) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  group_by(time) %>% 
  summarise(TEMP=first(TEMP)) 

CTD50 <- CTD %>% #CTD surface water temp at site 50
  select(Reservoir:Temp_C) %>%
  filter(Reservoir=="FCR") %>%
  filter(Site==50) %>%
  rename(time=Date, TEMP=Temp_C) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  group_by(time) %>% 
  summarise(TEMP=first(TEMP)) 

CTDcompare <- merge(CTD10, CTD50, by="time")
cor(CTDcompare$TEMP.x, CTDcompare$TEMP.y, method="pearson", use="pairwise.complete.obs")
cor.test(CTDcompare$TEMP.x, CTDcompare$TEMP.y, method="pearson", alternative="two.sided", exact=TRUE)
#in comparison of the CTD at the surface at site 10 vs. site 50, Pearson's r = 0.98, p < 2.2 e-16, df=175, t=68.374
#thus, going to go with the site 50 results because there are many more data points for that site than at site 10

#diagnostic plot to check for 8m water temp
plot(CTD50$time, CTD50$TEMP, type = "o")

#get water temp for wetland inflow file, setting 12/31 of each year to 4oC in lieu of CTD data for interpolation
wetland <- merge(wetland_inflow,CTD50, by="time",all.x=TRUE)
wetland$TEMP[231]<-4
wetland$TEMP[596]<-4
wetland$TEMP[961]<-4
wetland$TEMP[1327]<-4
wetland$TEMP[1692]<-4
wetland$TEMP[2057]<-4
wetland$TEMP[2422]<-4 #set last row as 4oC in prep for freezing
wetland$TEMP<-na.fill(na.approx(wetland$TEMP),"extend")
plot(wetland$time, wetland$TEMP, type = "o")

#set oxygen as 100% saturation concentrations, depending on temperature
for(i in 1:length(wetland$TEMP)){
  wetland$OXY_oxy[i]<-(temp.C= Eq.Ox.conc(wetland$TEMP[i], elevation.m = 506,
                                              bar.press = NULL, bar.units = NULL,
                                              out.DO.meas = "mg/L",
                                              salinity = 0, salinity.units = "pp.thou"))*1000*(1/32)
}

#diagnostic plot for DO
plot(wetland$time,wetland$OXY_oxy) 

#now let's get all columns in the proper order!
wetland_final <- wetland %>%
  select(time, FLOW, TEMP, SALT, OXY_oxy, NIT_amm:CAR_dic, CAR_ch4, SIL_rsi) %>%
  mutate_if(is.numeric, round, 4) #round to 4 digits 

#now write file for wetland inflow
#write.csv(wetland_final, "FCR_wetland_inflow_2013_2019_20200630_allfractions_2DOCpools.csv", row.names = FALSE)
write.csv(wetland_final, "FCR_wetland_inflow_2013_2019_20220104_allfractions_2DOCpools.csv", row.names = FALSE)

###############################################################################
# #NOW to get 1 pools of OC #SKIP THIS STEP IF YOU'RE GOING TO USE 2 POOLS OF OC!
# wetland_inflow <- alltdata %>% 
#   mutate(FLOW = FLOW/Flowratio) %>% 
#   mutate(TN_ugL = TN_ugL/TNratio) %>% 
#   mutate(TP_ugL = TP_ugL/TPratio) %>% 
#   mutate(NH4_ugL = NH4_ugL/NH4ratio) %>% 
#   mutate(NO3NO2_ugL = NO3NO2_ugL/NO3ratio) %>% 
#   mutate(SRP_ugL = SRP_ugL/SRPratio) %>% 
#   mutate(DOC_mgL = DOC_mgL/DOCratio) %>% 
#   mutate(DIC_mgL = DIC_mgL/DICratio) %>% 
#   mutate(DRSI_mgL = rep(median(silica$DRSI_mgL),length(wetland_inflow$time))) %>%
#   mutate(CAR_ch4 = CAR_ch4/CH4ratio) %>%
#   select(time, FLOW, SALT, TN_ugL:CAR_ch4, DRSI_mgL) %>% 
#   mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
#   mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
#   mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
#   mutate(OGM_doc = DOC_mgL*1000*(1/12.01)) %>%
#   mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
#   mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
#   mutate(OGM_poc = 0.1*(OGM_doc)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
#   mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>% #DON is ~5x greater than PON (Wetzel page 220)
#   mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>%
#   mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
#   mutate(OGM_pop = 0.7*(TP_ugL-PHS_frp)) %>% 
#   mutate(PHS_frp_ads = PHS_frp) %>% #Following Farrell et al. 2020 EcolMod
#   mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
#   #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
#   mutate(SIL_rsi = DRSI_mgL*1000*(1/60.08))  #setting the Silica concentration to the median 2014 inflow concentration for consistency
# 
# #reality check: these distributions should be at 0 or near zero due to rounding errors
# hist(wetland_inflow$TP_ugL - (wetland_inflow$PHS_frp + wetland_inflow$OGM_dop + wetland_inflow$OGM_pop))
# hist(wetland_inflow$TN_ugL - (wetland_inflow$NIT_amm + wetland_inflow$NIT_nit + wetland_inflow$OGM_don + wetland_inflow$OGM_pon))
# 
# #now need to get temp for the inflow; setting to 0.1 m water temp at site 50 using CTD
# #checked site 50 vs site 10 and they are highly correlated
# #need to pull in from EDI
# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/10/2461524a7da8f1906bfc3806d594f94c" 
# infile1 <- paste0(getwd(),"/CTD_final_2013_2019.csv")
# download.file(inUrl1,infile1,method="curl")
# 
# CTD<-read.csv("CTD_final_2013_2019.csv", header=TRUE) #now need to get temp at 8m for inflow
# 
# CTD50 <- CTD %>% #CTD surface water temp at site 50
#   select(Reservoir:Temp_C) %>%
#   filter(Reservoir=="FCR") %>%
#   filter(Site==50) %>%
#   rename(time=Date, TEMP=Temp_C) %>%
#   mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
#   group_by(time) %>% 
#   summarise(TEMP=first(TEMP)) #to get the shallowest temp 
# 
# #diagnostic plot to check for 8m water temp
# plot(CTD50$time, CTD50$TEMP, type = "o")
# 
# #get water temp for wetland inflow file, setting 12/31 of each year to 4oC in lieu of CTD data for interpolation
# wetland <- merge(wetland_inflow,CTD50, by="time",all.x=TRUE)
# wetland$TEMP[231]<-4
# wetland$TEMP[596]<-4
# wetland$TEMP[961]<-4
# wetland$TEMP[1327]<-4
# wetland$TEMP[1692]<-4
# wetland$TEMP[2057]<-4
# wetland$TEMP[2422]<-4 #set last row as 4oC in prep for freezing
# wetland$TEMP<-na.fill(na.approx(wetland$TEMP),"extend")
# plot(wetland$time, wetland$TEMP, type = "o")
# 
# #set oxygen as 100% saturation concentrations, depending on temperature
# for(i in 1:length(wetland$TEMP)){
#   wetland$OXY_oxy[i]<-(temp.C= Eq.Ox.conc(wetland$TEMP[i], elevation.m = 506,
#                                           bar.press = NULL, bar.units = NULL,
#                                           out.DO.meas = "mg/L",
#                                           salinity = 0, salinity.units = "pp.thou"))*1000*(1/32)
# }
# 
# #diagnostic plot for DO
# plot(wetland$time,wetland$OXY_oxy) 
# 
# #now let's get all columns in the proper order!
# wetland_1pool <- wetland %>%
#   select(time, FLOW, TEMP, SALT, OXY_oxy, NIT_amm:CAR_dic, CAR_ch4, SIL_rsi) %>%
#   mutate_if(is.numeric, round, 4) #round to 4 digits 
# 
# #now write file for wetland inflow
# write.csv(wetland_1pool, "FCR_wetland_inflow_newEDI_2013_2019_20200607_allfractions_1DOCpool.csv", row.names = FALSE)

##############################################################
##############################################################

#REGARDLESS OF YOUR OC POOLS, WILL NEED TO MAKE OUTFLOW!
#now need to make new spillway outflow that sums the weir + wetland inflows to keep water balance

outflow <- weir_data %>% #from above: this has both stream inflows together
  mutate(total = FLOW + wetland_runoff_m3s + baseflow_subtracted*1/Flowratio) %>% 
  select(time, total) %>%
  rename(FLOW = total) %>%
  mutate_if(is.numeric, round, 4) #round to 4 digits 

#diagnostic plot
plot(outflow$time, outflow$FLOW)
hist(outflow$FLOW - (wetland$FLOW + weir_inflow$FLOW)) #should be at zero minus rounding errors

#write file
write.csv(outflow, "FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv", row.names=F)
