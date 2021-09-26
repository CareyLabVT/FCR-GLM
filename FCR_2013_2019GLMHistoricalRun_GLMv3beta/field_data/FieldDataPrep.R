#*****************************************************************
#* TITLE:   FCR GLM-AED script to create field data files for water
#*          quality variables            
#* AUTHORS: C.C. Carey                                          
#* DATE:   Originally developed by CCC on 16 July 2018; made tidy and 
#*         cleaned up on 2 June 2020; last editing on 13 Sep 2021                           
#* NOTES:  This script creates data files that are used for the 
#*         sensitivity analysis, model calibration, and comparison of
#*         modeled output vs. observed concentrations.
#*****************************************************************

setwd("FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data")

library(tidyverse)
library(lubridate)

depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.2) #focal depths we are trying to compare modeled data vs observations
#going with 9.2 vs. 9.3 because 9.202598 is the shallowest depth experienced by reservoir over 2013-2017

###########################################################
######TEMPERATURE, DO, AND CHLA FROM CTD

#need to import CTD observations from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/10/2461524a7da8f1906bfc3806d594f94c" 
infile1 <- paste0(getwd(),"/CTD_final_2013_2019.csv")
download.file(inUrl1,infile1,method="curl")

#read in CTD temp file from EDI to create field file, but first need to subset CTD data per each day to depths
ctd<-read.csv('CTD_final_2013_2019.csv') %>% #read in observed CTD data, which has multiple casts on the same day (problematic for comparison)
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  rename(time=Date, depth=Depth_m, temp=Temp_C, DO=DO_mgL, chla = Chla_ugL) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  select(time, depth, temp, DO, chla) %>%
  na.omit() 

#to see on average how deep the CTD casts at site 50 really are!
Zmax <- ctd %>% 
  group_by(time) %>% 
  summarise(depth = max(depth)) %>% 
  select(time, depth)
hist(Zmax$depth)

#Initialize an empty matrix with the correct number of rows and columns 
temp<-matrix(data=NA, ncol=ncol(ctd), nrow=length(depths)) #of cols in CTD data, and then nrows = # of layers produced
super_final<-matrix(data=NA, ncol=1, nrow=0)
dates<-unique(ctd$time)

#create a function to chose the matching depth closest to our focal depths
closest<-function(xv, sv){
  xv[which.min(abs(xv-sv))]}

library(plyr) #only use plyr for this for loop, then detach!

#For loop to retrieve CTD depth with the closest function and fill in matrix
for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(ctd, ctd$time == j)

  layer1<- q[q[, "depth"] == closest(q$depth,0.1),][1,]
  layer2<- q[q[, "depth"] == closest(q$depth,1),][1,]
  layer3<- q[q[, "depth"] == closest(q$depth,2),][1,]
  layer4<- q[q[, "depth"] == closest(q$depth,3),][1,]
  layer5<- q[q[, "depth"] == closest(q$depth,4),][1,]
  layer6<- q[q[, "depth"] == closest(q$depth,5),][1,]
  layer7<- q[q[, "depth"] == closest(q$depth,6),][1,]
  layer8<- q[q[, "depth"] == closest(q$depth,7),][1,]
  layer9<- q[q[, "depth"] == closest(q$depth,8),][1,]
  layer10<-q[q[, "depth"] == closest(q$depth,9),][1,]
  layer11<- q[q[, "depth"] == closest(q$depth,9.2),][1,]
  
  temp<-rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11)
  temp[,((ncol(ctd))+1)] <- depths
  colnames(temp)[((ncol(ctd))+1)]<-"new_depth"
  final <- temp
  final <- data.frame(final)
  super_final <- rbind.fill.matrix(super_final,final)
}

detach(package:plyr)#to prevent issues with dplyr vs plyr not playing well together!

#now need to clean up the data frame and make all factors numeric
super_final <- as.data.frame(super_final) %>%
  select(time, new_depth, temp, DO, chla) %>%
  rename(depth = new_depth) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  dplyr::filter(time > "2013-05-16") %>% #need to make sure that the CTD data only start after first day of sim
  mutate(depth = as.numeric(levels(depth))[depth]) %>%
  mutate(temp = as.numeric(levels(temp))[temp]) %>%
  mutate(DO = as.numeric(levels(DO))[DO]) %>%
  mutate(chla = as.numeric(levels(chla))[chla])


#now pull in YSI data to fill in missing temp/DO data from CTD dataset
#need to import YSI observations from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/198/7/25b5e8b7f4291614d5c6d959a08148d8" 
infile1 <- paste0(getwd(),"/YSI_PAR_profiles_2013-2019.csv")
download.file(inUrl1,infile1,method="curl")

ysi <- read.csv("YSI_PAR_profiles_2013-2019.csv", header=T) %>% 
  dplyr::filter(Reservoir == "FCR") %>% 
  dplyr::filter(Site == 50) %>% 
  select(DateTime:DO_mgL) %>% 
  rename(time = DateTime, depth = Depth_m, temp = Temp_C, DO = DO_mgL) %>% 
  mutate(time = as.POSIXct(strptime(time, "%m/%d/%y %H:%M", tz="EST"))) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
  dplyr::filter(!(is.na(temp) & is.na(DO)))

depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.2) 
#Initialize an empty matrix with the correct number of rows and columns 
temp<-matrix(data=NA, ncol=ncol(ysi), nrow=length(depths)) #of cols in CTD data, and then nrows = # of layers produced
super_final_ysi<-matrix(data=NA, ncol=1, nrow=0)
dates<-unique(ysi$time)

#create a function to chose the matching depth closest to our focal depths
closest<-function(xv, sv){
  xv[which.min(abs(xv-sv))]}

library(plyr) #only use plyr for this for loop, then detach!

#For loop to retrieve CTD depth with the closest function and fill in matrix
for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(ysi, ysi$time == j)
  
  layer1<- q[q[, "depth"] == closest(q$depth,0.1),][1,]
  layer2<- q[q[, "depth"] == closest(q$depth,1),][1,]
  layer3<- q[q[, "depth"] == closest(q$depth,2),][1,]
  layer4<- q[q[, "depth"] == closest(q$depth,3),][1,]
  layer5<- q[q[, "depth"] == closest(q$depth,4),][1,]
  layer6<- q[q[, "depth"] == closest(q$depth,5),][1,]
  layer7<- q[q[, "depth"] == closest(q$depth,6),][1,]
  layer8<- q[q[, "depth"] == closest(q$depth,7),][1,]
  layer9<- q[q[, "depth"] == closest(q$depth,8),][1,]
  layer10<-q[q[, "depth"] == closest(q$depth,9),][1,]
  layer11<- q[q[, "depth"] == closest(q$depth,9.2),][1,]
  
  temp<-rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11)
  temp[,((ncol(ysi))+1)] <- depths
  colnames(temp)[((ncol(ysi))+1)]<-"new_depth"
  final <- temp
  final <- data.frame(final)
  super_final_ysi <- rbind.fill.matrix(super_final_ysi,final)
}

detach(package:plyr)#to prevent issues with dplyr vs plyr not playing well together!

#now need to clean up the data frame and make all factors numeric
super_final_ysi1 <- as.data.frame(super_final_ysi) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  dplyr::filter(time > "2013-05-16") %>% #need to make sure that the CTD data only start after first day of sim
  mutate(depth = as.numeric(levels(depth))[depth]) %>%
  mutate(new_depth = as.numeric(levels(new_depth))[new_depth]) %>%
  mutate(temp = as.numeric(levels(temp))[temp]) %>%
  mutate(DO = as.numeric(levels(DO))[DO]) %>%
  mutate(diff = new_depth - depth) %>% 
  dplyr::filter(abs(diff)<0.4)%>%
  select(time, new_depth, temp, DO) %>%
  rename(depth = new_depth) 

more2 <- merge(super_final, super_final_ysi1, all.x=T, all.y=T, by=c("time", "depth"))

#visualize the data
for(i in 1:length(unique(more2$depth))){
  tempdf<-subset(more2, more2$depth==depths[i])
  plot(tempdf$time,tempdf$DO.y, type='l', col='red',
       ylab='Oxygen mmol/m3', xlab='time',
       main = paste0("YSI=Red,CTD=Black,Depth=",depths[i]),ylim=c(0,15))
  points(tempdf$time, tempdf$DO.x, type="l",col='black')
}

#remove outliers
more <- more2 %>% 
  dplyr::filter((depth == 0.1 & DO.x > 5) |
                (depth == 0.1 & DO.y > 5) |
                (depth == 1 & DO.x > 5.5) |
                (depth == 1 & DO.y > 5.5) |
                (depth == 2 & DO.x > 5) |
                (depth == 2 & DO.y > 5) |
                (depth == 3 & DO.x > 2) |
                (depth == 3 & DO.y > 2) |
                (depth == 4 & DO.x < 13) |
                (depth == 4 & DO.y < 13) |
                (depth == 5 & DO.x < 13) |
                (depth == 5 & DO.y < 13) |
                (depth == 6 & DO.x < 13) |
                (depth == 6 & DO.y < 13) |
                (depth == 7 & DO.x < 12.5) |
                (depth == 7 & DO.y < 12.5) |
                (depth == 8 & DO.x < 12.5) |
                (depth == 8 & DO.y < 12.5) |
                (depth == 9 & DO.x < 12.5) |
                (depth == 9 & DO.y < 12.5) |
                (depth == 9.2 & DO.x < 12.5) |
                (depth == 9.2 & DO.y < 12.5)) 
  
#visualize the DO data
for(i in 1:length(unique(more$depth))){
  tempdf<-subset(more, more$depth==depths[i])
  plot(tempdf$time,tempdf$DO.y, type='l', col='red',
       ylab='Oxygen mmol/m3', xlab='time',
       main = paste0("YSI=Red,CTD=Black,Depth=",depths[i]),ylim=c(0,15))
  points(tempdf$time, tempdf$DO.x, type="l",col='black')
}

#combine DO & temp data from both YSI & CTD, defaults to CTD if both are present 
for(i in 1:length(more$time)){
  if(is.na(more$temp.x[i])==F){
    more$temp_new[i]=more$temp.x[i]
  }
  if(is.na(more$temp.x[i])==T & is.na(more$temp.y[i])==F){
    more$temp_new[i]=more$temp.y[i]
  }
  if(is.na(more$temp.x[i])==T & is.na(more$temp.y[i])==T){
    more$temp_new[i]=NA
  }
  if(is.na(more$DO.x[i])==F){
    more$DO_new[i]=more$DO.x[i]
  }
  if(is.na(more$DO.x[i])==T & is.na(more$DO.y[i])==F){
    more$DO_new[i]=more$DO.y[i]
  }
  if(is.na(more$DO.x[i])==T & is.na(more$DO.y[i])==T){
    more$DO_new[i]=NA
  }
}     

more1 <- more %>% 
  select(time, depth,temp_new,DO_new, chla) %>% 
  rename(temp = temp_new, DO = DO_new) 

#visualize the DO data
for(i in 1:length(unique(more1$depth))){
  tempdf<-subset(more1, more1$depth==depths[i])
  plot(tempdf$time,tempdf$DO, type='l', col='red',
       ylab='Oxygen mmol/m3', xlab='time',
       main = paste0("Combined CTD & YSI data,Depth=",depths[i]),ylim=c(0,15))
}

#visualize the temp data
for(i in 1:length(unique(more1$depth))){
  tempdf<-subset(more1, more1$depth==depths[i])
  plot(tempdf$time,tempdf$temp, type='l', col='red',
       ylab='Temp oC', xlab='time',
       main = paste0("Combined CTD & YSI data,Depth=",depths[i]),ylim=c(0,30))
}

#visualize the chla data
for(i in 1:length(unique(more1$depth))){
  tempdf<-subset(more1, more1$depth==depths[i])
  plot(tempdf$time,tempdf$chla, type='l', col='red',
       ylab='Chla ug/L', xlab='time',
       main = paste0("Combined CTD & YSI data,Depth=",depths[i]),ylim=c(0,30))
}


#export CTD data!
temp <- more1 %>%
  select(time, depth, temp) %>%
  dplyr::filter(!(time==as_date("2014-05-28") & depth == 9.2)) %>% 
  dplyr::filter(!(time==as_date("2017-03-27") & depth == 8)) %>% 
  dplyr::filter(!(time==as_date("2013-08-12") & depth == 7)) %>% 
  dplyr::filter(!(time==as_date("2016-06-28") & depth == 7)) %>% 
  dplyr::filter(!(time==as_date("2013-08-12") & depth == 6)) %>% 
  dplyr::filter(!(time==as_date("2014-05-28") & depth == 6)) %>% 
  dplyr::filter(!(time==as_date("2013-08-12") & depth == 5)) %>% 
  dplyr::filter(!(time==as_date("2013-08-12") & depth == 4)) %>% 
  dplyr::filter(!(time==as_date("2013-08-12") & depth == 3)) %>% 
  dplyr::filter(!(time==as_date("2014-05-28") & depth == 3)) %>% 
  dplyr::filter(!(time==as_date("2013-08-12") & depth == 2)) %>% 
  dplyr::filter(!(time==as_date("2014-05-28") & depth == 2)) %>% 
  dplyr::filter(!(time==as_date("2014-05-28") & depth == 1)) %>% 
  rename(DateTime = time, Depth = depth) %>% 
  drop_na() %>% 
  write.csv("CleanedObsTemp.csv", row.names = F)

oxygen <- more1 %>%
  select(time, depth, DO) %>%
  rename(DateTime = time, Depth = depth, OXY_oxy=DO) %>%
  mutate(OXY_oxy = OXY_oxy*1000/32) %>% #to convert mg/L to molar units
  drop_na() %>% 
  write.csv("CleanedObsOxy.csv", row.names = F)

chla <- more1 %>%
  select(time, depth, chla) %>%
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 9.2)) %>% 
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 9)) %>% 
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 8)) %>% 
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 7)) %>% 
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 6)) %>% 
  dplyr::filter(!(time==as_date("2016-07-26") & depth == 6)) %>% 
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 5)) %>% 
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 2)) %>% 
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 1)) %>% 
  dplyr::filter(!(time==as_date("2015-04-16") & depth == 0.1)) %>% 
  rename(DateTime = time, Depth = depth, PHY_TCHLA=chla) %>%
  drop_na() %>% 
  write.csv("CleanedObsChla.csv", row.names = F)

#fcr <- more1 %>%
#  select(time, depth, temp, DO) %>%
#  rename(DateTime = time, Depth = depth, OXY_oxy=DO) %>%
#  mutate(OXY_oxy = OXY_oxy*1000/32) %>% #to convert mg/L to molar units
#  write.csv("field_FCR.csv", row.names = F)


###########################################################
###### WATER CHEM DATA FROM EDI

#now let's build a chemistry field_data file
#first pull in FCR chem data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/6/2b3dc84ae6b12d10bd5485f1c300af13" 
infile1 <- paste0(getwd(),"/chem.csv")
download.file(inUrl1,infile1,method="curl")

FCRchem <- read.csv("chem.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth_m, NH4_ugL:DIC_mgL) %>%
  rename(Depth=Depth_m) %>%
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10) %>% #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
  mutate(OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90) %>% #assuming 90% of total DOC is in recalcitrant DOC pool
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
  #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
  select(DateTime, Depth, NIT_amm:CAR_dic) %>%
  drop_na(NIT_amm) %>%
  filter(OGM_docr<500) %>% #remove high DOC outliers
  distinct(DateTime, Depth, .keep_all=TRUE)
 
ggplot(FCRchem, aes(DateTime, OGM_docr, colour=Depth)) + 
  geom_point()

write.csv(FCRchem, "field_chem_2DOCpools.csv", row.names = F)


#######now make FCR chem dataset with one DOC pool
FCRchem <- read.csv("chem.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth_m, NH4_ugL:DIC_mgL) %>%
  rename(Depth=Depth_m) %>%
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)) %>% 
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
  #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
  select(DateTime, Depth, NIT_amm:CAR_dic) %>%
  drop_na(NIT_amm) %>%
  distinct(DateTime, Depth, .keep_all=TRUE)

ggplot(FCRchem, aes(DateTime, OGM_doc, colour=Depth)) + 
  geom_point()

write.csv(FCRchem, "field_chem_1DOCpool.csv", row.names = F)

#######now make totals chemistry files
FCRchem <- read.csv("chem.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth_m, TN_ugL:TP_ugL) %>%
  rename(Depth=Depth_m) %>%
  mutate(TOT_tn = TN_ugL*1000*0.001*(1/14.0067)) %>% 
  mutate(TOT_tp = TP_ugL*1000*0.001*(1/30.9738)) %>% 
  select(DateTime, Depth, TOT_tn, TOT_tp) %>%
  distinct(DateTime, Depth, .keep_all=TRUE)

ggplot(FCRchem, aes(DateTime, TOT_tn, colour=Depth)) + 
  geom_point()

write.csv(FCRchem, "totalNP.csv", row.names = F)


###########################################################
###### ANCILLARY LAB CHEMISTRY DATASETS NEEDED FOR CALIBRATION 

#read in lab dataset of dissolved silica, measured in summer 2014 only
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/542/1/791ec9ca0f1cb9361fa6a03fae8dfc95" 
infile1 <- paste0(getwd(),"/silica_master_df.csv")
download.file(inUrl1,infile1,method="curl")

silica <- read.csv("silica_master_df.csv", header=T) %>%
  dplyr::filter(Reservoir == "FCR") %>% 
  dplyr::filter(Site == 50) %>% #50 = weir inflow site
  select(DateTime, DRSI_mgL) %>%
  rename(DateTime = Date, SIL_rsi=DRSI_mgL) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(SIL_rsi = SIL_rsi*1000*(1/60.08)) #convert to molar units
#write.csv(silica, "field_silica.csv", row.names = F)

#read in lab dataset of dissolved methane concentrations, measured in FCR
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/551/5/38d72673295864956cccd6bbba99a1a3" 
infile1 <- paste0(getwd(),"/Dissolved_CO2_CH4_Virginia_Reservoirs.csv")
download.file(inUrl1,infile1,method="curl")

ch4 <- read.csv("Dissolved_CO2_CH4_Virginia_Reservoirs.csv", header=T) %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site == 50) %>% #to remove weir inflow site
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(Depth = Depth_m, CAR_ch4 = ch4_umolL, CAR_pCO2 = co2_umolL) %>%
  select(DateTime, Depth, CAR_ch4, CAR_pCO2) %>%
  mutate(CAR_pCO2 = CAR_pCO2*(0.0018/1000000)/0.0005667516) %>% #to convert umol/L to pCO2
  group_by(DateTime, Depth) %>%
  summarise(CAR_pCO2=mean(CAR_pCO2), CAR_ch4=mean(CAR_ch4))
#write.csv(ch4, "field_gases.csv", row.names=F)

###########################################################
###### SECCHI DATA FROM EDI

#first pull in Secchi data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/198/7/01c3762d9d2c4069eeb3dc10aa236c47" 
infile1 <- paste0(getwd(),"/Secchi_depth_2013-2019.csv")
download.file(inUrl1,infile1,method="curl")
#note that something's funky with this file- I had to open it up and re-format dates before it could be used

secchi <- read.csv("Secchi_depth_2013-2019.csv", header=T) %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  dplyr::filter(Flag_Secchi==0) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth = rep(1, length(DateTime))) %>%
  mutate(extc_coef = Secchi_m/1.7) %>%
  select(DateTime, Depth, Secchi_m, extc_coef)
write.csv(secchi, "field_secchi.csv", row.names=F)
  
  