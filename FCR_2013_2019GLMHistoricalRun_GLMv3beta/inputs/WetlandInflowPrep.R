#*****************************************************************                                                           *
#* TITLE:   Falling Creek Reservoir Precipitation-driven Inflow 
#*          Calculations                                         *
#* AUTHORS:  B. Steele, modified by N.K. Ward, W.M. Woelmer,     *
#*              & C.C. Carey                                     *
#* DATE:   Last modified 26 Sept 2021                             *
#* NOTES:  W.M. Woelmer modified B. Steele and N.K. Ward's code 
#*         originally developed for Lake Sunapee 
#*         for CCC to estimate reservoir inflows for FCR; 
#*         CCC subsequently edited on 12 June 2020 and made tidy,
#*         with subsequent tweaks to annotation in summer 2021
#*****************************************************************

#This script is used to calculate the precipitation-driven flow (runoff) that 
# enters into Falling Creek Reservoir both through the weir inflow (Tunnel Branch)
# and wetland inflow (Falling Creek).


setwd("FCR_2013_2019GLMHistoricalRun_GLMv3beta/inputs")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(EcoHydRology)

#add final theme for formatting ggplots
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=16, face='bold'))

#### bring in NLDAS-2 climate data for FCR ####
MetData <- read.csv('FCR_GLM_NLDAS_010113_123119_GMTadjusted.csv', header=T, na.strings=NA) %>% 
  rename(dateTime = time) %>% 
  mutate(dateTime = as.POSIXct(dateTime, format='%Y-%m-%d %H:%M')) %>% 
  mutate(AirTemp.F = AirTemp*1.8 + 32) %>% #convert to F for Schuler eqn calcs
  mutate(Rain.m_hr = Rain/24) #convert to rain in m/hr 


#### ---- RUNOFF AND SNOW MELT CALCS ---- ####

#calculate runoff coefficients (Rv) and store as values in workspace
# Rv = 0.05 + 0.9Ia 
#Ia = proportion of drainage area that is impervious (in decimal form)

# FCR refers to the whole watershed
# Inf refers to the inflow to FCR from BVR where the weir is (Tunnel Branch)
# Wet refers to the wetland inflow at the north side of the reservoir (Falling Creek)

impFCR <- 0.0000437 # Ia #number calculated from StreamStats watershed delineation model
RvFCR = 0.05 + 0.9 * impFCR #multiply by assumed variable
impInf <- 0  # number calculated from StreamStats watershed delineation model
RvInf = 0.05 + 0.9 * impInf
impWet <- 0
RvWet <- 0.05 +0.9*impWet

##### SNOW ACCUMULATION #####
# If precipitation is falling when the air temperature is less than or equal
# to 0 degrees celsius, then the depth of precipition is stored as SWE
# (Snow Water Equivalents) 

#### Snow Melt #####
# If T(degrees celsius) is greater than 0, the precipitation falls as rain
# and any accumulated SWE contributes to runoff according to the relationship 
# from Part 630 Hydrology National Engineering Handbook with equations converted
# from US to metric:
# melt (meters) = (Cm * ((T * 1.8 + 32) - 32))
# SWE contribution to runoff = melt * Cwr
# Cm (melt rate coefficient) = 0.001524 meters of melt per degree day
# Cwr (winter runoff coefficient) = 0.5

# Degree day method of snowmelt modeling:
# http://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/NEHhydrology/ch11.pdf
# also referred to as the HEC-1 here: http://www.dtic.mil/dtic/tr/fulltext/u2/a366395.pdf
# 

# Table 6-1 http://www.publications.usace.army.mil/Portals/76/Publications/EngineerManuals/EM_1110-2-1406.pdf
# Relative Magnitude of Melt-Rate Factors (Refer to Table 5-4)
# Case  -  in./deg F - comment - cm/deg F
# 1 - 0.068 Clear, low albedo - 0.02677165
# 2 - 0.073 Case 1, 40% forest -  0.02874016
# 3 - 0.040 Case 1, cloud cover - 0.01574803
# 4 - 0.046 Case 1, fresh snow - 0.01811024
#- - - - - - - - - - - - - - - - - - - - - - -- - - - - -
# 5 - 0.180 Heavy rain, windy - 0.07086614
# 6 - 0.163 Light rain, windy - 0.06417323
# 7 - 0.062 Light rain, light wind - 0.02440945
  
# heavy rain is 3in/hr = 0.0762 m/h
# windy is > 15mph = 6.706 mps

case <- (c(3, 5, 6, 7)) 
melt_coeff <- c(0.0001574803, 0.0007086614, 0.0006417323, 0.0002440945) 
case_melt <- data.frame(case, melt_coeff)  

#assign melt cases to met data  
MetData_sub <- subset(MetData, select=c('dateTime', 'AirTemp', 'AirTemp.F', 'WindSpeed', 'Rain.m_hr')) #subset for needed variables 
MetData_sub$melt_case <- as.numeric(NA) #create column to fill in with melting case 
str(MetData_sub)

#assign case number by temp, rain and wind data -
for (i in 1:nrow(MetData_sub)){
 if(MetData_sub$AirTemp[i] <= 0){  #if temperature is at or below zero there is no melt, only accumulation
   MetData_sub$melt_case[i] <- NA} else  #therefore there is no melt case applicable
     if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]>=0.0762 & MetData_sub$WindSpeed[i]>=6.706) { #temp >0 and heavy rain and windy, case 5
       MetData_sub$melt_case[i] <- 5} else
         if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]>=0.0762 & MetData_sub$WindSpeed[i]<6.706) { #temp >0 and heavy rain and light windy, case 6
           MetData_sub$melt_case[i] <- 6} else
             if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]<0.0762 & MetData_sub$WindSpeed[i]>=6.706) { #temp >0 and light rain and windy, case 6
               MetData_sub$melt_case[i] <- 6} else
                 if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]<0.0762 &MetData_sub$Rain.m_hr[i]>0 & MetData_sub$WindSpeed[i]<6.706) { #temp >0 and light rain and windy, case 6
                   MetData_sub$melt_case[i] <- 7} else
                     MetData_sub$melt_case[i] <- 3 #any other temperature, rain and windspeed combination would be
}

#merge with case data for melt coefficient
MetData_sub <- merge(MetData_sub, case_melt, by.x='melt_case', by.y='case', all.x=T) 

MetData_sub <- MetData_sub[with(MetData_sub, order(dateTime)), ] #re-order by date (R sorts it by melt case and it needs to be in date order for next step)
MetData_sub$SWE <- 0 # column to store SWE (snow accumulation as a depth of water) 
MetData_sub$runoff_SWE <- 0  # column of SWE contribution to runoff 
str(MetData_sub) #check work 

####---- RUNOFF DATA BY SUB WATERSHED ---- ####
# R = P * Pj * Rv 
# Where: 
# R =  runoff (meters) 
# P =  rainfall (meters) 
# Pj = Fraction of  rainfall events that produce runoff (usually 0.9) 
# Rv = Runoff coefficient 

# melt partitioning between infiltration and runoff generation should be
# slightly higher than assumption for non-frozen periods (if we are using 40%
# infiltration for non-winter conditions, let's use 50% for winter to account for 
# some ground being frozen). !!!! I am very open to adjusting this percent !!!! ##NKW: <-I think this is worded backwards?
# using the above logic, we add 0.1 to the Rv value to calculate the Cwr value
# calculate winter runoff coefficient based on land cover type - adding 10% as 
# we assume ground is frozen or saturated
CwrFCR <- RvFCR + 0.1
CwrInf <- RvInf + 0.1
CwrWet <- RvWet + 0.1


#runoff_reg_m and runoff_reg_m_fro are calculated for all times so that values can be applied in the for loop in the next step
MetData_FCR <- MetData_sub #copy into new dataframe
MetData_FCR$runoff_reg_m <- MetData_FCR$Rain.m_hr * RvFCR * 0.9 #apply runoff coefficient specific to subwatershed
MetData_FCR$runoff_reg_m_fro <- MetData_FCR$Rain.m_hr * (CwrFCR) * 0.9 #caluclate winter/frozen runoff specific to subwatershed

MetData_Inf <- MetData_sub
MetData_Inf$runoff_reg_m <- MetData_Inf$Rain.m_hr * RvInf * 0.9
MetData_Inf$runoff_reg_m_fro <- MetData_Inf$Rain.m_hr * (CwrInf) * 0.9

MetData_Wet <- MetData_sub
MetData_Wet$runoff_reg_m <- MetData_Wet$Rain.m_hr * RvWet * 0.9
MetData_Wet$runoff_reg_m_fro <- MetData_Wet$Rain.m_hr * (CwrWet) * 0.9


#### FCR total watershed Runoff ####
#calculate snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_FCR)){                                        # for every row (except the first)
    if(MetData_FCR$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_FCR$Rain.m_hr[i] + MetData_FCR$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_FCR$SWE[i] <- swe_index 
  } else { 
    if ((MetData_FCR$AirTemp[i]>0) & (MetData_FCR$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_FCR$melt_coeff[i] * (MetData_FCR$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_FCR$SWE[i-1])                # in case melt value is greater 
      MetData_FCR$SWE[i] <- MetData_FCR$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- CwrFCR*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_FCR$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_FCR$runoff_sum <- 0

for (i in 1:nrow(MetData_FCR)) {
  if (MetData_FCR$AirTemp[i]>0 & MetData_FCR$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_FCR$runoff_reg_m_fro[i] + MetData_FCR$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_FCR$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_FCR$AirTemp[i]>0 & MetData_FCR$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_FCR$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_FCR$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### Inf: FCR Weir (from BVR, Tunnel Branch) Inflow Runoff ####
#calculate snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_Inf)){                                        # for every row (except the first)
  if(MetData_Inf$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_Inf$Rain.m_hr[i] + MetData_Inf$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_Inf$SWE[i] <- swe_index 
  } else { 
    if ((MetData_Inf$AirTemp[i]>0) & (MetData_Inf$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_Inf$melt_coeff[i] * (MetData_Inf$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_Inf$SWE[i-1])                # in case melt value is greater 
      MetData_Inf$SWE[i] <- MetData_Inf$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- CwrInf*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_Inf$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_Inf$runoff_sum <- 0

for (i in 1:nrow(MetData_Inf)) {
  if (MetData_Inf$AirTemp[i]>0 & MetData_Inf$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_Inf$runoff_reg_m_fro[i] + MetData_Inf$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_Inf$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_Inf$AirTemp[i]>0 & MetData_Inf$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_Inf$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_Inf$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### Wet Runoff: the wetland inflow at the north end of the reservoir ####
#calculate snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_Wet)){                                        # for every row (except the first)
  if(MetData_Wet$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_Wet$Rain.m_hr[i] + MetData_Wet$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_Wet$SWE[i] <- swe_index 
  } else { 
    if ((MetData_Wet$AirTemp[i]>0) & (MetData_Wet$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_Wet$melt_coeff[i] * (MetData_Wet$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_Wet$SWE[i-1])                # in case melt value is greater 
      MetData_Wet$SWE[i] <- MetData_Wet$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- CwrWet*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_Wet$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_Wet$runoff_sum <- 0

for (i in 1:nrow(MetData_Wet)) {
  if (MetData_Wet$AirTemp[i]>0 & MetData_Wet$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_Wet$runoff_reg_m_fro[i] + MetData_Wet$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_Wet$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_Wet$AirTemp[i]>0 & MetData_Wet$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_Wet$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_Wet$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

####************* scale up to WS and sub WS m3/day over all area **************####
# areas in hectares
areaFCR <- 354.8
areaInf <- 77.7
areaWet <- 163.2


MetData_FCR$total_runoff_m3_hr <- MetData_FCR$runoff_sum * areaFCR * 10000 #runoff in m. area is in ha. one ha is 10000m 
MetData_Inf$total_runoff_m3_hr <- MetData_Inf$runoff_sum * areaInf * 10000
MetData_Wet$total_runoff_m3_hr <- MetData_Wet$runoff_sum * areaWet * 10000


# add column for date for aggregation
MetData_FCR$date <- as.Date(format(MetData_FCR$dateTime, '%Y-%m-%d'))
MetData_Inf$date <- as.Date(format(MetData_Inf$dateTime, '%Y-%m-%d'))
MetData_Wet$date <- as.Date(format(MetData_Wet$dateTime, '%Y-%m-%d'))

# summarize runoff/area by day
MetData_FCR_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_FCR, FUN = sum)) ; names(MetData_FCR_daily) <- c("date", "total_runoff_m3d")
MetData_Inf_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_Inf, FUN = sum)) ; names(MetData_Inf_daily) <- c("date", "total_runoff_m3d")
MetData_Wet_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_Wet, FUN = sum)) ; names(MetData_Wet_daily) <- c("date", "total_runoff_m3d")

MetData_FCR_daily$total_runoff_model_imp_50_50_m3d<-NA
MetData_Inf_daily$total_runoff_model_imp_50_50_m3d<-NA
MetData_Wet_daily$total_runoff_model_imp_50_50_m3d<-NA

# calculate discharge as proportion of runoff from previous days (because only 50% makes it there)
for(i in 2: nrow(MetData_FCR_daily)) {
  model_50_50 =  MetData_FCR_daily$total_runoff_m3d [i] * .5 + MetData_FCR_daily$total_runoff_m3d [i-1] * .5
  MetData_FCR_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_Inf_daily)) {
  model_50_50 =  MetData_Inf_daily$total_runoff_m3d [i] * .5 + MetData_Inf_daily$total_runoff_m3d [i-1] * .5
  MetData_Inf_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_Wet_daily)) {
  model_50_50 =  MetData_Wet_daily$total_runoff_m3d [i] * .5 + MetData_Wet_daily$total_runoff_m3d [i-1] * .5
  MetData_Wet_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}


#reality check
sum(MetData_FCR_daily$total_runoff_model_imp_50_50_m3d, na.rm = T)
sum(MetData_Inf_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + sum(MetData_Wet_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) 

## convert to m3/s
MetData_FCR_daily$total_runoff_model_imp_50_50_m3s <- MetData_FCR_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_Inf_daily$total_runoff_model_imp_50_50_m3s <- MetData_Inf_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_Wet_daily$total_runoff_model_imp_50_50_m3s <- MetData_Wet_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)

#subset for desired variables
MetData_FCR_daily_sub <- subset(MetData_FCR_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_Inf_daily_sub <- subset(MetData_Inf_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_Wet_daily_sub <- subset(MetData_Wet_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))

MetData_FCR_daily_sub$Site <- 'whole' 
MetData_Inf_daily_sub$Site <- 'weir' #Tunnel Branch precipitation-driven flow
MetData_Wet_daily_sub$Site <- 'wetland' #Falling Creek precipitation-driven flow

wsrunoff <- merge(MetData_Inf_daily_sub, MetData_Wet_daily_sub, by="date") %>% 
  filter(date > "2013-05-14") %>% 
  rename(DateTime = date, weir_runoff_m3s = total_runoff_model_imp_50_50_m3s.x, 
         wetland_runoff_m3s = total_runoff_model_imp_50_50_m3s.y) %>% 
  select(DateTime, weir_runoff_m3s, wetland_runoff_m3s)
write.csv(wsrunoff, "WetlandWeir_Runoff_FCR_20200615.csv", row.names = FALSE)

