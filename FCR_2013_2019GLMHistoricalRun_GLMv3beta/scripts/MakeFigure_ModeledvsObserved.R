#to compare modeled vs observed for 9m in FCR
#time series of concentrations

#load packages
library(zoo)
library(tidyverse)
library(lubridate)
#library(gganimate)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#set GLM output files for analysis & figures

#baseline scenario, based on observed SSS practices
nc_file <- file.path(sim_folder, 'output/output_2013_2019.nc')

#set up figure
pdf("figures/TimeSeries_ModeledVsObserved.pdf", width=8.5, height=11)
par(mfrow=c(4,2))

####water temp####
obstemp<-read_csv('field_data/CleanedObsTemp.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  filter(Depth==9)

modtemp <- get_temp(nc_file, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

plot(obstemp$DateTime, obstemp$temp, type='p', col='red',
  ylab='Water temperature (oC)', xlab='time',
  main = "9m Water Temp, RMSE = 1.32oC",ylim=c(0,20))
points(modtemp$DateTime, modtemp$temp, type="l",col='black')

#oxygen
obs_oxy<-read.csv('field_data/CleanedObsOxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  filter(Depth==9)

mod_oxy <- get_var(nc_file, "OXY_oxy", reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 
  
plot(obs_oxy$DateTime,obs_oxy$OXY_oxy, type='p', col='red',
  ylab='Oxygen mmol/m3', xlab='time',
  main = "9m DO, RMSE = 100.5 mmol/m3", ylim=c(0,500))
points(mod_oxy$DateTime, mod_oxy$OXY_oxy, type="l",col='black')
  
####NH4####
var="NIT_amm"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 
obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9) %>% 
  na.omit()

mod<- get_var(nc_file, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

plot(obs$DateTime,obs$NIT_amm, type='p', col='red',
    ylab="NH4 mmol/m3", xlab='time',
    main = "9m NH4, RMSE = 15.3 mmol/m3")
points(mod$DateTime, mod$NIT_amm, type="l",col='black')

####NO3####
var="NIT_nit"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 
obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9) %>% 
  na.omit()

mod<- get_var(nc_file, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

plot(obs$DateTime,obs$NIT_nit, type='p', col='red',
     ylab="NO3 mmol/m3", xlab='time', ylim=c(0,1.4),
     main = "9m NO3, RMSE = 0.37 mmol/m3")
points(mod$DateTime, mod$NIT_nit, type="l",col='black')

####FRP####
var="PHS_frp"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 
obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9) %>% 
  na.omit()

mod<- get_var(nc_file, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  filter(PHS_frp>0.0001)

plot(obs$DateTime,obs$PHS_frp, type='p', col='red',
     ylab="FRP mmol/m3", xlab='time', ylim=c(0,0.3),
     main = "9m FRP, RMSE = 0.057 mmol/m3")
points(mod$DateTime, mod$PHS_frp, type="l",col='black')

####DOC + DOCr####
var="OGM_doc"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 
obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9) %>% 
  na.omit()

mod <- get_var(nc_file, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

var="OGM_docr"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 
obsR<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9) %>% 
  na.omit()

modR<- get_var(nc_file, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

obsDOC <- merge(obs, obsR, by="DateTime") %>% 
  mutate(DOCall = OGM_doc + OGM_docr)

modDOC <-merge(mod,modR, by="DateTime") %>% 
  mutate(DOCall = OGM_doc + OGM_docr)

plot(obsDOC$DateTime,obsDOC$DOCall, type='p', col='red',
     ylab="DOC (both pools summed) mmol/m3", xlab='time', ylim=c(50,500),
     main = "9m DOC, RMSE = 57.4 mmol/m3")
points(modDOC$DateTime, modDOC$DOCall, type="l",col='black')



####totals####

TN <- get_var(nc_file, "TOT_tn",z_out=9,reference = 'surface')
TP <- get_var(nc_file, "TOT_tp",z_out=9,reference = 'surface')
TOC <- get_var(nc_file, "TOT_toc",z_out=9,reference = 'surface')

cyano <- get_var(nc_file,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoN = CyanoConc*0.12,
         cyanoP = CyanoConc*0.0005,
         cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoN, cyanoP, cyanoC)

green <- get_var(nc_file,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenN = GreenConc*0.12,
         greenP = GreenConc*0.0005,
         greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenN, greenP, greenC)

diatom <- get_var(nc_file,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomN = DiatomConc*0.12,
         diatomP = DiatomConc*0.0005,
         diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth, diatomN, diatomP, diatomC)

data<-as.data.frame(cbind(diatom[1:5],cyano[,3:5],green[,3:5],TN[,2],TP[,2],TOC[,2])) 
colnames(data) = c("DateTime", "Depth", "diatomN", "diatomP", "diatomC",
                   "cyanoN", "cyanoP", "cyanoC", "greenN", "greenP", "greenC",
                   "TN", "TP", "TOC") 
data1 <- data %>% 
  mutate(totalN = TN + diatomN + cyanoN + greenN,
         totalP = TP + diatomP + cyanoP + greenP,
         totalC = TOC + diatomC + cyanoC + greenC) %>% 
  select(DateTime, totalN:totalC) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

obs<-read.csv('field_data/totalNP.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  filter(Depth==9) %>% 
  na.omit()

#total N
plot(obs$DateTime,obs$TOT_tn, type='p', col='red',
     ylab="TN mmol/m3", xlab='time', ylim=c(0,160),
     main = "9m TN, RMSE = 4.16 mmol/m3")
points(data1$DateTime, data1$totalN, type="l",col='black')

plot(obs$DateTime,obs$TOT_tp, type='p', col='red',
     ylab="TP mmol/m3", xlab='time', ylim=c(0,5),
     main = "9m TP, RMSE = 0.68 mmol/m3")
points(data1$DateTime, data1$totalP, type="l",col='black')

dev.off()
