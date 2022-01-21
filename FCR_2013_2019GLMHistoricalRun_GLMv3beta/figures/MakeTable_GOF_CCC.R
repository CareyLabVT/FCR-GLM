#*****************************************************************
#* TITLE:   FCR GLM-AED script to create goodness-of-fit tables            
#* AUTHORS:  A.G. Hounshell and Cayelan C. Carey                                       
#* DATE:   Originally developed by AGH 12 Jan 2021; updated by CCC for ms revision
#* NOTES:  This script creates output tables needed for both the main 
#*         manuscript and SI 
#*****************************************************************

# devtools::install_github("CareyLabVT/GLMr", force = TRUE) 
# devtools::install_github("CareyLabVT/glmtools", force = TRUE)

# Load in libraries
# pacman::p_load(devtools)
pacman::p_load(zoo,tidyverse,lubridate,hydroGOF,GLMr,glmtools,ggplot2)

# Set working directory
setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
sim_folder <- getwd()

# Using baseline model data (aka: no oxygenation scenarios)

# Load in nc_file and needed variables
baseline <- file.path(sim_folder, 'output/output_2013_2019.nc')
field <- file.path(sim_folder, 'field_data/CleanedObsTemp.csv')
thermo_field <- file.path(sim_folder,'field_data/CleanedObsTemp_thermo.txt')

obs_temp <- read_csv('field_data/CleanedObsTemp.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(temp_obs = temp)
obs_oxy <- read.csv('field_data/CleanedObsOxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(oxy_obs = OXY_oxy)
obs_NIT_amm <- read.csv('field_data/field_chem.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  select(DateTime,Depth,NIT_amm) %>% 
  rename(NIT_amm_obs = NIT_amm)
obs_NIT_nit <- read.csv('field_data/field_chem_2DOCpools.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  select(DateTime,Depth,NIT_nit) %>% 
  rename(NIT_nit_obs = NIT_nit)
obs_PHS_frp <- read.csv('field_data/field_chem.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  select(DateTime,Depth,PHS_frp) %>% 
  rename(PHS_frp_obs = PHS_frp)
obs_allDOC <- read.csv('field_data/field_chem.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  select(DateTime,Depth,OGM_doc,OGM_docr) %>% 
  rename(DOC_obs = OGM_doc,rDOC_obs = OGM_docr) %>% 
  mutate(allDOC_obs = DOC_obs + rDOC_obs)
obs_Totals <- read.csv('field_data/totalNP.csv',header=TRUE) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  rename(TN_obs = TOT_tn, TP_obs = TOT_tp) %>% 
  dplyr::filter(Depth %in% c(0.1,1.6,3.8,5.0,6.2,8.0,9.0))

# Find necessary parameters
print(sim_vars(baseline))

############################# Full year, full water column GOF statistics ############################

# Create empty dataframe for GOF results
# Using all GOF parameters for now - will cull eventually

# Full year, full water column, full period (2013-2019)
all_gof <- setNames(data.frame(matrix(ncol=10,nrow=22)),c("Parameter","Temp","Thermo","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","NRMSE%_all","PBIAS%_all","RSR_all","rSD_all","NSE_all","mNSE_all","rNSE_all","d_all","md_all","rd_all","cp_all","r_all",
                          "R2_all","bR2_all","KGE_all","VE_all","r.Spearman","nonparamR2")

# Full year, full water column, calibration period (2013-2018)
all_gof_cal <- setNames(data.frame(matrix(ncol=10,nrow=22)),c("Parameter","Temp","Thermo","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_gof_cal$Parameter <- c("ME_cal","MAE_cal","MSE_cal","RMSE_cal","NRMSE%_cal","PBIAS%_cal","RSR_cal","rSD_cal","NSE_cal","mNSE_cal","rNSE_cal","d_cal","md_cal","rd_cal","cp_cal","r_cal",
                              "R2_cal","bR2_cal","KGE_cal","VE_cal","r.Spearman","nonparamR2")

# Full year, full water column, validation period (2019)
all_gof_val <- setNames(data.frame(matrix(ncol=10,nrow=22)),c("Parameter","Temp","Thermo","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_gof_val$Parameter <- c("ME_val","MAE_val","MSE_val","RMSE_val","NRMSE%_val","PBIAS%_val","RSR_val","rSD_val","NSE_val","mNSE_val","rNSE_val","d_val","md_val","rd_val","cp_val","r_val",
                              "R2_val","bR2_val","KGE_val","VE_val","r.Spearman","nonparamR2")

#### TEMP ####
# 9 m Obs temp
obs_temp$Depth <- as.numeric(obs_temp$Depth)
obs_temp_cal <- obs_temp %>% 
  dplyr::filter(DateTime < "2019-01-01")
obs_temp_val <- obs_temp %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Full water column Model temp
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
mod_temp <- get_temp(baseline, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(temp_mod = temp)
mod_temp$Depth <- as.numeric(mod_temp$Depth)
mod_temp_cal <- mod_temp %>% 
  dplyr::filter(DateTime < "2019-01-01")
mod_temp_val <- mod_temp %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later)
# For FULL year
comb_temp <- left_join(obs_temp,mod_temp,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(temp_obs = mean(temp_obs), temp_mod = mean(temp_mod)) %>% 
  ungroup()
all_gof$Temp[1:21] <- gof(comb_temp$temp_mod,comb_temp$temp_obs,do.spearman = TRUE)

#now need to calculate non-parametric (ranked) R2, following Brett et al. 2016
comb_temp_rank <- comb_temp %>% 
  mutate(rank_obs = rank(temp_obs),
         rank_mod = rank(temp_mod)) 
all_gof$Temp[22] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared

#calibration years only
comb_temp_cal <- left_join(obs_temp_cal,mod_temp_cal,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(temp_obs = mean(temp_obs), temp_mod = mean(temp_mod)) %>% 
  ungroup()
all_gof_cal$Temp[1:21] <- gof(comb_temp_cal$temp_mod,comb_temp_cal$temp_obs,do.spearman = TRUE)

comb_temp_cal_rank <- comb_temp_cal %>% 
  mutate(rank_obs = rank(temp_obs),
         rank_mod = rank(temp_mod)) 
all_gof_cal$Temp[22] <- summary(lm(comb_temp_cal_rank$rank_obs ~ comb_temp_cal_rank$rank_mod))$r.squared

#validation years only
comb_temp_val <- left_join(obs_temp_val,mod_temp_val,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(temp_obs = mean(temp_obs), temp_mod = mean(temp_mod)) %>% 
  ungroup()
all_gof_val$Temp[1:21] <- gof(comb_temp_val$temp_mod,comb_temp_val$temp_obs,do.spearman = TRUE)

comb_temp_val_rank <- comb_temp_val %>% 
  mutate(rank_obs = rank(temp_obs),
         rank_mod = rank(temp_mod)) 
all_gof_val$Temp[22] <- summary(lm(comb_temp_val_rank$rank_obs ~ comb_temp_val_rank$rank_mod))$r.squared


#### Thermocline ####
# obs thermo - calculated using LakeAnalyzer
field_thermo <- load.ts(thermo_field, tz = "EST")
field_thermo <- field_thermo %>% 
  pivot_wider(names_from = depth,names_prefix = "temp_",values_from = temp)

field_thermo_m <- ts.thermo.depth(field_thermo,na.rm=TRUE)

field_thermo_m <- field_thermo_m %>% 
  rename(field_thermo_m = thermo.depth)

field_thermo_m_cal <- field_thermo_m %>% 
  dplyr::filter(datetime < "2019-01-01")

field_thermo_m_val <- field_thermo_m %>% 
  dplyr::filter(datetime >= "2019-01-01")

# model thermo - also calculated using LakeAnalyzer
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
mod_thermo <- get_temp(baseline, reference="surface", z_out=depths) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

mod_thermo_m <- ts.thermo.depth(mod_thermo,na.rm=TRUE)

mod_thermo_m <- mod_thermo_m %>% 
  rename(mod_thermo_m = thermo.depth)

mod_thermo_m_cal <- mod_thermo_m %>% 
  dplyr::filter(datetime < "2019-01-01")

mod_thermo_m_val <- mod_thermo_m %>% 
  dplyr::filter(datetime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters
#full period
comb_thermo <- left_join(field_thermo_m,mod_thermo_m,by="datetime") %>% 
  mutate(week = week(datetime),
         year = year(datetime),
         month = month(datetime)) %>% 
  dplyr::filter(week >= 20, #remove the non-stratified period for TD calculations
         week < 41) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(field_thermo_m = mean(field_thermo_m), mod_thermo_m = mean(mod_thermo_m)) %>% 
  ungroup()
all_gof$Thermo[1:21] <- gof(comb_thermo$mod_thermo_m,comb_thermo$field_thermo_m,do.spearman=TRUE)

comb_thermo_rank <- comb_thermo %>% 
  mutate(rank_obs = rank(field_thermo_m),
         rank_mod = rank(mod_thermo_m)) 
all_gof$Thermo[22] <- summary(lm(comb_thermo_rank$rank_obs ~ comb_thermo_rank$rank_mod))$r.squared

#calibration period
comb_thermo_cal <- left_join(field_thermo_m_cal,mod_thermo_m_cal,by="datetime") %>% 
  mutate(week = week(datetime),
         year = year(datetime),
         month = month(datetime)) %>% 
  dplyr::filter(week >= 20, #remove the non-stratified period for TD calculations
         week < 41) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(field_thermo_m = mean(field_thermo_m), mod_thermo_m = mean(mod_thermo_m)) %>% 
  ungroup()
all_gof_cal$Thermo[1:21] <- gof(comb_thermo_cal$mod_thermo_m,comb_thermo_cal$field_thermo_m,do.spearman=TRUE)

comb_thermo_cal_rank <- comb_thermo_cal %>% 
  mutate(rank_obs = rank(field_thermo_m),
         rank_mod = rank(mod_thermo_m)) 
all_gof_cal$Thermo[22] <- summary(lm(comb_thermo_cal_rank$rank_obs ~ comb_thermo_cal_rank$rank_mod))$r.squared

#validation period
comb_thermo_val <- left_join(field_thermo_m_val,mod_thermo_m_val,by="datetime")%>% 
  mutate(week = week(datetime),
         year = year(datetime),
         month = month(datetime)) %>% 
  dplyr::filter(week >= 20, #remove the non-stratified period for TD calculations
         week < 41) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(field_thermo_m = mean(field_thermo_m), mod_thermo_m = mean(mod_thermo_m)) %>% 
  ungroup()
all_gof_val$Thermo[1:21] <- gof(comb_thermo_val$mod_thermo_m,comb_thermo_val$field_thermo_m,do.spearman=TRUE)

comb_thermo_val_rank <- comb_thermo_val %>% 
  mutate(rank_obs = rank(field_thermo_m),
         rank_mod = rank(mod_thermo_m)) 
all_gof_val$Thermo[22] <- summary(lm(comb_thermo_val_rank$rank_obs ~ comb_thermo_val_rank$rank_mod))$r.squared


#### OXY ####
# obs oxy
obs_oxy$Depth <- as.numeric(obs_oxy$Depth)
obs_oxy_cal <- obs_oxy %>% 
  dplyr::filter(DateTime < "2019-01-01")
obs_oxy_val <- obs_oxy %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# modeled oxy
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
mod_oxy <- get_var(baseline,'OXY_oxy',reference='surface',z_out=depths) %>% 
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(oxy_mod = OXY_oxy)
mod_oxy$Depth <- as.numeric(mod_oxy$Depth)
mod_oxy_cal <- mod_oxy %>% 
  dplyr::filter(DateTime < "2019-01-01")
mod_oxy_val <- mod_oxy %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# full period
comb_oxy <- left_join(obs_oxy,mod_oxy,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(oxy_obs = mean(oxy_obs), oxy_mod = mean(oxy_mod)) %>% 
  ungroup()
all_gof$Oxy[1:21] <- gof(comb_oxy$oxy_mod,comb_oxy$oxy_obs,do.spearman = TRUE)

comb_oxy_rank <- comb_oxy %>% 
  mutate(rank_obs = rank(oxy_obs),
         rank_mod = rank(oxy_mod)) 
all_gof$Oxy[22] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared

#calibration period
comb_oxy_cal <- left_join(obs_oxy_cal,mod_oxy_cal,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(oxy_obs = mean(oxy_obs), oxy_mod = mean(oxy_mod)) %>% 
  ungroup()
all_gof_cal$Oxy[1:21] <- gof(comb_oxy_cal$oxy_mod,comb_oxy_cal$oxy_obs,do.spearman = TRUE)

comb_oxy_cal_rank <- comb_oxy_cal %>% 
  mutate(rank_obs = rank(oxy_obs),
         rank_mod = rank(oxy_mod)) 
all_gof_cal$Oxy[22] <- summary(lm(comb_oxy_cal_rank$rank_obs ~ comb_oxy_cal_rank$rank_mod))$r.squared

#validation period
comb_oxy_val <- left_join(obs_oxy_val,mod_oxy_val,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(oxy_obs = mean(oxy_obs), oxy_mod = mean(oxy_mod)) %>% 
  ungroup()
all_gof_val$Oxy[1:21] <- gof(comb_oxy_val$oxy_mod,comb_oxy_val$oxy_obs,do.spearman = TRUE)

comb_oxy_val_rank <- comb_oxy_val %>% 
  mutate(rank_obs = rank(oxy_obs),
         rank_mod = rank(oxy_mod)) 
all_gof_val$Oxy[22] <- summary(lm(comb_oxy_val_rank$rank_obs ~ comb_oxy_val_rank$rank_mod))$r.squared


#### NIT_amm ####
# Obs NIT_amm
obs_NIT_amm$Depth <- as.numeric(obs_NIT_amm$Depth)
obs_NIT_amm_cal <- obs_NIT_amm %>% 
  dplyr::filter(DateTime < "2019-01-01")
obs_NIT_amm_val <- obs_NIT_amm %>% 
  dplyr::filter(DateTime >= "2019-01-01")

### Mod NIT-amm ###
depths<- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9) #all available sampling depths
mod_NIT_amm <- get_var(baseline,'NIT_amm',z_out=depths,reference='surface') %>% 
  pivot_longer(cols=starts_with("NIT_amm_"), names_to="Depth", names_prefix="NIT_amm_", values_to = "NIT_amm_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
mod_NIT_amm$Depth <- as.numeric(mod_NIT_amm$Depth)
mod_NIT_amm_cal <- mod_NIT_amm %>% 
  dplyr::filter(DateTime < "2019-01-01")
mod_NIT_amm_val <- mod_NIT_amm %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
#full period
comb_NIT_amm <- left_join(obs_NIT_amm,mod_NIT_amm,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(NIT_amm_obs = mean(NIT_amm_obs), NIT_amm_mod = mean(NIT_amm_mod)) %>% 
  ungroup()
all_gof$NIT_amm[1:21] <- gof(comb_NIT_amm$NIT_amm_mod,comb_NIT_amm$NIT_amm_obs,do.spearman = TRUE)

comb_NIT_amm_rank <- comb_NIT_amm %>% 
  mutate(rank_obs = rank(NIT_amm_obs),
         rank_mod = rank(NIT_amm_mod)) 
all_gof$NIT_amm[22] <- summary(lm(comb_NIT_amm_rank$rank_obs ~ comb_NIT_amm_rank$rank_mod))$r.squared

#calibration only
comb_NIT_amm_cal <- left_join(obs_NIT_amm_cal,mod_NIT_amm_cal,by=c("DateTime","Depth"))%>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(NIT_amm_obs = mean(NIT_amm_obs), NIT_amm_mod = mean(NIT_amm_mod)) %>% 
  ungroup()
all_gof_cal$NIT_amm[1:21] <- gof(comb_NIT_amm_cal$NIT_amm_mod,comb_NIT_amm_cal$NIT_amm_obs,do.spearman = TRUE)

comb_NIT_amm_cal_rank <- comb_NIT_amm_cal %>% 
  mutate(rank_obs = rank(NIT_amm_obs),
         rank_mod = rank(NIT_amm_mod)) 
all_gof_cal$NIT_amm[22] <- summary(lm(comb_NIT_amm_cal_rank$rank_obs ~ comb_NIT_amm_cal_rank$rank_mod))$r.squared

#validation only
comb_NIT_amm_val <- left_join(obs_NIT_amm_val,mod_NIT_amm_val,by=c("DateTime","Depth"))%>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(NIT_amm_obs = mean(NIT_amm_obs), NIT_amm_mod = mean(NIT_amm_mod)) %>% 
  ungroup()
all_gof_val$NIT_amm[1:21] <- gof(comb_NIT_amm_val$NIT_amm_mod,comb_NIT_amm_val$NIT_amm_obs,do.spearman = TRUE)

comb_NIT_amm_val_rank <- comb_NIT_amm_val %>% 
  mutate(rank_obs = rank(NIT_amm_obs),
         rank_mod = rank(NIT_amm_mod)) 
all_gof_val$NIT_amm[22] <- summary(lm(comb_NIT_amm_val_rank$rank_obs ~ comb_NIT_amm_val_rank$rank_mod))$r.squared


#### NIT_nit ####
# Obs NIT_nit
obs_NIT_nit$Depth <- as.numeric(obs_NIT_nit$Depth)
obs_NIT_nit_cal <- obs_NIT_nit %>% 
  dplyr::filter(DateTime < "2019-01-01")
obs_NIT_nit_val <- obs_NIT_nit %>% 
  dplyr::filter(DateTime >= "2019-01-01")

### Mod NIT-nit ###
depths<- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9) #all available sampling depths
mod_NIT_nit <- get_var(baseline,'NIT_nit',z_out=depths,reference='surface') %>% 
  pivot_longer(cols=starts_with("NIT_nit_"), names_to="Depth", names_prefix="NIT_nit_", values_to = "NIT_nit_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
mod_NIT_nit$Depth <- as.numeric(mod_NIT_nit$Depth)
mod_NIT_nit_cal <- mod_NIT_nit %>% 
  dplyr::filter(DateTime < "2019-01-01")
mod_NIT_nit_val <- mod_NIT_nit %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL period
comb_NIT_nit <- left_join(obs_NIT_nit,mod_NIT_nit,by=c("DateTime","Depth")) %>% 
  dplyr::filter(NIT_nit_obs > 0.09, #remove values below LOQ
         NIT_nit_mod > 0.09) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(NIT_nit_obs = mean(NIT_nit_obs), NIT_nit_mod = mean(NIT_nit_mod)) %>% 
  ungroup()
all_gof$NIT_nit[1:21] <- gof(comb_NIT_nit$NIT_nit_mod,comb_NIT_nit$NIT_nit_obs,do.spearman = TRUE)

comb_NIT_nit_rank <- comb_NIT_nit %>% 
  mutate(rank_obs = rank(NIT_nit_obs),
         rank_mod = rank(NIT_nit_mod)) 
all_gof$NIT_nit[22] <- summary(lm(comb_NIT_nit_rank$rank_obs ~ comb_NIT_nit_rank$rank_mod))$r.squared

#calibration period only
comb_NIT_nit_cal <- left_join(obs_NIT_nit_cal,mod_NIT_nit_cal,by=c("DateTime","Depth")) %>% 
  dplyr::filter(NIT_nit_obs > 0.09, #remove values below LOQ
         NIT_nit_mod > 0.09) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(NIT_nit_obs = mean(NIT_nit_obs), NIT_nit_mod = mean(NIT_nit_mod)) %>% 
  ungroup()
all_gof_cal$NIT_nit[1:21] <- gof(comb_NIT_nit_cal$NIT_nit_mod,comb_NIT_nit_cal$NIT_nit_obs,do.spearman = TRUE)

comb_NIT_nit_cal_rank <- comb_NIT_nit_cal %>% 
  mutate(rank_obs = rank(NIT_nit_obs),
         rank_mod = rank(NIT_nit_mod)) 
all_gof_cal$NIT_nit[22] <- summary(lm(comb_NIT_nit_cal_rank$rank_obs ~ comb_NIT_nit_cal_rank$rank_mod))$r.squared

#validation period only
comb_NIT_nit_val <- left_join(obs_NIT_nit_val,mod_NIT_nit_val,by=c("DateTime","Depth")) %>% 
  dplyr::filter(NIT_nit_obs > 0.09, #remove values below LOQ
         NIT_nit_mod > 0.09) %>% 
  mutate(month = month(DateTime),
         year = year(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(NIT_nit_obs = mean(NIT_nit_obs), NIT_nit_mod = mean(NIT_nit_mod)) %>% 
  ungroup()
all_gof_val$NIT_nit[1:21] <- gof(comb_NIT_nit_val$NIT_nit_mod,comb_NIT_nit_val$NIT_nit_obs,do.spearman = TRUE)

comb_NIT_nit_val_rank <- comb_NIT_nit_val %>% 
  mutate(rank_obs = rank(NIT_nit_obs),
         rank_mod = rank(NIT_nit_mod)) 
all_gof_val$NIT_nit[22] <- summary(lm(comb_NIT_nit_val_rank$rank_obs ~ comb_NIT_nit_val_rank$rank_mod))$r.squared


#### SRP ####
obs_PHS_frp$Depth <- as.numeric(obs_PHS_frp$Depth)
obs_PHS_frp_cal <- obs_PHS_frp %>% 
  dplyr::filter(DateTime < "2019-01-01")
obs_PHS_frp_val <- obs_PHS_frp %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Mod SRP #
depths<- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9) #all available SRP depths
mod_PHS_frp <- get_var(baseline,'PHS_frp',z_out=depths,reference='surface') %>% 
  pivot_longer(cols=starts_with("PHS_frp_"), names_to="Depth", names_prefix="PHS_frp_", values_to = "PHS_frp_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
mod_PHS_frp$Depth <- as.numeric(mod_PHS_frp$Depth)
mod_PHS_frp_cal <- mod_PHS_frp %>% 
  dplyr::filter(DateTime < "2019-01-01")
mod_PHS_frp_val <- mod_PHS_frp %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# full period
comb_PHS_frp <- left_join(obs_PHS_frp,mod_PHS_frp,by=c("DateTime","Depth")) %>% 
  dplyr::filter(PHS_frp_obs > 0.08, #remove values below the LOQ
         PHS_frp_mod > 0.08) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(PHS_frp_obs = mean(PHS_frp_obs), PHS_frp_mod = mean(PHS_frp_mod)) %>% 
  ungroup() 
all_gof$SRP[1:21] <- gof(comb_PHS_frp$PHS_frp_mod,comb_PHS_frp$PHS_frp_obs,do.spearman = TRUE)

comb_PHS_frp_rank <- comb_PHS_frp %>% 
  mutate(rank_obs = rank(PHS_frp_obs),
         rank_mod = rank(PHS_frp_mod)) 
all_gof$SRP[22] <- summary(lm(comb_PHS_frp_rank$rank_obs ~ comb_PHS_frp_rank$rank_mod))$r.squared

#calibration period only
comb_PHS_frp_cal <- left_join(obs_PHS_frp_cal,mod_PHS_frp_cal,by=c("DateTime","Depth")) %>% 
  dplyr::filter(PHS_frp_obs > 0.08, #remove values below the LOQ
         PHS_frp_mod > 0.08) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(PHS_frp_obs = mean(PHS_frp_obs), PHS_frp_mod = mean(PHS_frp_mod)) %>% 
  ungroup()
all_gof_cal$SRP[1:21] <- gof(comb_PHS_frp_cal$PHS_frp_mod,comb_PHS_frp_cal$PHS_frp_obs,do.spearman = TRUE)

comb_PHS_frp_cal_rank <- comb_PHS_frp_cal %>% 
  mutate(rank_obs = rank(PHS_frp_obs),
         rank_mod = rank(PHS_frp_mod)) 
all_gof_cal$SRP[22] <- summary(lm(comb_PHS_frp_cal_rank$rank_obs ~ comb_PHS_frp_cal_rank$rank_mod))$r.squared

#validation period only
comb_PHS_frp_val <- left_join(obs_PHS_frp_val,mod_PHS_frp_val,by=c("DateTime","Depth")) %>% 
  dplyr::filter(PHS_frp_obs > 0.08, #remove values below the LOQ
         PHS_frp_mod > 0.08) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(PHS_frp_obs = mean(PHS_frp_obs), PHS_frp_mod = mean(PHS_frp_mod)) %>% 
  ungroup()
all_gof_val$SRP[1:21] <- gof(comb_PHS_frp_val$PHS_frp_mod,comb_PHS_frp_val$PHS_frp_obs,do.spearman = TRUE)

comb_PHS_frp_val_rank <- comb_PHS_frp_val %>% 
  mutate(rank_obs = rank(PHS_frp_obs),
         rank_mod = rank(PHS_frp_mod)) 
all_gof_val$SRP[22] <- summary(lm(comb_PHS_frp_val_rank$rank_obs ~ comb_PHS_frp_val_rank$rank_mod))$r.squared


#### DOC + rDOC ####
# Obs #
obs_allDOC$Depth <- as.numeric(obs_allDOC$Depth)
obs_allDOC_cal <- obs_allDOC %>% 
  dplyr::filter(DateTime < "2019-01-01")
obs_allDOC_val <- obs_allDOC %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Mod #
# Summed DOC and rDOC following MakeFigure_ModeledvsObserved
depths<- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9) #all available data
mod_DOC <- get_var(baseline,'OGM_doc',z_out=depths,reference='surface') %>% 
  pivot_longer(cols=starts_with("OGM_doc_"), names_to="Depth", names_prefix="OGM_doc_", values_to = "DOC_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
mod_DOC$Depth <- as.numeric(mod_DOC$Depth)

mod_rDOC <- get_var(baseline,'OGM_docr',z_out=depths,reference='surface') %>% 
  pivot_longer(cols=starts_with("OGM_docr_"), names_to="Depth", names_prefix="OGM_docr_", values_to = "rDOC_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
mod_rDOC$Depth <- as.numeric(mod_rDOC$Depth)

mod_allDOC <- left_join(mod_DOC,mod_rDOC,by=c("DateTime","Depth")) %>% 
  mutate(allDOC_mod = DOC_mod + rDOC_mod)

mod_allDOC_cal <- mod_allDOC %>% 
  dplyr::filter(DateTime < "2019-01-01")
mod_allDOC_val <- mod_allDOC %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# full period
comb_allDOC <- left_join(obs_allDOC,mod_allDOC,by=c("DateTime","Depth")) %>% 
  dplyr::filter(allDOC_obs > 94, #remove values below LOQ
         allDOC_mod > 94) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(allDOC_obs = mean(allDOC_obs), allDOC_mod = mean(allDOC_mod)) %>% 
  ungroup()
all_gof$DOC[1:21] <- gof(comb_allDOC$allDOC_mod,comb_allDOC$allDOC_obs,do.spearman = TRUE)

comb_allDOC_rank <- comb_allDOC %>% 
  mutate(rank_obs = rank(allDOC_obs),
         rank_mod = rank(allDOC_mod)) 
all_gof$DOC[22] <- summary(lm(comb_allDOC_rank$rank_obs ~ comb_allDOC_rank$rank_mod))$r.squared

#calibration only
comb_allDOC_cal <- left_join(obs_allDOC_cal,mod_allDOC_cal,by=c("DateTime","Depth")) %>% 
  dplyr::filter(allDOC_obs > 94, #remove values below LOQ
         allDOC_mod > 94) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(allDOC_obs = mean(allDOC_obs), allDOC_mod = mean(allDOC_mod)) %>% 
  ungroup()
all_gof_cal$DOC[1:21] <- gof(comb_allDOC_cal$allDOC_mod,comb_allDOC_cal$allDOC_obs,do.spearman = TRUE)

comb_allDOC_cal_rank <- comb_allDOC_cal %>% 
  mutate(rank_obs = rank(allDOC_obs),
         rank_mod = rank(allDOC_mod)) 
all_gof_cal$DOC[22] <- summary(lm(comb_allDOC_cal_rank$rank_obs ~ comb_allDOC_cal_rank$rank_mod))$r.squared

#validation only
comb_allDOC_val <- left_join(obs_allDOC_val,mod_allDOC_val,by=c("DateTime","Depth")) %>% 
  dplyr::filter(allDOC_obs > 94, #remove values below LOQ
         allDOC_mod > 94) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(allDOC_obs = mean(allDOC_obs), allDOC_mod = mean(allDOC_mod)) %>% 
  ungroup()
all_gof_val$DOC[1:21] <- gof(comb_allDOC_val$allDOC_mod,comb_allDOC_val$allDOC_obs,do.spearman = TRUE)

comb_allDOC_val_rank <- comb_allDOC_val %>% 
  mutate(rank_obs = rank(allDOC_obs),
         rank_mod = rank(allDOC_mod)) 
all_gof_val$DOC[22] <- summary(lm(comb_allDOC_val_rank$rank_obs ~ comb_allDOC_val_rank$rank_mod))$r.squared


#### TOTALS ####
# Observed data
obs_Totals$Depth <- as.numeric(obs_Totals$Depth)
obs_Totals_cal <- obs_Totals %>% 
  dplyr::filter(DateTime < "2019-01-01")
obs_Totals_val <- obs_Totals %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Mod #
# Calculations following MakeFigure_ModeledvsObserved
# Calculate modeled data
TN <- get_var(baseline, "TOT_tn",z_out=depths,reference = 'surface') %>% 
  pivot_longer(cols=starts_with("TOT_tn_"), names_to="Depth", names_prefix="TOT_tn_", values_to = "TOT_tn_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
TN$Depth <- as.numeric(TN$Depth)

TP <- get_var(baseline, "TOT_tp",z_out=depths,reference = 'surface') %>% 
  pivot_longer(cols=starts_with("TOT_tp_"), names_to="Depth", names_prefix="TOT_tp_", values_to = "TOT_tp_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
TP$Depth <- as.numeric(TP$Depth)

TOC <- get_var(baseline, "TOT_toc",z_out=depths,reference = 'surface') %>% 
  pivot_longer(cols=starts_with("TOT_toc_"), names_to="Depth", names_prefix="TOT_toc_", values_to = "TOT_toc_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
TOC$Depth <- as.numeric(TOC$Depth)

cyano <- get_var(baseline,var_name = 'PHY_cyano',z_out=depths,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoN = CyanoConc*0.12,
         cyanoP = CyanoConc*0.0005,
         cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoN, cyanoP, cyanoC)

green <- get_var(baseline,var_name = 'PHY_green',z_out=depths,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenN = GreenConc*0.12,
         greenP = GreenConc*0.0005,
         greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenN, greenP, greenC)

diatom <- get_var(baseline,var_name = 'PHY_diatom',z_out=depths,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomN = DiatomConc*0.12,
         diatomP = DiatomConc*0.0005,
         diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth, diatomN, diatomP, diatomC)

mod_totals<-as.data.frame(cbind(diatom[1:5],cyano[,3:5],green[,3:5],TN[,3],TP[,3],TOC[,3])) 
colnames(mod_totals) = c("DateTime", "Depth", "diatomN", "diatomP", "diatomC",
                            "cyanoN", "cyanoP", "cyanoC", "greenN", "greenP", "greenC",
                            "TN", "TP", "TOC") 

mod_totals2 <- mod_totals %>% 
  mutate(TN_mod = TN + diatomN + cyanoN + greenN,
         TP_mod = TP + diatomP + cyanoP + greenP,
         TOC_mod = TOC + diatomC + cyanoC + greenC) %>% 
  select(DateTime, Depth, TN_mod:TOC_mod) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))
mod_totals2$Depth <- as.numeric(mod_totals2$Depth)
mod_totals2_cal <- mod_totals2 %>% 
  dplyr::filter(DateTime < "2019-01-01")
mod_totals2_val <- mod_totals2 %>% 
  dplyr::filter(DateTime >= "2019-01-01")


####TN####
# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# full period
comb_totals <- left_join(obs_Totals,mod_totals2,by=c("DateTime","Depth")) %>% 
  select(DateTime, Depth, TN_obs, TN_mod)

comb_TN <- comb_totals %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(TN_obs = mean(TN_obs), 
            TN_mod = mean(TN_mod)) %>% 
  ungroup()
all_gof$TN[1:21] <- gof(comb_TN$TN_mod,comb_TN$TN_obs,na.rm=TRUE,do.spearman = TRUE)

comb_Totals_rank <- comb_TN %>% 
  mutate(rank_obsTN = rank(TN_obs),
         rank_modTN = rank(TN_mod)) 
all_gof$TN[22] <- summary(lm(comb_Totals_rank$rank_obsTN ~ comb_Totals_rank$rank_modTN))$r.squared

#calibration period only
comb_Totals_cal <- left_join(obs_Totals_cal,mod_totals2_cal,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(TN_obs = mean(TN_obs), 
            TN_mod = mean(TN_mod)) %>% 
  ungroup()
all_gof_cal$TN[1:21] <- gof(comb_Totals_cal$TN_mod,comb_Totals_cal$TN_obs,na.rm=TRUE,do.spearman = TRUE)

comb_Totals_cal_rank <- comb_Totals_cal %>% 
  mutate(rank_obsTN = rank(TN_obs),
         rank_modTN = rank(TN_mod)) 
all_gof_cal$TN[22] <- summary(lm(comb_Totals_cal_rank$rank_obsTN ~ comb_Totals_cal_rank$rank_modTN))$r.squared

#validation period only
comb_Totals_val <- left_join(obs_Totals_val,mod_totals2_val,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(TN_obs = mean(TN_obs), 
            TN_mod = mean(TN_mod)) %>% 
  ungroup()
all_gof_val$TN[1:21] <- gof(comb_Totals_val$TN_mod,comb_Totals_val$TN_obs,na.rm=TRUE,do.spearman = TRUE)

comb_Totals_val_rank <- comb_Totals_val %>% 
  mutate(rank_obsTN = rank(TN_obs),
         rank_modTN = rank(TN_mod)) 
all_gof_val$TN[22] <- summary(lm(comb_Totals_val_rank$rank_obsTN ~ comb_Totals_val_rank$rank_modTN))$r.squared


####TP####
comb_totals <- left_join(obs_Totals,mod_totals2,by=c("DateTime","Depth")) %>% 
  select(DateTime, Depth, TP_obs, TP_mod)

comb_TP <- comb_totals %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(TP_obs = mean(TP_obs),
            TP_mod = mean(TP_mod)) %>% 
  ungroup()
all_gof$TP[1:21] <- gof(comb_TP$TP_mod,comb_TP$TP_obs,na.rm=TRUE,do.spearman = TRUE)

comb_Totals_rank <- comb_TP %>% 
  mutate(rank_obsTP = rank(TP_obs),
         rank_modTP = rank(TP_mod)) 
all_gof$TP[22] <- summary(lm(comb_Totals_rank$rank_obsTP ~ comb_Totals_rank$rank_modTP))$r.squared

#calibration period only
comb_Totals_cal <- left_join(obs_Totals_cal,mod_totals2_cal,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(TP_obs = mean(TP_obs),
            TP_mod = mean(TP_mod)) %>% 
  ungroup()
all_gof_cal$TP[1:21] <- gof(comb_Totals_cal$TP_mod,comb_Totals_cal$TP_obs,na.rm=TRUE,do.spearman = TRUE)

comb_Totals_cal_rank <- comb_Totals_cal %>% 
  mutate(rank_obsTP = rank(TP_obs),
         rank_modTP = rank(TP_mod)) 
all_gof_cal$TP[22] <- summary(lm(comb_Totals_cal_rank$rank_obsTP ~ comb_Totals_cal_rank$rank_modTP))$r.squared

#validation period only
comb_Totals_val <- left_join(obs_Totals_val,mod_totals2_val,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(TP_obs = mean(TP_obs),
            TP_mod = mean(TP_mod)) %>% 
  ungroup()
all_gof_val$TP[1:21] <- gof(comb_Totals_val$TP_mod,comb_Totals_val$TP_obs,na.rm=TRUE,do.spearman = TRUE)

comb_Totals_val_rank <- comb_Totals_val %>% 
  mutate(rank_obsTP = rank(TP_obs),
         rank_modTP = rank(TP_mod)) 
all_gof_val$TP[22] <- summary(lm(comb_Totals_val_rank$rank_obsTP ~ comb_Totals_val_rank$rank_modTP))$r.squared


####Cleaning up table ####
## Add NMAE calculation for all parameters
# all_gof
all_gof[nrow(all_gof)+1,] <- NA
all_gof[23,1] <- "NMAE_all"
all_gof$Parameter[21] <- "r.Spearman_all"
all_gof$Temp[23] <- round(all_gof$Temp[2]/mean(comb_temp$temp_obs),digits = 2)
all_gof$Thermo[23] <- round(all_gof$Thermo[2]/mean(comb_thermo$field_thermo_m,na.rm=TRUE),digits = 2)
all_gof$Oxy[23] <- round(all_gof$Oxy[2]/mean(comb_oxy$oxy_obs),digits = 2)
all_gof$NIT_amm[23] <- round(all_gof$NIT_amm[2]/mean(comb_NIT_amm$NIT_amm_obs),digits = 2)
all_gof$NIT_nit[23] <- round(all_gof$NIT_nit[2]/mean(comb_NIT_nit$NIT_nit_obs,na.rm=TRUE),digits = 2)
all_gof$SRP[23] <- round(all_gof$SRP[2]/mean(comb_PHS_frp$PHS_frp_obs,na.rm=TRUE),digits = 2)
all_gof$DOC[23] <- round(all_gof$DOC[2]/mean(comb_allDOC$allDOC_obs),digits = 2)
all_gof$TN[23] <- round(all_gof$TN[2]/mean(comb_TN$TN_obs,na.rm=TRUE),digits = 2)
all_gof$TP[23] <- round(all_gof$TP[2]/mean(comb_TP$TP_obs,na.rm=TRUE),digits = 2)

# all_gof_cal
all_gof_cal[nrow(all_gof_cal)+1,] <- NA
all_gof_cal[23,1] <- "NMAE_cal"
all_gof_cal$Parameter[21] <- "r.Spearman_cal"
all_gof_cal$Temp[23] <- round(all_gof_cal$Temp[2]/mean(comb_temp_cal$temp_obs),digits = 2)
all_gof_cal$Thermo[23] <- round(all_gof_cal$Thermo[2]/mean(comb_thermo_cal$field_thermo_m,na.rm=TRUE),digits = 2)
all_gof_cal$Oxy[23] <- round(all_gof_cal$Oxy[2]/mean(comb_oxy_cal$oxy_obs),digits = 2)
all_gof_cal$NIT_amm[23] <- round(all_gof_cal$NIT_amm[2]/mean(comb_NIT_amm_cal$NIT_amm_obs),digits = 2)
all_gof_cal$NIT_nit[23] <- round(all_gof_cal$NIT_nit[2]/mean(comb_NIT_nit_cal$NIT_nit_obs,na.rm=TRUE),digits = 2)
all_gof_cal$SRP[23] <- round(all_gof_cal$SRP[2]/mean(comb_PHS_frp_cal$PHS_frp_obs,na.rm=TRUE),digits = 2)
all_gof_cal$DOC[23] <- round(all_gof_cal$DOC[2]/mean(comb_allDOC_cal$allDOC_obs),digits = 2)
all_gof_cal$TN[23] <- round(all_gof_cal$TN[2]/mean(comb_TN$TN_obs,na.rm=TRUE),digits = 2)
all_gof_cal$TP[23] <- round(all_gof_cal$TP[2]/mean(comb_TP$TP_obs,na.rm=TRUE),digits = 2)

# all_gof_val
all_gof_val[nrow(all_gof_val)+1,] <- NA
all_gof_val[23,1] <- "NMAE_val"
all_gof_val$Parameter[21] <- "r.Spearman_val"
all_gof_val$Temp[23] <- round(all_gof_val$Temp[2]/mean(comb_temp_val$temp_obs),digits = 2)
all_gof_val$Thermo[23] <- round(all_gof_val$Thermo[2]/mean(comb_thermo_val$field_thermo_m,na.rm=TRUE),digits = 2)
all_gof_val$Oxy[23] <- round(all_gof_val$Oxy[2]/mean(comb_oxy_val$oxy_obs),digits = 2)
all_gof_val$NIT_amm[23] <- round(all_gof_val$NIT_amm[2]/mean(comb_NIT_amm_val$NIT_amm_obs),digits = 2)
all_gof_val$NIT_nit[23] <- round(all_gof_val$NIT_nit[2]/mean(comb_NIT_nit_val$NIT_nit_obs,na.rm=TRUE),digits = 2)
all_gof_val$SRP[23] <- round(all_gof_val$SRP[2]/mean(comb_PHS_frp_val$PHS_frp_obs,na.rm=TRUE),digits = 2)
all_gof_val$DOC[23] <- round(all_gof_val$DOC[2]/mean(comb_allDOC_val$allDOC_obs),digits = 2)
all_gof_val$TN[23] <- round(all_gof_val$TN[2]/mean(comb_TN$TN_obs,na.rm=TRUE),digits = 2)
all_gof_val$TP[23] <- round(all_gof_val$TP[2]/mean(comb_TP$TP_obs,na.rm=TRUE),digits = 2)

# Select GOF variables for the full year
full_n_all <- c("n_all",length(obs_temp$temp_obs),
                length(na.omit(field_thermo_m$field_thermo_m)),
                length(obs_oxy$DateTime),
                length(na.omit(obs_NIT_amm$NIT_amm_obs)),
                length(na.omit(obs_NIT_nit$NIT_nit_obs)),
                length(na.omit(obs_PHS_frp$PHS_frp_obs)),
                length(na.omit(obs_allDOC$allDOC_obs)),
                length(na.omit(obs_Totals$TN_obs)),
                length(na.omit(obs_Totals$TP_obs)))

full_n_cal <- c("n_cal",length(obs_temp$DateTime[which(obs_temp$DateTime<"2019-01-01")]),
                length(na.omit(field_thermo_m$field_thermo_m[which(field_thermo_m$datetime<"2019-01-01")])),
                length(obs_oxy$DateTime[which(obs_oxy$DateTime<"2019-01-01")]),
                length(na.omit(obs_NIT_amm$NIT_amm_obs[which(obs_NIT_amm$DateTime<"2019-01-01")])),
                length(na.omit(obs_NIT_nit$NIT_nit_obs[which(obs_NIT_nit$DateTime<"2019-01-01")])),
                length(na.omit(obs_PHS_frp$PHS_frp_obs[which(obs_PHS_frp$DateTime<"2019-01-01")])),
                length(na.omit(obs_allDOC$allDOC_obs[which(obs_allDOC$DateTime<"2019-01-01")])),
                length(na.omit(obs_Totals$TN_obs[which(obs_Totals$DateTime<"2019-01-01")])),
                length(na.omit(obs_Totals$TP_obs[which(obs_Totals$DateTime<"2019-01-01")])))

full_n_val <- c("n_val",length(obs_temp$DateTime[which(obs_temp$DateTime>="2019-01-01")]),
                length(na.omit(field_thermo_m$field_thermo_m[which(field_thermo_m$datetime>="2019-01-01")])),
                length(obs_oxy$DateTime[which(obs_oxy$DateTime>="2019-01-01")]),
                length(na.omit(obs_NIT_amm$NIT_amm_obs[which(obs_NIT_amm$DateTime>="2019-01-01")])),
                length(na.omit(obs_NIT_nit$NIT_nit_obs[which(obs_NIT_nit$DateTime>="2019-01-01")])),
                length(na.omit(obs_PHS_frp$PHS_frp_obs[which(obs_PHS_frp$DateTime>="2019-01-01")])),
                length(na.omit(obs_allDOC$allDOC_obs[which(obs_allDOC$DateTime>="2019-01-01")])),
                length(na.omit(obs_Totals$TN_obs[which(obs_Totals$DateTime>="2019-01-01")])),
                length(na.omit(obs_Totals$TP_obs[which(obs_Totals$DateTime>="2019-01-01")])))

full_gof_all_table <- all_gof %>% 
  filter(Parameter == "r.Spearman_all" | Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NMAE_all")

full_gof_cal_table <- all_gof_cal %>% 
  filter(Parameter == "r.Spearman_cal" | Parameter == "R2_cal" | Parameter == "RMSE_cal" | Parameter == "PBIAS%_cal" | Parameter == "NMAE_cal")

full_gof_val_table <- all_gof_val %>% 
  filter(Parameter == "r.Spearman_val" | Parameter == "R2_val" | Parameter == "RMSE_val" | Parameter == "PBIAS%_val" | Parameter == "NMAE_val")

full_gof_table <- rbind(full_n_all,full_gof_all_table,full_n_cal,full_gof_cal_table,full_n_val,full_gof_val_table)

write_csv(full_gof_table,'figures/table_gof_watercol_fullyear_updated.csv')
