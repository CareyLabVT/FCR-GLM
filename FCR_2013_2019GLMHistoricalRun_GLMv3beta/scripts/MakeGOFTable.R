# Script to make GOF tables for main MS and supplementary
# 12 Jan 2021
# A Hounshell

# Load in libraries
# pacman::p_load(devtools)
pacman::p_load(zoo,tidyverse,lubridate,hydroGOF,GLMr,glmtools,ggplot2)

# devtools::install_github("CareyLabVT/GLMr", force = TRUE) 
# devtools::install_github("CareyLabVT/glmtools", force = TRUE)

# Set working directory
setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

# Using baseline model data (aka: no oxygenation scenarios)
# Download nc_file from dropbox link: https://www.dropbox.com/s/bdh99f6zyll1e5y/output_2013_2019.nc?dl=0
# AGH saved locally to /output folder

# Load in nc_file
baseline <- file.path(sim_folder, 'FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/output_2013_2019.nc')

# Then use various field observations from the observed/field files for the various parameters

############################# START WITH MAIN MS TABLE ####################################
# Notes from CCC: For the main text, I was looking for a 9m-only comparison of water temp, DO, DOC, etc 
# (all the variables in Fig 2) for the calibration period (2013-2018), validation period (2019 only) and then 
# complete simulation period (2013-2019). When I talked to Nicole about it for her recommendations, she suggested 
# focusing on summer only conditions, which is also what we did in Kait’s Ecol Modeling paper (May 15-Oct 15 is 
# what I think I used as the definition of summer for export calculations in the script, but please check this!). 
# If it would be possible to run this table both ways (full year vs summer only) that would be great.

# Variables to include: temp, oxy, NIT_amm, NIT_nit, SRP, DOC, TN, TP for 9 m ONLY
# Include: Calibration period (2013-2018); Validation period (2019); and All (2013-2019)

# Summer calculations defined as July 15 to Oct 1 - Following summer stratified period as defined in 
# MakeFigure_Boxplots.R

# Create empty dataframe for GOF results
# Using all GOF parameters for now - will cull eventually
# Summer, 9m, full period (2013-2019)
summer_9m_gof <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
summer_9m_gof$Parameter <- c("ME","MAE","MSE","RMSE","NRMSE%","PBIAS%","RSR","rSD","NSE","mNSE","rNSE","d","md","rd","cp","r",
                             "R2","bR2","KGE","VE")

# Full year, 9m, full period (2013-2019)
all_9m_gof <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_9m_gof$Parameter <- c("ME","MAE","MSE","RMSE","NRMSE%","PBIAS%","RSR","rSD","NSE","mNSE","rNSE","d","md","rd","cp","r",
                             "R2","bR2","KGE","VE")

# Summer, 9m, calibration period (2013-2018)
summer_9m_gof_cal <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
summer_9m_gof_cal$Parameter <- c("ME","MAE","MSE","RMSE","NRMSE%","PBIAS%","RSR","rSD","NSE","mNSE","rNSE","d","md","rd","cp","r",
                       "R2","bR2","KGE","VE")

# Full year, 9m, calibration period (2013-2018)
all_9m_gof_cal <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_9m_gof_cal$Parameter <- c("ME","MAE","MSE","RMSE","NRMSE%","PBIAS%","RSR","rSD","NSE","mNSE","rNSE","d","md","rd","cp","r",
                    "R2","bR2","KGE","VE")

# Summer, 9m, validation period (2019)
summer_9m_gof_val <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
summer_9m_gof_val$Parameter <- c("ME","MAE","MSE","RMSE","NRMSE%","PBIAS%","RSR","rSD","NSE","mNSE","rNSE","d","md","rd","cp","r",
                       "R2","bR2","KGE","VE")

# Full year, 9m, validation period (2019)
all_9m_gof_val <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_9m_gof_val$Parameter <- c("ME","MAE","MSE","RMSE","NRMSE%","PBIAS%","RSR","rSD","NSE","mNSE","rNSE","d","md","rd","cp","r",
                    "R2","bR2","KGE","VE")

### 9m Summer TEMP ###
# 9m Model temp
mod_temp_9m <- get_temp(baseline,z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(temp_mod = temp_9)
mod_temp_9m_cal <- mod_temp_9m %>% 
  filter(DateTime < "2019-01-01")
mod_temp_9m_val <- mod_temp_9m %>% 
  filter(DateTime >= "2019-01-01")

mod_temp_9m_summer <- mod_temp_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_temp_9m_summer_cal <- mod_temp_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_temp_9m_summer_val <- mod_temp_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# 9 m Obs temp
obs_temp <- read_csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/CleanedObsTemp.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(temp_obs = temp)
obs_temp_9m <- obs_temp %>% 
  filter(Depth==9)
obs_temp_9m_cal <- obs_temp_9m %>% 
  filter(DateTime < "2019-01-01")
obs_temp_9m_val <- obs_temp_9m %>% 
  filter(DateTime >= "2019-01-01")

obs_temp_9m_summer <- obs_temp_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_temp_9m_summer_cal <- obs_temp_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_temp_9m_summer_val <- obs_temp_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# For SUMMER only
comb_9m_temp_summer <- left_join(obs_temp_9m_summer,mod_temp_9m_summer,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof$Temp <- gof(comb_9m_temp_summer$temp_mod,comb_9m_temp_summer$temp_obs)

comb_9m_temp_summer_cal <- left_join(obs_temp_9m_summer_cal,mod_temp_9m_summer_cal,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_cal$Temp <- gof(comb_9m_temp_summer_cal$temp_mod,comb_9m_temp_summer_cal$temp_obs)

comb_9m_temp_summer_val <- left_join(obs_temp_9m_summer_val,mod_temp_9m_summer_val,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_val$Temp <- gof(comb_9m_temp_summer_val$temp_mod,comb_9m_temp_summer_val$temp_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later)
# For FULL year
comb_9m_temp <- left_join(obs_temp_9m,mod_temp_9m,by="DateTime")
all_9m_gof$Temp <- gof(comb_9m_temp$temp_mod,comb_9m_temp$temp_obs)

comb_9m_temp_cal <- left_join(obs_temp_9m_cal,mod_temp_9m_cal,by="DateTime")
all_9m_gof_cal$Temp <- gof(comb_9m_temp_cal$temp_mod,comb_9m_temp_cal$temp_obs)

comb_9m_temp_val <- left_join(obs_temp_9m_val,mod_temp_9m_val,by="DateTime")
all_9m_gof_val$Temp <- gof(comb_9m_temp_val$temp_mod,comb_9m_temp_val$temp_obs)

### 9m summer OXY ###
# 9m modeled oxy
mod_oxy_9m <- get_var(baseline,'OXY_oxy',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(oxy_mod = OXY_oxy_9)
mod_oxy_9m_cal <- mod_oxy_9m %>% 
  filter(DateTime < "2019-01-01")
mod_oxy_9m_val <- mod_oxy_9m %>% 
  filter(DateTime >= "2019-01-01")

mod_oxy_9m_summer <- mod_oxy_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_oxy_9m_summer_cal <- mod_oxy_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_oxy_9m_summer_val <- mod_oxy_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# 9m obs oxy
obs_oxy <- read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/CleanedObsOxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(oxy_obs = OXY_oxy)
obs_oxy_9m <- obs_oxy %>% 
  filter(Depth==9)
obs_oxy_9m_cal <- obs_oxy_9m %>% 
  filter(DateTime < "2019-01-01")
obs_oxy_9m_val <- obs_oxy_9m %>% 
  filter(DateTime >= "2019-01-01")

obs_oxy_9m_summer <- obs_oxy_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_oxy_9m_summer_cal <- obs_oxy_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_oxy_9m_summer_val <- obs_oxy_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
comb_9m_oxy_summer <- left_join(obs_oxy_9m_summer,mod_oxy_9m_summer,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof$Oxy <- gof(comb_9m_oxy_summer$oxy_mod,comb_9m_oxy_summer$oxy_obs)

comb_9m_oxy_summer_cal <- left_join(obs_oxy_9m_summer_cal,mod_oxy_9m_summer_cal,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_cal$Oxy <- gof(comb_9m_oxy_summer_cal$oxy_mod,comb_9m_oxy_summer_cal$oxy_obs)

comb_9m_oxy_summer_val <- left_join(obs_oxy_9m_summer_val,mod_oxy_9m_summer_val,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_val$Oxy <- gof(comb_9m_oxy_summer_val$oxy_mod,comb_9m_oxy_summer_val$oxy_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_oxy <- left_join(obs_oxy_9m,mod_oxy_9m,by="DateTime")
all_9m_gof$Oxy <- gof(comb_9m_oxy$oxy_mod,comb_9m_oxy$oxy_obs)

comb_9m_oxy_cal <- left_join(obs_oxy_9m_cal,mod_oxy_9m_cal,by="DateTime")
all_9m_gof_cal$Oxy <- gof(comb_9m_oxy_cal$oxy_mod,comb_9m_oxy_cal$oxy_obs)

comb_9m_oxy_val <- left_join(obs_oxy_9m_val,mod_oxy_9m_val,by="DateTime")
all_9m_gof_val$Oxy <- gof(comb_9m_oxy_val$oxy_mod,comb_9m_oxy_val$oxy_obs)

### 9 m summer NIT-amm ###
mod_NIT_amm_9m <- get_var(baseline,'NIT_amm',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(NIT_amm_mod = NIT_amm_9)
mod_NIT_amm_9m_cal <- mod_NIT_amm_9m %>% 
  filter(DateTime < "2019-01-01")
mod_NIT_amm_9m_val <- mod_NIT_amm_9m %>% 
  filter(DateTime >= "2019-01-01")

mod_NIT_amm_9m_summer <- mod_NIT_amm_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_NIT_amm_9m_summer_cal <- mod_NIT_amm_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_NIT_amm_9m_summer_val <- mod_NIT_amm_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

obs_NIT_amm <- read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  select(DateTime,Depth,NIT_amm) %>% 
  rename(NIT_amm_obs = NIT_amm)
obs_NIT_amm_9m <- obs_NIT_amm %>% 
  filter(Depth == 9) %>% 
  na.omit()
obs_NIT_amm_9m_cal <- obs_NIT_amm_9m %>% 
  filter(DateTime < "2019-01-01")
obs_NIT_amm_9m_val <- obs_NIT_amm_9m %>% 
  filter(DateTime >= "2019-01-01")

obs_NIT_amm_9m_summer <- obs_NIT_amm_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_NIT_amm_9m_summer_cal <- obs_NIT_amm_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_NIT_amm_9m_summer_val <- obs_NIT_amm_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
comb_9m_NIT_amm_summer <- left_join(obs_NIT_amm_9m_summer,mod_NIT_amm_9m_summer,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof$NIT_amm <- gof(comb_9m_NIT_amm_summer$NIT_amm_mod,comb_9m_NIT_amm_summer$NIT_amm_obs)

comb_9m_NIT_amm_summer_cal <- left_join(obs_NIT_amm_9m_summer_cal,mod_NIT_amm_9m_summer_cal,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_cal$NIT_amm <- gof(comb_9m_NIT_amm_summer_cal$NIT_amm_mod,comb_9m_NIT_amm_summer_cal$NIT_amm_obs)

comb_9m_NIT_amm_summer_val <- left_join(obs_NIT_amm_9m_summer_val,mod_NIT_amm_9m_summer_val,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_val$NIT_amm <- gof(comb_9m_NIT_amm_summer_val$NIT_amm_mod,comb_9m_NIT_amm_summer_val$NIT_amm_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_NIT_amm <- left_join(obs_NIT_amm_9m,mod_NIT_amm_9m,by="DateTime")
all_9m_gof$NIT_amm <- gof(comb_9m_NIT_amm$NIT_amm_mod,comb_9m_NIT_amm$NIT_amm_obs)

comb_9m_NIT_amm_cal <- left_join(obs_NIT_amm_9m_cal,mod_NIT_amm_9m_cal,by="DateTime")
all_9m_gof_cal$NIT_amm <- gof(comb_9m_NIT_amm_cal$NIT_amm_mod,comb_9m_NIT_amm_cal$NIT_amm_obs)

comb_9m_NIT_amm_val <- left_join(obs_NIT_amm_9m_val,mod_NIT_amm_9m_val,by="DateTime")
all_9m_gof_val$NIT_amm <- gof(comb_9m_NIT_amm_val$NIT_amm_mod,comb_9m_NIT_amm_val$NIT_amm_obs)

### 9m summer NIT_nit ###
mod_NIT_nit_9m <- get_var(baseline,'NIT_nit',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(NIT_nit_mod = NIT_nit_9)
mod_NIT_nit_9m_cal <- mod_NIT_nit_9m %>% 
  filter(DateTime < "2019-01-01")
mod_NIT_nit_9m_val <- mod_NIT_nit_9m %>% 
  filter(DateTime >= "2019-01-01")

mod_NIT_nit_9m_summer <- mod_NIT_nit_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_NIT_nit_9m_summer_cal <- mod_NIT_nit_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_NIT_nit_9m_summer_val <- mod_NIT_nit_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

obs_NIT_nit <- read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  select(DateTime,Depth,NIT_nit) %>% 
  rename(NIT_nit_obs = NIT_nit)
obs_NIT_nit_9m <- obs_NIT_nit %>% 
  filter(Depth == 9) %>% 
  na.omit()
obs_NIT_nit_9m_cal <- obs_NIT_nit_9m %>% 
  filter(DateTime < "2019-01-01")
obs_NIT_nit_9m_val <- obs_NIT_nit_9m %>% 
  filter(DateTime >= "2019-01-01")

obs_NIT_nit_9m_summer <- obs_NIT_nit_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_NIT_nit_9m_summer_cal <- obs_NIT_nit_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_NIT_nit_9m_summer_val <- obs_NIT_nit_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
comb_9m_NIT_nit_summer <- left_join(obs_NIT_nit_9m_summer,mod_NIT_nit_9m_summer,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof$NIT_nit <- gof(comb_9m_NIT_nit_summer$NIT_nit_mod,comb_9m_NIT_nit_summer$NIT_nit_obs)

comb_9m_NIT_nit_summer_cal <- left_join(obs_NIT_nit_9m_summer_cal,mod_NIT_nit_9m_summer_cal,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_cal$NIT_nit <- gof(comb_9m_NIT_nit_summer_cal$NIT_nit_mod,comb_9m_NIT_nit_summer_cal$NIT_nit_obs)

comb_9m_NIT_nit_summer_val <- left_join(obs_NIT_nit_9m_summer_val,mod_NIT_nit_9m_summer_val,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_val$NIT_nit <- gof(comb_9m_NIT_nit_summer_val$NIT_nit_mod,comb_9m_NIT_nit_summer_val$NIT_nit_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_NIT_nit <- left_join(obs_NIT_nit_9m,mod_NIT_nit_9m,by="DateTime")
all_9m_gof$NIT_nit <- gof(comb_9m_NIT_nit$NIT_nit_mod,comb_9m_NIT_nit$NIT_nit_obs)

comb_9m_NIT_nit_cal <- left_join(obs_NIT_nit_9m_cal,mod_NIT_nit_9m_cal,by="DateTime")
all_9m_gof_cal$NIT_nit <- gof(comb_9m_NIT_nit_cal$NIT_nit_mod,comb_9m_NIT_nit_cal$NIT_nit_obs)

comb_9m_NIT_nit_val <- left_join(obs_NIT_nit_9m_val,mod_NIT_nit_9m_val,by="DateTime")
all_9m_gof_val$NIT_nit <- gof(comb_9m_NIT_nit_val$NIT_nit_mod,comb_9m_NIT_nit_val$NIT_nit_obs)

### 9 m SRP ###
mod_PHS_frp_9m <- get_var(baseline,'PHS_frp',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(PHS_frp_mod = PHS_frp_9)
mod_PHS_frp_9m_cal <- mod_PHS_frp_9m %>% 
  filter(DateTime < "2019-01-01")
mod_PHS_frp_9m_val <- mod_PHS_frp_9m %>% 
  filter(DateTime >= "2019-01-01")

mod_PHS_frp_9m_summer <- mod_PHS_frp_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_PHS_frp_9m_summer_cal <- mod_PHS_frp_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_PHS_frp_9m_summer_val <- mod_PHS_frp_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

obs_PHS_frp <- read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  select(DateTime,Depth,PHS_frp) %>% 
  rename(PHS_frp_obs = PHS_frp)
obs_PHS_frp_9m <- obs_PHS_frp %>% 
  filter(Depth == 9) %>% 
  na.omit()
obs_PHS_frp_9m_cal <- obs_PHS_frp_9m %>% 
  filter(DateTime < "2019-01-01")
obs_PHS_frp_9m_val <- obs_PHS_frp_9m %>% 
  filter(DateTime >= "2019-01-01")

obs_PHS_frp_9m_summer <- obs_PHS_frp_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_PHS_frp_9m_summer_cal <- obs_PHS_frp_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_PHS_frp_9m_summer_val <- obs_PHS_frp_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
comb_9m_PHS_frp_summer <- left_join(obs_PHS_frp_9m_summer,mod_PHS_frp_9m_summer,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof$SRP <- gof(comb_9m_PHS_frp_summer$PHS_frp_mod,comb_9m_PHS_frp_summer$PHS_frp_obs)

comb_9m_PHS_frp_summer_cal <- left_join(obs_PHS_frp_9m_summer_cal,mod_PHS_frp_9m_summer_cal,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_cal$SRP <- gof(comb_9m_PHS_frp_summer_cal$PHS_frp_mod,comb_9m_PHS_frp_summer_cal$PHS_frp_obs)

comb_9m_PHS_frp_summer_val <- left_join(obs_PHS_frp_9m_summer_val,mod_PHS_frp_9m_summer_val,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_val$SRP <- gof(comb_9m_PHS_frp_summer_val$PHS_frp_mod,comb_9m_PHS_frp_summer_val$PHS_frp_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_PHS_frp <- left_join(obs_PHS_frp_9m,mod_PHS_frp_9m,by="DateTime")
all_9m_gof$SRP <- gof(comb_9m_PHS_frp$PHS_frp_mod,comb_9m_PHS_frp$PHS_frp_obs)

comb_9m_PHS_frp_cal <- left_join(obs_PHS_frp_9m_cal,mod_PHS_frp_9m_cal,by="DateTime")
all_9m_gof_cal$SRP <- gof(comb_9m_PHS_frp_cal$PHS_frp_mod,comb_9m_PHS_frp_cal$PHS_frp_obs)

comb_9m_PHS_frp_val <- left_join(obs_PHS_frp_9m_val,mod_PHS_frp_9m_val,by="DateTime")
all_9m_gof_val$SRP <- gof(comb_9m_PHS_frp_val$PHS_frp_mod,comb_9m_PHS_frp_val$PHS_frp_obs)

### 9m DOC + rDOC ####
# Summed DOC and rDOC following MakeFigure_ModeledvsObserved
mod_DOC_9m <- get_var(baseline,'OGM_doc',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(DOC_mod = OGM_doc_9)
mod_rDOC_9m <- get_var(baseline,'OGM_docr',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(rDOC_mod = OGM_docr_9)

mod_allDOC_9m <- left_join(mod_DOC_9m,mod_rDOC_9m,by="DateTime") %>% 
  mutate(allDOC_mod = DOC_mod + rDOC_mod)
mod_allDOC_9m_cal <- mod_allDOC_9m %>% 
  filter(DateTime < "2019-01-01")
mod_allDOC_9m_val <- mod_allDOC_9m %>% 
  filter(DateTime >= "2019-01-01")

mod_allDOC_9m_summer <- mod_allDOC_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_allDOC_9m_summer_cal <- mod_allDOC_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_allDOC_9m_summer_val <- mod_allDOC_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

obs_allDOC <- read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  select(DateTime,Depth,OGM_doc,OGM_docr) %>% 
  rename(DOC_obs = OGM_doc,rDOC_obs = OGM_docr) %>% 
  mutate(allDOC_obs = DOC_obs + rDOC_obs)
obs_allDOC_9m <- obs_allDOC %>% 
  filter(Depth == 9) %>% 
  na.omit()
obs_allDOC_9m_cal <- obs_allDOC_9m %>% 
  filter(DateTime < "2019-01-01")
obs_allDOC_9m_val <- obs_allDOC_9m %>% 
  filter(DateTime >= "2019-01-01")

obs_allDOC_9m_summer <- obs_allDOC_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_allDOC_9m_summer_cal <- obs_allDOC_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_allDOC_9m_summer_val <- obs_allDOC_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
comb_9m_allDOC_summer <- left_join(obs_allDOC_9m_summer,mod_allDOC_9m_summer,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof$DOC <- gof(comb_9m_allDOC_summer$allDOC_mod,comb_9m_allDOC_summer$allDOC_obs)

comb_9m_allDOC_summer_cal <- left_join(obs_allDOC_9m_summer_cal,mod_allDOC_9m_summer_cal,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_cal$DOC <- gof(comb_9m_allDOC_summer_cal$allDOC_mod,comb_9m_allDOC_summer_cal$allDOC_obs)

comb_9m_allDOC_summer_val <- left_join(obs_allDOC_9m_summer_val,mod_allDOC_9m_summer_val,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_val$DOC <- gof(comb_9m_allDOC_summer_val$allDOC_mod,comb_9m_allDOC_summer_val$allDOC_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_allDOC <- left_join(obs_allDOC_9m,mod_allDOC_9m,by="DateTime")
all_9m_gof$DOC <- gof(comb_9m_allDOC$allDOC_mod,comb_9m_allDOC$allDOC_obs)

comb_9m_allDOC_cal <- left_join(obs_allDOC_9m_cal,mod_allDOC_9m_cal,by="DateTime")
all_9m_gof_cal$DOC <- gof(comb_9m_allDOC_cal$allDOC_mod,comb_9m_allDOC_cal$allDOC_obs)

comb_9m_allDOC_val <- left_join(obs_allDOC_9m_val,mod_allDOC_9m_val,by="DateTime")
all_9m_gof_val$DOC <- gof(comb_9m_allDOC_val$allDOC_mod,comb_9m_allDOC_val$allDOC_obs)

### 9m TOTALS ###
# Calculations following MakeFigure_ModeledvsObserved
# Calculate modeled data
TN_9m <- get_var(baseline, "TOT_tn",z_out=9,reference = 'surface')
TP_9m <- get_var(baseline, "TOT_tp",z_out=9,reference = 'surface')
TOC_9m <- get_var(baseline, "TOT_toc",z_out=9,reference = 'surface')

cyano_9m <- get_var(baseline,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoN = CyanoConc*0.12,
         cyanoP = CyanoConc*0.0005,
         cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoN, cyanoP, cyanoC)

green_9m <- get_var(baseline,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenN = GreenConc*0.12,
         greenP = GreenConc*0.0005,
         greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenN, greenP, greenC)

diatom_9m <- get_var(baseline,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomN = DiatomConc*0.12,
         diatomP = DiatomConc*0.0005,
         diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth, diatomN, diatomP, diatomC)

mod_totals_9m<-as.data.frame(cbind(diatom_9m[1:5],cyano_9m[,3:5],green_9m[,3:5],TN_9m[,2],TP_9m[,2],TOC_9m[,2])) 
colnames(mod_totals_9m) = c("DateTime", "Depth", "diatomN", "diatomP", "diatomC",
                      "cyanoN", "cyanoP", "cyanoC", "greenN", "greenP", "greenC",
                      "TN", "TP", "TOC") 

mod_totals2_9m <- mod_totals_9m %>% 
  mutate(TN_mod = TN + diatomN + cyanoN + greenN,
         TP_mod = TP + diatomP + cyanoP + greenP,
         TOC_mod = TOC + diatomC + cyanoC + greenC) %>% 
  select(DateTime, TN_mod:TOC_mod) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))
mod_totals2_9m_cal <- mod_totals2_9m %>% 
  filter(DateTime < "2019-01-01")
mod_totals2_9m_val <- mod_totals2_9m %>% 
  filter(DateTime >= "2019-01-01")

mod_totals2_9m_summer <- mod_totals2_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_totals2_9m_summer_cal <- mod_totals2_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
mod_totals2_9m_summer_val <- mod_totals2_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Observed data
obs_Totals <- read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/totalNP.csv',header=TRUE) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz='EST'))) %>% 
  rename(TN_obs = TOT_tn, TP_obs = TOT_tp)
obs_Totals_9m <- obs_Totals %>% 
  filter(Depth == 9)
obs_Totals_9m_cal <- obs_Totals_9m %>% 
  filter(DateTime < "2019-01-01")
obs_Totals_9m_val <- obs_Totals_9m %>% 
  filter(DateTime >= "2019-01-01")

obs_Totals_9m_summer <- obs_Totals_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_Totals_9m_summer_cal <- obs_Totals_9m_cal %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")
obs_Totals_9m_summer_val <- obs_Totals_9m_val %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
comb_9m_Totals_summer <- left_join(obs_Totals_9m_summer,mod_totals2_9m_summer,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof$TP <- gof(comb_9m_Totals_summer$TP_mod,comb_9m_Totals_summer$TP_obs,na.rm=TRUE)

comb_9m_Totals_summer_cal <- left_join(obs_Totals_9m_summer_cal,mod_totals2_9m_summer_cal,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_cal$TP <- gof(comb_9m_Totals_summer_cal$TP_mod,comb_9m_Totals_summer_cal$TP_obs,na.rm=TRUE)

comb_9m_Totals_summer_val <- left_join(obs_Totals_9m_summer_val,mod_totals2_9m_summer_val,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))
summer_9m_gof_val$TP <- gof(comb_9m_Totals_summer_val$TP_mod,comb_9m_Totals_summer_val$TP_obs,na.rm=TRUE)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_Totals <- left_join(obs_Totals_9m,mod_totals2_9m,by="DateTime")
all_9m_gof$TP <- gof(comb_9m_Totals$TP_mod,comb_9m_Totals$TP_obs,na.rm=TRUE)

comb_9m_Totals_cal <- left_join(obs_Totals_9m_cal,mod_totals2_9m_cal,by="DateTime")
all_9m_gof_cal$TP <- gof(comb_9m_Totals_cal$TP_mod,comb_9m_Totals_cal$TP_obs,na.rm=TRUE)

comb_9m_Totals_val <- left_join(obs_Totals_9m_val,mod_totals2_9m_val,by="DateTime")
all_9m_gof_val$TP <- gof(comb_9m_Totals_val$TP_mod,comb_9m_Totals_val$TP_obs,na.rm=TRUE)

# Add TN
summer_9m_gof$TN <- gof(comb_9m_Totals_summer$TN_mod,comb_9m_Totals_summer$TN_obs,na.rm=TRUE)
summer_9m_gof_cal$TN <- gof(comb_9m_Totals_summer_cal$TN_mod,comb_9m_Totals_summer_cal$TN_obs,na.rm=TRUE)
summer_9m_gof_val$TN <- gof(comb_9m_Totals_summer_val$TN_mod,comb_9m_Totals_summer_val$TN_obs,na.rm=TRUE)

all_9m_gof$TN <- gof(comb_9m_Totals$TN_mod,comb_9m_Totals$TN_obs,na.rm=TRUE)
all_9m_gof_cal$TN <- gof(comb_9m_Totals_cal$TN_mod,comb_9m_Totals_cal$TN_obs,na.rm=TRUE)
all_9m_gof_val$TN <- gof(comb_9m_Totals_val$TN_mod,comb_9m_Totals_val$TN_obs,na.rm=TRUE)

####################### COMPLETED: 9m_Summer and 9m_FullYear for full period, calibration period, and validation period ####################

### NEXT UP: Format tables for 9m summer and 9m full year (following Ward et al. 2020 for now) ###
