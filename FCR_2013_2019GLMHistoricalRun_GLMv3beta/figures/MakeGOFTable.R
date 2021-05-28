# Script to make GOF tables for main MS and supplementary
# 12 Jan 2021
# A Hounshell
# Updated w/ new output file: 04 Mar 2021 - focus on FULL YEAR ONLY!

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
# Download nc_file from dropbox link: https://www.dropbox.com/s/q3n96xfwztyamiv/output_2013_2019.nc?dl=0
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
# summer_9m_gof <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
# summer_9m_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","NRMSE%_all","PBIAS%_all","RSR_all","rSD_all","NSE_all","mNSE_all","rNSE_all","d_all","md_all","rd_all","cp_all","r_all",
#                              "R2_all","bR2_all","KGE_all","VE_all")

# Full year, 9m, full period (2013-2019)
all_9m_gof <- setNames(data.frame(matrix(ncol=9,nrow=21)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_9m_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","NRMSE%_all","PBIAS%_all","RSR_all","rSD_all","NSE_all","mNSE_all","rNSE_all","d_all","md_all","rd_all","cp_all","r_all",
             "R2_all","bR2_all","KGE_all","VE_all","r.Spearman")

# Summer, 9m, calibration period (2013-2018)
# summer_9m_gof_cal <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
# summer_9m_gof_cal$Parameter <- c("ME_cal","MAE_cal","MSE_cal","RMSE_cal","NRMSE%_cal","PBIAS%_cal","RSR_cal","rSD_cal","NSE_cal","mNSE_cal","rNSE_cal","d_cal","md_cal","rd_cal","cp_cal","r_cal",
#                                  "R2_cal","bR2_cal","KGE_cal","VE_cal")

# Full year, 9m, calibration period (2013-2018)
all_9m_gof_cal <- setNames(data.frame(matrix(ncol=9,nrow=21)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_9m_gof_cal$Parameter <- c("ME_cal","MAE_cal","MSE_cal","RMSE_cal","NRMSE%_cal","PBIAS%_cal","RSR_cal","rSD_cal","NSE_cal","mNSE_cal","rNSE_cal","d_cal","md_cal","rd_cal","cp_cal","r_cal",
                              "R2_cal","bR2_cal","KGE_cal","VE_cal","r.Spearman")

# Summer, 9m, validation period (2019)
# summer_9m_gof_val <- setNames(data.frame(matrix(ncol=9,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
# summer_9m_gof_val$Parameter <- c("ME_val","MAE_val","MSE_val","RMSE_val","NRMSE%_val","PBIAS%_val","RSR_val","rSD_val","NSE_val","mNSE_val","rNSE_val","d_val","md_val","rd_val","cp_val","r_val",
#                                  "R2_val","bR2_val","KGE_val","VE_val")

# Full year, 9m, validation period (2019)
all_9m_gof_val <- setNames(data.frame(matrix(ncol=9,nrow=21)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_9m_gof_val$Parameter <- c("ME_val","MAE_val","MSE_val","RMSE_val","NRMSE%_val","PBIAS%_val","RSR_val","rSD_val","NSE_val","mNSE_val","rNSE_val","d_val","md_val","rd_val","cp_val","r_val",
                              "R2_val","bR2_val","KGE_val","VE_val","r.Spearman")

### 9m Summer TEMP ###
# 9m Model temp
mod_temp_9m <- get_temp(baseline,z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(temp_mod = temp_9)
mod_temp_9m_cal <- mod_temp_9m %>% 
  filter(DateTime < "2019-01-01")
mod_temp_9m_val <- mod_temp_9m %>% 
  filter(DateTime >= "2019-01-01")

# mod_temp_9m_summer <- mod_temp_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_temp_9m_summer_cal <- mod_temp_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_temp_9m_summer_val <- mod_temp_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

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
# comb_9m_temp_summer <- left_join(obs_temp_9m_summer,mod_temp_9m_summer,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof$Temp <- gof(comb_9m_temp_summer$temp_mod,comb_9m_temp_summer$temp_obs)
# 
# comb_9m_temp_summer_cal <- left_join(obs_temp_9m_summer_cal,mod_temp_9m_summer_cal,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_cal$Temp <- gof(comb_9m_temp_summer_cal$temp_mod,comb_9m_temp_summer_cal$temp_obs)
# 
# comb_9m_temp_summer_val <- left_join(obs_temp_9m_summer_val,mod_temp_9m_summer_val,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_val$Temp <- gof(comb_9m_temp_summer_val$temp_mod,comb_9m_temp_summer_val$temp_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later)
# For FULL year
comb_9m_temp <- left_join(obs_temp_9m,mod_temp_9m,by="DateTime")
all_9m_gof$Temp <- gof(comb_9m_temp$temp_mod,comb_9m_temp$temp_obs,do.spearman = TRUE)

comb_9m_temp_cal <- left_join(obs_temp_9m_cal,mod_temp_9m_cal,by="DateTime")
all_9m_gof_cal$Temp <- gof(comb_9m_temp_cal$temp_mod,comb_9m_temp_cal$temp_obs,do.spearman = TRUE)

comb_9m_temp_val <- left_join(obs_temp_9m_val,mod_temp_9m_val,by="DateTime")
all_9m_gof_val$Temp <- gof(comb_9m_temp_val$temp_mod,comb_9m_temp_val$temp_obs,do.spearman = TRUE)

### 9m summer OXY ###
# 9m modeled oxy
mod_oxy_9m <- get_var(baseline,'OXY_oxy',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(oxy_mod = OXY_oxy_9)
mod_oxy_9m_cal <- mod_oxy_9m %>% 
  filter(DateTime < "2019-01-01")
mod_oxy_9m_val <- mod_oxy_9m %>% 
  filter(DateTime >= "2019-01-01")

# mod_oxy_9m_summer <- mod_oxy_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_oxy_9m_summer_cal <- mod_oxy_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_oxy_9m_summer_val <- mod_oxy_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

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

# obs_oxy_9m_summer <- obs_oxy_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_oxy_9m_summer_cal <- obs_oxy_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_oxy_9m_summer_val <- obs_oxy_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
# comb_9m_oxy_summer <- left_join(obs_oxy_9m_summer,mod_oxy_9m_summer,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof$Oxy <- gof(comb_9m_oxy_summer$oxy_mod,comb_9m_oxy_summer$oxy_obs)
# 
# comb_9m_oxy_summer_cal <- left_join(obs_oxy_9m_summer_cal,mod_oxy_9m_summer_cal,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_cal$Oxy <- gof(comb_9m_oxy_summer_cal$oxy_mod,comb_9m_oxy_summer_cal$oxy_obs)
# 
# comb_9m_oxy_summer_val <- left_join(obs_oxy_9m_summer_val,mod_oxy_9m_summer_val,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_val$Oxy <- gof(comb_9m_oxy_summer_val$oxy_mod,comb_9m_oxy_summer_val$oxy_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_oxy <- left_join(obs_oxy_9m,mod_oxy_9m,by="DateTime")
all_9m_gof$Oxy <- gof(comb_9m_oxy$oxy_mod,comb_9m_oxy$oxy_obs,do.spearman = TRUE)

comb_9m_oxy_cal <- left_join(obs_oxy_9m_cal,mod_oxy_9m_cal,by="DateTime")
all_9m_gof_cal$Oxy <- gof(comb_9m_oxy_cal$oxy_mod,comb_9m_oxy_cal$oxy_obs,do.spearman = TRUE)

comb_9m_oxy_val <- left_join(obs_oxy_9m_val,mod_oxy_9m_val,by="DateTime")
all_9m_gof_val$Oxy <- gof(comb_9m_oxy_val$oxy_mod,comb_9m_oxy_val$oxy_obs,do.spearman = TRUE)

### 9 m summer NIT-amm ###
mod_NIT_amm_9m <- get_var(baseline,'NIT_amm',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(NIT_amm_mod = NIT_amm_9)
mod_NIT_amm_9m_cal <- mod_NIT_amm_9m %>% 
  filter(DateTime < "2019-01-01")
mod_NIT_amm_9m_val <- mod_NIT_amm_9m %>% 
  filter(DateTime >= "2019-01-01")

# mod_NIT_amm_9m_summer <- mod_NIT_amm_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_NIT_amm_9m_summer_cal <- mod_NIT_amm_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_NIT_amm_9m_summer_val <- mod_NIT_amm_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

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

# obs_NIT_amm_9m_summer <- obs_NIT_amm_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_NIT_amm_9m_summer_cal <- obs_NIT_amm_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_NIT_amm_9m_summer_val <- obs_NIT_amm_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
# comb_9m_NIT_amm_summer <- left_join(obs_NIT_amm_9m_summer,mod_NIT_amm_9m_summer,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof$NIT_amm <- gof(comb_9m_NIT_amm_summer$NIT_amm_mod,comb_9m_NIT_amm_summer$NIT_amm_obs)
# 
# comb_9m_NIT_amm_summer_cal <- left_join(obs_NIT_amm_9m_summer_cal,mod_NIT_amm_9m_summer_cal,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_cal$NIT_amm <- gof(comb_9m_NIT_amm_summer_cal$NIT_amm_mod,comb_9m_NIT_amm_summer_cal$NIT_amm_obs)
# 
# comb_9m_NIT_amm_summer_val <- left_join(obs_NIT_amm_9m_summer_val,mod_NIT_amm_9m_summer_val,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_val$NIT_amm <- gof(comb_9m_NIT_amm_summer_val$NIT_amm_mod,comb_9m_NIT_amm_summer_val$NIT_amm_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_NIT_amm <- left_join(obs_NIT_amm_9m,mod_NIT_amm_9m,by="DateTime")
all_9m_gof$NIT_amm <- gof(comb_9m_NIT_amm$NIT_amm_mod,comb_9m_NIT_amm$NIT_amm_obs,do.spearman = TRUE)

comb_9m_NIT_amm_cal <- left_join(obs_NIT_amm_9m_cal,mod_NIT_amm_9m_cal,by="DateTime")
all_9m_gof_cal$NIT_amm <- gof(comb_9m_NIT_amm_cal$NIT_amm_mod,comb_9m_NIT_amm_cal$NIT_amm_obs,do.spearman = TRUE)

comb_9m_NIT_amm_val <- left_join(obs_NIT_amm_9m_val,mod_NIT_amm_9m_val,by="DateTime")
all_9m_gof_val$NIT_amm <- gof(comb_9m_NIT_amm_val$NIT_amm_mod,comb_9m_NIT_amm_val$NIT_amm_obs,do.spearman = TRUE)

### 9m summer NIT_nit ###
mod_NIT_nit_9m <- get_var(baseline,'NIT_nit',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(NIT_nit_mod = NIT_nit_9)
mod_NIT_nit_9m_cal <- mod_NIT_nit_9m %>% 
  filter(DateTime < "2019-01-01")
mod_NIT_nit_9m_val <- mod_NIT_nit_9m %>% 
  filter(DateTime >= "2019-01-01")

# mod_NIT_nit_9m_summer <- mod_NIT_nit_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_NIT_nit_9m_summer_cal <- mod_NIT_nit_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_NIT_nit_9m_summer_val <- mod_NIT_nit_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

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

# obs_NIT_nit_9m_summer <- obs_NIT_nit_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_NIT_nit_9m_summer_cal <- obs_NIT_nit_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_NIT_nit_9m_summer_val <- obs_NIT_nit_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
# comb_9m_NIT_nit_summer <- left_join(obs_NIT_nit_9m_summer,mod_NIT_nit_9m_summer,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof$NIT_nit <- gof(comb_9m_NIT_nit_summer$NIT_nit_mod,comb_9m_NIT_nit_summer$NIT_nit_obs)
# 
# comb_9m_NIT_nit_summer_cal <- left_join(obs_NIT_nit_9m_summer_cal,mod_NIT_nit_9m_summer_cal,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_cal$NIT_nit <- gof(comb_9m_NIT_nit_summer_cal$NIT_nit_mod,comb_9m_NIT_nit_summer_cal$NIT_nit_obs)
# 
# comb_9m_NIT_nit_summer_val <- left_join(obs_NIT_nit_9m_summer_val,mod_NIT_nit_9m_summer_val,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_val$NIT_nit <- gof(comb_9m_NIT_nit_summer_val$NIT_nit_mod,comb_9m_NIT_nit_summer_val$NIT_nit_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_NIT_nit <- left_join(obs_NIT_nit_9m,mod_NIT_nit_9m,by="DateTime")
all_9m_gof$NIT_nit <- gof(comb_9m_NIT_nit$NIT_nit_mod,comb_9m_NIT_nit$NIT_nit_obs,do.spearman = TRUE)

comb_9m_NIT_nit_cal <- left_join(obs_NIT_nit_9m_cal,mod_NIT_nit_9m_cal,by="DateTime")
all_9m_gof_cal$NIT_nit <- gof(comb_9m_NIT_nit_cal$NIT_nit_mod,comb_9m_NIT_nit_cal$NIT_nit_obs,do.spearman = TRUE)

comb_9m_NIT_nit_val <- left_join(obs_NIT_nit_9m_val,mod_NIT_nit_9m_val,by="DateTime")
all_9m_gof_val$NIT_nit <- gof(comb_9m_NIT_nit_val$NIT_nit_mod,comb_9m_NIT_nit_val$NIT_nit_obs,do.spearman = TRUE)

### 9 m SRP ###
mod_PHS_frp_9m <- get_var(baseline,'PHS_frp',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(PHS_frp_mod = PHS_frp_9)
mod_PHS_frp_9m_cal <- mod_PHS_frp_9m %>% 
  filter(DateTime < "2019-01-01")
mod_PHS_frp_9m_val <- mod_PHS_frp_9m %>% 
  filter(DateTime >= "2019-01-01")

# mod_PHS_frp_9m_summer <- mod_PHS_frp_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_PHS_frp_9m_summer_cal <- mod_PHS_frp_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_PHS_frp_9m_summer_val <- mod_PHS_frp_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

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

# obs_PHS_frp_9m_summer <- obs_PHS_frp_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_PHS_frp_9m_summer_cal <- obs_PHS_frp_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_PHS_frp_9m_summer_val <- obs_PHS_frp_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
# comb_9m_PHS_frp_summer <- left_join(obs_PHS_frp_9m_summer,mod_PHS_frp_9m_summer,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof$SRP <- gof(comb_9m_PHS_frp_summer$PHS_frp_mod,comb_9m_PHS_frp_summer$PHS_frp_obs)
# 
# comb_9m_PHS_frp_summer_cal <- left_join(obs_PHS_frp_9m_summer_cal,mod_PHS_frp_9m_summer_cal,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_cal$SRP <- gof(comb_9m_PHS_frp_summer_cal$PHS_frp_mod,comb_9m_PHS_frp_summer_cal$PHS_frp_obs)
# 
# comb_9m_PHS_frp_summer_val <- left_join(obs_PHS_frp_9m_summer_val,mod_PHS_frp_9m_summer_val,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_val$SRP <- gof(comb_9m_PHS_frp_summer_val$PHS_frp_mod,comb_9m_PHS_frp_summer_val$PHS_frp_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_PHS_frp <- left_join(obs_PHS_frp_9m,mod_PHS_frp_9m,by="DateTime")
all_9m_gof$SRP <- gof(comb_9m_PHS_frp$PHS_frp_mod,comb_9m_PHS_frp$PHS_frp_obs,do.spearman = TRUE)

comb_9m_PHS_frp_cal <- left_join(obs_PHS_frp_9m_cal,mod_PHS_frp_9m_cal,by="DateTime")
all_9m_gof_cal$SRP <- gof(comb_9m_PHS_frp_cal$PHS_frp_mod,comb_9m_PHS_frp_cal$PHS_frp_obs,do.spearman = TRUE)

comb_9m_PHS_frp_val <- left_join(obs_PHS_frp_9m_val,mod_PHS_frp_9m_val,by="DateTime")
all_9m_gof_val$SRP <- gof(comb_9m_PHS_frp_val$PHS_frp_mod,comb_9m_PHS_frp_val$PHS_frp_obs,do.spearman = TRUE)

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

# mod_allDOC_9m_summer <- mod_allDOC_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_allDOC_9m_summer_cal <- mod_allDOC_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# mod_allDOC_9m_summer_val <- mod_allDOC_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

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

# obs_allDOC_9m_summer <- obs_allDOC_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_allDOC_9m_summer_cal <- obs_allDOC_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_allDOC_9m_summer_val <- obs_allDOC_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
# comb_9m_allDOC_summer <- left_join(obs_allDOC_9m_summer,mod_allDOC_9m_summer,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof$DOC <- gof(comb_9m_allDOC_summer$allDOC_mod,comb_9m_allDOC_summer$allDOC_obs)
# 
# comb_9m_allDOC_summer_cal <- left_join(obs_allDOC_9m_summer_cal,mod_allDOC_9m_summer_cal,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_cal$DOC <- gof(comb_9m_allDOC_summer_cal$allDOC_mod,comb_9m_allDOC_summer_cal$allDOC_obs)
# 
# comb_9m_allDOC_summer_val <- left_join(obs_allDOC_9m_summer_val,mod_allDOC_9m_summer_val,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_val$DOC <- gof(comb_9m_allDOC_summer_val$allDOC_mod,comb_9m_allDOC_summer_val$allDOC_obs)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_allDOC <- left_join(obs_allDOC_9m,mod_allDOC_9m,by="DateTime")
all_9m_gof$DOC <- gof(comb_9m_allDOC$allDOC_mod,comb_9m_allDOC$allDOC_obs,do.spearman = TRUE)

comb_9m_allDOC_cal <- left_join(obs_allDOC_9m_cal,mod_allDOC_9m_cal,by="DateTime")
all_9m_gof_cal$DOC <- gof(comb_9m_allDOC_cal$allDOC_mod,comb_9m_allDOC_cal$allDOC_obs,do.spearman = TRUE)

comb_9m_allDOC_val <- left_join(obs_allDOC_9m_val,mod_allDOC_9m_val,by="DateTime")
all_9m_gof_val$DOC <- gof(comb_9m_allDOC_val$allDOC_mod,comb_9m_allDOC_val$allDOC_obs,do.spearman = TRUE)

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

# obs_Totals_9m_summer <- obs_Totals_9m %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_Totals_9m_summer_cal <- obs_Totals_9m_cal %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")
# obs_Totals_9m_summer_val <- obs_Totals_9m_val %>% 
#   mutate(Month = format(DateTime,"%m-%d")) %>% 
#   filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# SUMMER ONLY
# comb_9m_Totals_summer <- left_join(obs_Totals_9m_summer,mod_totals2_9m_summer,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof$TP <- gof(comb_9m_Totals_summer$TP_mod,comb_9m_Totals_summer$TP_obs,na.rm=TRUE)
# 
# comb_9m_Totals_summer_cal <- left_join(obs_Totals_9m_summer_cal,mod_totals2_9m_summer_cal,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_cal$TP <- gof(comb_9m_Totals_summer_cal$TP_mod,comb_9m_Totals_summer_cal$TP_obs,na.rm=TRUE)
# 
# comb_9m_Totals_summer_val <- left_join(obs_Totals_9m_summer_val,mod_totals2_9m_summer_val,by="DateTime") %>% 
#   select(-c("Month.x","Month.y"))
# summer_9m_gof_val$TP <- gof(comb_9m_Totals_summer_val$TP_mod,comb_9m_Totals_summer_val$TP_obs,na.rm=TRUE)

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_9m_Totals <- left_join(obs_Totals_9m,mod_totals2_9m,by="DateTime")
all_9m_gof$TP <- gof(comb_9m_Totals$TP_mod,comb_9m_Totals$TP_obs,na.rm=TRUE,do.spearman = TRUE)

comb_9m_Totals_cal <- left_join(obs_Totals_9m_cal,mod_totals2_9m_cal,by="DateTime")
all_9m_gof_cal$TP <- gof(comb_9m_Totals_cal$TP_mod,comb_9m_Totals_cal$TP_obs,na.rm=TRUE,do.spearman = TRUE)

comb_9m_Totals_val <- left_join(obs_Totals_9m_val,mod_totals2_9m_val,by="DateTime")
all_9m_gof_val$TP <- gof(comb_9m_Totals_val$TP_mod,comb_9m_Totals_val$TP_obs,na.rm=TRUE,do.spearman = TRUE)

# Add TN
# summer_9m_gof$TN <- gof(comb_9m_Totals_summer$TN_mod,comb_9m_Totals_summer$TN_obs,na.rm=TRUE)
# summer_9m_gof_cal$TN <- gof(comb_9m_Totals_summer_cal$TN_mod,comb_9m_Totals_summer_cal$TN_obs,na.rm=TRUE)
# summer_9m_gof_val$TN <- gof(comb_9m_Totals_summer_val$TN_mod,comb_9m_Totals_summer_val$TN_obs,na.rm=TRUE)

all_9m_gof$TN <- gof(comb_9m_Totals$TN_mod,comb_9m_Totals$TN_obs,na.rm=TRUE,do.spearman = TRUE)
all_9m_gof_cal$TN <- gof(comb_9m_Totals_cal$TN_mod,comb_9m_Totals_cal$TN_obs,na.rm=TRUE,do.spearman = TRUE)
all_9m_gof_val$TN <- gof(comb_9m_Totals_val$TN_mod,comb_9m_Totals_val$TN_obs,na.rm=TRUE,do.spearman = TRUE)

## Add NMAE calculation for all parameters
# all_9m_gof
all_9m_gof[nrow(all_9m_gof)+1,] <- NA
all_9m_gof[22,1] <- "NMAE_all"
all_9m_gof$Parameter[21] <- "r.Spearman_all"
all_9m_gof$Temp[22] <- round(all_9m_gof$Temp[2]/mean(comb_9m_temp$temp_obs),digits = 2)
all_9m_gof$Oxy[22] <- round(all_9m_gof$Oxy[2]/mean(comb_9m_oxy$oxy_obs),digits = 2)
all_9m_gof$NIT_amm[22] <- round(all_9m_gof$NIT_amm[2]/mean(comb_9m_NIT_amm$NIT_amm_obs),digits = 2)
all_9m_gof$NIT_nit[22] <- round(all_9m_gof$NIT_nit[2]/mean(comb_9m_NIT_nit$NIT_nit_obs),digits = 2)
all_9m_gof$SRP[22] <- round(all_9m_gof$SRP[2]/mean(comb_9m_PHS_frp$PHS_frp_obs),digits = 2)
all_9m_gof$DOC[22] <- round(all_9m_gof$DOC[2]/mean(comb_9m_allDOC$allDOC_obs),digits = 2)
all_9m_gof$TN[22] <- round(all_9m_gof$TN[2]/mean(comb_9m_Totals$TN_obs,na.rm=TRUE),digits = 2)
all_9m_gof$TP[22] <- round(all_9m_gof$TP[2]/mean(comb_9m_Totals$TP_obs,na.rm=TRUE),digits = 2)

# all_9m_gof_cal
all_9m_gof_cal[nrow(all_9m_gof_cal)+1,] <- NA
all_9m_gof_cal[22,1] <- "NMAE_cal"
all_9m_gof_cal$Parameter[21] <- "r.Spearman_cal"
all_9m_gof_cal$Temp[22] <- round(all_9m_gof_cal$Temp[2]/mean(comb_9m_temp_cal$temp_obs),digits = 2)
all_9m_gof_cal$Oxy[22] <- round(all_9m_gof_cal$Oxy[2]/mean(comb_9m_oxy_cal$oxy_obs),digits = 2)
all_9m_gof_cal$NIT_amm[22] <- round(all_9m_gof_cal$NIT_amm[2]/mean(comb_9m_NIT_amm_cal$NIT_amm_obs),digits = 2)
all_9m_gof_cal$NIT_nit[22] <- round(all_9m_gof_cal$NIT_nit[2]/mean(comb_9m_NIT_nit_cal$NIT_nit_obs),digits = 2)
all_9m_gof_cal$SRP[22] <- round(all_9m_gof_cal$SRP[2]/mean(comb_9m_PHS_frp_cal$PHS_frp_obs),digits = 2)
all_9m_gof_cal$DOC[22] <- round(all_9m_gof_cal$DOC[2]/mean(comb_9m_allDOC_cal$allDOC_obs),digits = 2)
all_9m_gof_cal$TN[22] <- round(all_9m_gof_cal$TN[2]/mean(comb_9m_Totals_cal$TN_obs,na.rm=TRUE),digits = 2)
all_9m_gof_cal$TP[22] <- round(all_9m_gof_cal$TP[2]/mean(comb_9m_Totals_cal$TP_obs,na.rm=TRUE),digits = 2)

# all_9m_gof_val
all_9m_gof_val[nrow(all_9m_gof_cal)+1,] <- NA
all_9m_gof_val[22,1] <- "NMAE_val"
all_9m_gof_val$Parameter[21] <- "r.Spearman_val"
all_9m_gof_val$Temp[22] <- round(all_9m_gof_val$Temp[2]/mean(comb_9m_temp_val$temp_obs),digits = 2)
all_9m_gof_val$Oxy[22] <- round(all_9m_gof_val$Oxy[2]/mean(comb_9m_oxy_val$oxy_obs),digits = 2)
all_9m_gof_val$NIT_amm[22] <- round(all_9m_gof_val$NIT_amm[2]/mean(comb_9m_NIT_amm_val$NIT_amm_obs),digits = 2)
all_9m_gof_val$NIT_nit[22] <- round(all_9m_gof_val$NIT_nit[2]/mean(comb_9m_NIT_nit_val$NIT_nit_obs),digits = 2)
all_9m_gof_val$SRP[22] <- round(all_9m_gof_val$SRP[2]/mean(comb_9m_PHS_frp_val$PHS_frp_obs),digits = 2)
all_9m_gof_val$DOC[22] <- round(all_9m_gof_val$DOC[2]/mean(comb_9m_allDOC_val$allDOC_obs),digits = 2)
all_9m_gof_val$TN[22] <- round(all_9m_gof_val$TN[2]/mean(comb_9m_Totals_val$TN_obs,na.rm=TRUE),digits = 2)
all_9m_gof_val$TP[22] <- round(all_9m_gof_val$TP[2]/mean(comb_9m_Totals_val$TP_obs,na.rm=TRUE),digits = 2)

####################### COMPLETED: 9m_Summer and 9m_FullYear for full period, calibration period, and validation period ####################

### NEXT UP: Format tables for 9m summer and 9m full year (following Ward et al. 2020 for now) ###
# Select GOF variables of interest for each time period
# summer_n_all <- c("n_all",length(comb_9m_temp_summer$DateTime),length(comb_9m_oxy_summer$DateTime),length(comb_9m_NIT_amm_summer$DateTime),
#           length(comb_9m_NIT_nit_summer$DateTime),length(comb_9m_PHS_frp_summer$DateTime),length(comb_9m_allDOC_summer$DateTime),
#           length(comb_9m_Totals_summer$TN_obs[!is.na(comb_9m_Totals_summer$TN_obs)]),length(comb_9m_Totals_summer$TP_obs[!is.na(comb_9m_Totals_summer$TP_obs)]))
# 
# summer_9m_gof_all_table <- summer_9m_gof %>% 
#   filter(Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NSE_all" | Parameter == "MAE_all")
# 
# summer_n_cal <- c("n_cal",length(comb_9m_temp_summer_cal$DateTime),length(comb_9m_oxy_summer_cal$DateTime),length(comb_9m_NIT_amm_summer_cal$DateTime),
#                   length(comb_9m_NIT_nit_summer_cal$DateTime),length(comb_9m_PHS_frp_summer_cal$DateTime),length(comb_9m_allDOC_summer_cal$DateTime),
#                   length(comb_9m_Totals_summer_cal$TN_obs[!is.na(comb_9m_Totals_summer_cal$TN_obs)]),length(comb_9m_Totals_summer_cal$TP_obs[!is.na(comb_9m_Totals_summer_cal$TP_obs)]))
# 
# summer_9m_gof_cal_table <- summer_9m_gof_cal %>% 
#   filter(Parameter == "R2_cal" | Parameter == "RMSE_cal" | Parameter == "PBIAS%_cal" | Parameter == "NSE_cal" | Parameter == "MAE_cal")
# 
# summer_n_val <- c("n_val",length(comb_9m_temp_summer_val$DateTime),length(comb_9m_oxy_summer_val$DateTime),length(comb_9m_NIT_amm_summer_val$DateTime),
#                   length(comb_9m_NIT_nit_summer_val$DateTime),length(comb_9m_PHS_frp_summer_val$DateTime),length(comb_9m_allDOC_summer_val$DateTime),
#                   length(comb_9m_Totals_summer_val$TN_obs[!is.na(comb_9m_Totals_summer_val$TN_obs)]),length(comb_9m_Totals_summer_val$TP_obs[!is.na(comb_9m_Totals_summer_val$TP_obs)]))
# 
# summer_9m_gof_val_table <- summer_9m_gof_val %>% 
#   filter(Parameter == "R2_val" | Parameter == "RMSE_val" | Parameter == "PBIAS%_val" | Parameter == "NSE_val" | Parameter == "MAE_val")
# 
# summer_9m_gof_table <- rbind(summer_n_all,summer_9m_gof_all_table,summer_n_cal,summer_9m_gof_cal_table,summer_n_val,summer_9m_gof_val_table)

# Select GOF variables for the full year
full_n_all <- c("n_all",length(comb_9m_temp$DateTime),length(comb_9m_oxy$DateTime),length(comb_9m_NIT_amm$DateTime),
                length(comb_9m_NIT_nit$DateTime),length(comb_9m_PHS_frp$DateTime),length(comb_9m_allDOC$DateTime),
                length(comb_9m_Totals$TN_obs[!is.na(comb_9m_Totals$TN_obs)]),length(comb_9m_Totals$TP_obs[!is.na(comb_9m_Totals$TP_obs)]))

full_9m_gof_all_table <- all_9m_gof %>% 
  filter(Parameter == "r.Spearman_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NSE_all" | Parameter == "NMAE_all")

full_n_cal <- c("n_cal",length(comb_9m_temp_cal$DateTime),length(comb_9m_oxy_cal$DateTime),length(comb_9m_NIT_amm_cal$DateTime),
                length(comb_9m_NIT_nit_cal$DateTime),length(comb_9m_PHS_frp_cal$DateTime),length(comb_9m_allDOC_cal$DateTime),
                length(comb_9m_Totals_cal$TN_obs[!is.na(comb_9m_Totals_cal$TN_obs)]),length(comb_9m_Totals_cal$TP_obs[!is.na(comb_9m_Totals_cal$TP_obs)]))

full_9m_gof_cal_table <- all_9m_gof_cal %>% 
  filter(Parameter == "r.Spearman_cal" | Parameter == "RMSE_cal" | Parameter == "PBIAS%_cal" | Parameter == "NSE_cal" | Parameter == "NMAE_cal")

full_n_val <- c("n_val",length(comb_9m_temp_val$DateTime),length(comb_9m_oxy_val$DateTime),length(comb_9m_NIT_amm_val$DateTime),
                length(comb_9m_NIT_nit_val$DateTime),length(comb_9m_PHS_frp_val$DateTime),length(comb_9m_allDOC_val$DateTime),
                length(comb_9m_Totals_val$TN_obs[!is.na(comb_9m_Totals_val$TN_obs)]),length(comb_9m_Totals_val$TP_obs[!is.na(comb_9m_Totals_val$TP_obs)]))

full_9m_gof_val_table <- all_9m_gof_val %>% 
  filter(Parameter == "r.Spearman_val" | Parameter == "RMSE_val" | Parameter == "PBIAS%_val" | Parameter == "NSE_val" | Parameter == "NMAE_val")

full_9m_gof_table <- rbind(full_n_all,full_9m_gof_all_table,full_n_cal,full_9m_gof_cal_table,full_n_val,full_9m_gof_val_table)

# write_csv(summer_9m_gof_table,'FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/table_gof_9m_summer.csv')

write_csv(full_9m_gof_table,'FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/table_gof_9m_fullyear.csv')

############################# Same thing but for full water column ############################
# For full year only!

# Create empty dataframe for GOF results
# Using all GOF parameters for now - will cull eventually

# Full year, full water column, full period (2013-2019)
all_gof <- setNames(data.frame(matrix(ncol=9,nrow=21)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","NRMSE%_all","PBIAS%_all","RSR_all","rSD_all","NSE_all","mNSE_all","rNSE_all","d_all","md_all","rd_all","cp_all","r_all",
                          "R2_all","bR2_all","KGE_all","VE_all","r.Spearman")

# Full year, full water column, calibration period (2013-2018)
all_gof_cal <- setNames(data.frame(matrix(ncol=9,nrow=21)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_gof_cal$Parameter <- c("ME_cal","MAE_cal","MSE_cal","RMSE_cal","NRMSE%_cal","PBIAS%_cal","RSR_cal","rSD_cal","NSE_cal","mNSE_cal","rNSE_cal","d_cal","md_cal","rd_cal","cp_cal","r_cal",
                              "R2_cal","bR2_cal","KGE_cal","VE_cal","r.Spearman")

# Full year, full water column, validation period (2019)
all_gof_val <- setNames(data.frame(matrix(ncol=9,nrow=21)),c("Parameter","Temp","Oxy","NIT_amm","NIT_nit","SRP","DOC","TN","TP"))
all_gof_val$Parameter <- c("ME_val","MAE_val","MSE_val","RMSE_val","NRMSE%_val","PBIAS%_val","RSR_val","rSD_val","NSE_val","mNSE_val","rNSE_val","d_val","md_val","rd_val","cp_val","r_val",
                              "R2_val","bR2_val","KGE_val","VE_val","r.Spearman")

#### TEMP ####
# 9 m Obs temp
obs_temp$Depth <- as.numeric(obs_temp$Depth)
obs_temp_cal <- obs_temp %>% 
  filter(DateTime < "2019-01-01")
obs_temp_val <- obs_temp %>% 
  filter(DateTime >= "2019-01-01")

# Full water column Model temp
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.2) 
mod_temp <- get_temp(baseline, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(temp_mod = temp)
mod_temp$Depth <- as.numeric(mod_temp$Depth)
mod_temp_cal <- mod_temp %>% 
  filter(DateTime < "2019-01-01")
mod_temp_val <- mod_temp %>% 
  filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later)
# For FULL year
comb_temp <- left_join(obs_temp,mod_temp,by=c("DateTime","Depth"))
all_gof$Temp <- gof(comb_temp$temp_mod,comb_temp$temp_obs,do.spearman = TRUE)

comb_temp_cal <- left_join(obs_temp_cal,mod_temp_cal,by=c("DateTime","Depth"))
all_gof_cal$Temp <- gof(comb_temp_cal$temp_mod,comb_temp_cal$temp_obs,do.spearman = TRUE)

comb_temp_val <- left_join(obs_temp_val,mod_temp_val,by=c("DateTime","Depth"))
all_gof_val$Temp <- gof(comb_temp_val$temp_mod,comb_temp_val$temp_obs,do.spearman = TRUE)

### OXY ###
# obs oxy
obs_oxy$Depth <- as.numeric(obs_oxy$Depth)
obs_oxy_cal <- obs_oxy %>% 
  filter(DateTime < "2019-01-01")
obs_oxy_val <- obs_oxy %>% 
  filter(DateTime >= "2019-01-01")

# modeled oxy
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.2) 
mod_oxy <- get_var(baseline,'OXY_oxy',reference='surface',z_out=depths) %>% 
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(oxy_mod = OXY_oxy)
mod_oxy$Depth <- as.numeric(mod_oxy$Depth)
mod_oxy_cal <- mod_oxy %>% 
  filter(DateTime < "2019-01-01")
mod_oxy_val <- mod_oxy %>% 
  filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_oxy <- left_join(obs_oxy,mod_oxy,by=c("DateTime","Depth"))
all_gof$Oxy <- gof(comb_oxy$oxy_mod,comb_oxy$oxy_obs,do.spearman = TRUE)

comb_oxy_cal <- left_join(obs_oxy_cal,mod_oxy_cal,by=c("DateTime","Depth"))
all_gof_cal$Oxy <- gof(comb_oxy_cal$oxy_mod,comb_oxy_cal$oxy_obs,do.spearman = TRUE)

comb_oxy_val <- left_join(obs_oxy_val,mod_oxy_val,by=c("DateTime","Depth"))
all_gof_val$Oxy <- gof(comb_oxy_val$oxy_mod,comb_oxy_val$oxy_obs,do.spearman = TRUE)

### NIT_amm ###
# Obs NIT_amm
obs_NIT_amm$Depth <- as.numeric(obs_NIT_amm$Depth)
obs_NIT_amm_cal <- obs_NIT_amm %>% 
  filter(DateTime < "2019-01-01")
obs_NIT_amm_val <- obs_NIT_amm %>% 
  filter(DateTime >= "2019-01-01")

### Mod NIT-amm ###
depths<- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9) 
mod_NIT_amm <- get_var(baseline,'NIT_amm',z_out=depths,reference='surface') %>% 
  pivot_longer(cols=starts_with("NIT_amm_"), names_to="Depth", names_prefix="NIT_amm_", values_to = "NIT_amm_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
mod_NIT_amm$Depth <- as.numeric(mod_NIT_amm$Depth)
mod_NIT_amm_cal <- mod_NIT_amm %>% 
  filter(DateTime < "2019-01-01")
mod_NIT_amm_val <- mod_NIT_amm %>% 
  filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_NIT_amm <- left_join(obs_NIT_amm,mod_NIT_amm,by=c("DateTime","Depth"))
all_gof$NIT_amm <- gof(comb_NIT_amm$NIT_amm_mod,comb_NIT_amm$NIT_amm_obs,do.spearman = TRUE)

comb_NIT_amm_cal <- left_join(obs_NIT_amm_cal,mod_NIT_amm_cal,by=c("DateTime","Depth"))
all_gof_cal$NIT_amm <- gof(comb_NIT_amm_cal$NIT_amm_mod,comb_NIT_amm_cal$NIT_amm_obs,do.spearman = TRUE)

comb_NIT_amm_val <- left_join(obs_NIT_amm_val,mod_NIT_amm_val,by=c("DateTime","Depth"))
all_gof_val$NIT_amm <- gof(comb_NIT_amm_val$NIT_amm_mod,comb_NIT_amm_val$NIT_amm_obs,do.spearman = TRUE)

### NIT_nit ###
# Obs NIT_nit
obs_NIT_nit$Depth <- as.numeric(obs_NIT_nit$Depth)
obs_NIT_nit_cal <- obs_NIT_nit %>% 
  filter(DateTime < "2019-01-01")
obs_NIT_nit_val <- obs_NIT_nit %>% 
  filter(DateTime >= "2019-01-01")

### Mod NIT-nit ###
depths<- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9) 
mod_NIT_nit <- get_var(baseline,'NIT_nit',z_out=depths,reference='surface') %>% 
  pivot_longer(cols=starts_with("NIT_nit_"), names_to="Depth", names_prefix="NIT_nit_", values_to = "NIT_nit_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
mod_NIT_nit$Depth <- as.numeric(mod_NIT_nit$Depth)
mod_NIT_nit_cal <- mod_NIT_nit %>% 
  filter(DateTime < "2019-01-01")
mod_NIT_nit_val <- mod_NIT_nit %>% 
  filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_NIT_nit <- left_join(obs_NIT_nit,mod_NIT_nit,by=c("DateTime","Depth"))
all_gof$NIT_nit <- gof(comb_NIT_nit$NIT_nit_mod,comb_NIT_nit$NIT_nit_obs,do.spearman = TRUE)

comb_NIT_nit_cal <- left_join(obs_NIT_nit_cal,mod_NIT_nit_cal,by=c("DateTime","Depth"))
all_gof_cal$NIT_nit <- gof(comb_NIT_nit_cal$NIT_nit_mod,comb_NIT_nit_cal$NIT_nit_obs,do.spearman = TRUE)

comb_NIT_nit_val <- left_join(obs_NIT_nit_val,mod_NIT_nit_val,by=c("DateTime","Depth"))
all_gof_val$NIT_nit <- gof(comb_NIT_nit_val$NIT_nit_mod,comb_NIT_nit_val$NIT_nit_obs,do.spearman = TRUE)

### SRP ###
obs_PHS_frp$Depth <- as.numeric(obs_PHS_frp$Depth)
obs_PHS_frp_cal <- obs_PHS_frp %>% 
  filter(DateTime < "2019-01-01")
obs_PHS_frp_val <- obs_PHS_frp %>% 
  filter(DateTime >= "2019-01-01")

# Mod SRP #
depths<- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)
mod_PHS_frp <- get_var(baseline,'PHS_frp',z_out=depths,reference='surface') %>% 
  pivot_longer(cols=starts_with("PHS_frp_"), names_to="Depth", names_prefix="PHS_frp_", values_to = "PHS_frp_mod") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
mod_PHS_frp$Depth <- as.numeric(mod_PHS_frp$Depth)
mod_PHS_frp_cal <- mod_PHS_frp %>% 
  filter(DateTime < "2019-01-01")
mod_PHS_frp_val <- mod_PHS_frp %>% 
  filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_PHS_frp <- left_join(obs_PHS_frp,mod_PHS_frp,by=c("DateTime","Depth"))
all_gof$SRP <- gof(comb_PHS_frp$PHS_frp_mod,comb_PHS_frp$PHS_frp_obs,do.spearman = TRUE)

comb_PHS_frp_cal <- left_join(obs_PHS_frp_cal,mod_PHS_frp_cal,by=c("DateTime","Depth"))
all_gof_cal$SRP <- gof(comb_PHS_frp_cal$PHS_frp_mod,comb_PHS_frp_cal$PHS_frp_obs,do.spearman = TRUE)

comb_PHS_frp_val <- left_join(obs_PHS_frp_val,mod_PHS_frp_val,by=c("DateTime","Depth"))
all_gof_val$SRP <- gof(comb_PHS_frp_val$PHS_frp_mod,comb_PHS_frp_val$PHS_frp_obs,do.spearman = TRUE)

### DOC + rDOC ####
# Obs #
obs_allDOC$Depth <- as.numeric(obs_allDOC$Depth)
obs_allDOC_cal <- obs_allDOC %>% 
  filter(DateTime < "2019-01-01")
obs_allDOC_val <- obs_allDOC %>% 
  filter(DateTime >= "2019-01-01")

# Mod #
# Summed DOC and rDOC following MakeFigure_ModeledvsObserved
depths<- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)
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
  filter(DateTime < "2019-01-01")
mod_allDOC_val <- mod_allDOC %>% 
  filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_allDOC <- left_join(obs_allDOC,mod_allDOC,by=c("DateTime","Depth"))
all_gof$DOC <- gof(comb_allDOC$allDOC_mod,comb_allDOC$allDOC_obs,do.spearman = TRUE)

comb_allDOC_cal <- left_join(obs_allDOC_cal,mod_allDOC_cal,by=c("DateTime","Depth"))
all_gof_cal$DOC <- gof(comb_allDOC_cal$allDOC_mod,comb_allDOC_cal$allDOC_obs,do.spearman = TRUE)

comb_allDOC_val <- left_join(obs_allDOC_val,mod_allDOC_val,by=c("DateTime","Depth"))
all_gof_val$DOC <- gof(comb_allDOC_val$allDOC_mod,comb_allDOC_val$allDOC_obs,do.spearman = TRUE)

### TOTALS ###
# Observed data
obs_Totals$Depth <- as.numeric(obs_Totals$Depth)
obs_Totals_cal <- obs_Totals %>% 
  filter(DateTime < "2019-01-01")
obs_Totals_val <- obs_Totals %>% 
  filter(DateTime >= "2019-01-01")

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

mod_totals<-as.data.frame(cbind(diatom[1:5],cyano[,3:5],green[,3:5],TN[,2],TP[,2],TOC[,2])) 
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
  filter(DateTime < "2019-01-01")
mod_totals2_val <- mod_totals2 %>% 
  filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
# FULL YEAR
comb_Totals <- left_join(obs_Totals,mod_totals2,by=c("DateTime","Depth"))
all_gof$TN <- gof(comb_Totals$TN_mod,comb_Totals$TN_obs,na.rm=TRUE,do.spearman = TRUE)
all_gof$TP <- gof(comb_Totals$TP_mod,comb_Totals$TP_obs,na.rm=TRUE,do.spearman = TRUE)

comb_Totals_cal <- left_join(obs_Totals_cal,mod_totals2_cal,by=c("DateTime","Depth"))
all_gof_cal$TN <- gof(comb_Totals_cal$TN_mod,comb_Totals_cal$TN_obs,na.rm=TRUE,do.spearman = TRUE)
all_gof_cal$TP <- gof(comb_Totals_cal$TP_mod,comb_Totals_cal$TP_obs,na.rm=TRUE,do.spearman = TRUE)

comb_Totals_val <- left_join(obs_Totals_val,mod_totals2_val,by=c("DateTime","Depth"))
all_gof_val$TN <- gof(comb_Totals_val$TN_mod,comb_Totals_val$TN_obs,na.rm=TRUE,do.spearman = TRUE)
all_gof_val$TP <- gof(comb_Totals_val$TP_mod,comb_Totals_val$TP_obs,na.rm=TRUE,do.spearman = TRUE)

## Add NMAE calculation for all parameters
# all_gof
all_gof[nrow(all_gof)+1,] <- NA
all_gof[22,1] <- "NMAE_all"
all_gof$Parameter[21] <- "r.Spearman_all"
all_gof$Temp[22] <- round(all_gof$Temp[2]/mean(comb_temp$temp_obs),digits = 2)
all_gof$Oxy[22] <- round(all_gof$Oxy[2]/mean(comb_oxy$oxy_obs),digits = 2)
all_gof$NIT_amm[22] <- round(all_gof$NIT_amm[2]/mean(comb_NIT_amm$NIT_amm_obs),digits = 2)
all_gof$NIT_nit[22] <- round(all_gof$NIT_nit[2]/mean(comb_NIT_nit$NIT_nit_obs,na.rm=TRUE),digits = 2)
all_gof$SRP[22] <- round(all_gof$SRP[2]/mean(comb_PHS_frp$PHS_frp_obs,na.rm=TRUE),digits = 2)
all_gof$DOC[22] <- round(all_gof$DOC[2]/mean(comb_allDOC$allDOC_obs),digits = 2)
all_gof$TN[22] <- round(all_gof$TN[2]/mean(comb_Totals$TN_obs,na.rm=TRUE),digits = 2)
all_gof$TP[22] <- round(all_gof$TP[2]/mean(comb_Totals$TP_obs,na.rm=TRUE),digits = 2)

# all_gof_cal
all_gof_cal[nrow(all_gof_cal)+1,] <- NA
all_gof_cal[22,1] <- "NMAE_cal"
all_gof_cal$Parameter[21] <- "r.Spearman_cal"
all_gof_cal$Temp[22] <- round(all_gof_cal$Temp[2]/mean(comb_temp_cal$temp_obs),digits = 2)
all_gof_cal$Oxy[22] <- round(all_gof_cal$Oxy[2]/mean(comb_oxy_cal$oxy_obs),digits = 2)
all_gof_cal$NIT_amm[22] <- round(all_gof_cal$NIT_amm[2]/mean(comb_NIT_amm_cal$NIT_amm_obs),digits = 2)
all_gof_cal$NIT_nit[22] <- round(all_gof_cal$NIT_nit[2]/mean(comb_NIT_nit_cal$NIT_nit_obs,na.rm=TRUE),digits = 2)
all_gof_cal$SRP[22] <- round(all_gof_cal$SRP[2]/mean(comb_PHS_frp_cal$PHS_frp_obs,na.rm=TRUE),digits = 2)
all_gof_cal$DOC[22] <- round(all_gof_cal$DOC[2]/mean(comb_allDOC_cal$allDOC_obs),digits = 2)
all_gof_cal$TN[22] <- round(all_gof_cal$TN[2]/mean(comb_Totals_cal$TN_obs,na.rm=TRUE),digits = 2)
all_gof_cal$TP[22] <- round(all_gof_cal$TP[2]/mean(comb_Totals_cal$TP_obs,na.rm=TRUE),digits = 2)

# all_gof_val
all_gof_val[nrow(all_gof_val)+1,] <- NA
all_gof_val[22,1] <- "NMAE_val"
all_gof_val$Parameter[21] <- "r.Spearman_val"
all_gof_val$Temp[22] <- round(all_gof_val$Temp[2]/mean(comb_temp_val$temp_obs),digits = 2)
all_gof_val$Oxy[22] <- round(all_gof_val$Oxy[2]/mean(comb_oxy_val$oxy_obs),digits = 2)
all_gof_val$NIT_amm[22] <- round(all_gof_val$NIT_amm[2]/mean(comb_NIT_amm_val$NIT_amm_obs),digits = 2)
all_gof_val$NIT_nit[22] <- round(all_gof_val$NIT_nit[2]/mean(comb_NIT_nit_val$NIT_nit_obs,na.rm=TRUE),digits = 2)
all_gof_val$SRP[22] <- round(all_gof_val$SRP[2]/mean(comb_PHS_frp_val$PHS_frp_obs,na.rm=TRUE),digits = 2)
all_gof_val$DOC[22] <- round(all_gof_val$DOC[2]/mean(comb_allDOC_val$allDOC_obs),digits = 2)
all_gof_val$TN[22] <- round(all_gof_val$TN[2]/mean(comb_Totals_val$TN_obs,na.rm=TRUE),digits = 2)
all_gof_val$TP[22] <- round(all_gof_val$TP[2]/mean(comb_Totals_val$TP_obs,na.rm=TRUE),digits = 2)

# Select GOF variables for the full year
full_n_all <- c("n_all",length(comb_temp$DateTime),
                length(comb_oxy$DateTime),
                length(comb_NIT_amm$DateTime),
                length(comb_NIT_nit$NIT_nit_obs[!is.na(comb_NIT_nit$NIT_nit_obs)]),
                length(comb_PHS_frp$PHS_frp_obs[!is.na(comb_PHS_frp$PHS_frp_obs)]),
                length(comb_allDOC$DateTime),
                length(comb_Totals$TN_obs[!is.na(comb_Totals$TN_obs)]),
                length(comb_Totals$TP_obs[!is.na(comb_Totals$TP_obs)]))

full_gof_all_table <- all_gof %>% 
  filter(Parameter == "r.Spearman_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NSE_all" | Parameter == "NMAE_all")

full_n_cal <- c("n_cal",length(comb_temp_cal$DateTime),
                length(comb_oxy_cal$DateTime),
                length(comb_NIT_amm_cal$DateTime),
                length(comb_NIT_nit_cal$NIT_nit_obs[!is.na(comb_NIT_nit_cal$NIT_nit_obs)]),
                length(comb_PHS_frp_cal$PHS_frp_obs[!is.na(comb_PHS_frp_cal$PHS_frp_obs)]),
                length(comb_allDOC_cal$DateTime),
                length(comb_Totals_cal$TN_obs[!is.na(comb_Totals_cal$TN_obs)]),
                length(comb_Totals_cal$TP_obs[!is.na(comb_Totals_cal$TP_obs)]))

full_gof_cal_table <- all_gof_cal %>% 
  filter(Parameter == "r.Spearman_cal" | Parameter == "RMSE_cal" | Parameter == "PBIAS%_cal" | Parameter == "NSE_cal" | Parameter == "NMAE_cal")

full_n_val <- c("n_val",length(comb_temp_val$DateTime),
                length(comb_oxy_val$DateTime),
                length(comb_NIT_amm_val$DateTime),
                length(comb_NIT_nit_val$NIT_nit_obs[!is.na(comb_NIT_nit_val$NIT_nit_obs)]),
                length(comb_PHS_frp_val$PHS_frp_obs[!is.na(comb_PHS_frp_val$PHS_frp_obs)]),
                length(comb_allDOC_val$DateTime),
                length(comb_Totals_val$TN_obs[!is.na(comb_Totals_val$TN_obs)]),
                length(comb_Totals_val$TP_obs[!is.na(comb_Totals_val$TP_obs)]))

full_gof_val_table <- all_gof_val %>% 
  filter(Parameter == "r.Spearman_val" | Parameter == "RMSE_val" | Parameter == "PBIAS%_val" | Parameter == "NSE_val" | Parameter == "NMAE_val")

full_gof_table <- rbind(full_n_all,full_gof_all_table,full_n_cal,full_gof_cal_table,full_n_val,full_gof_val_table)

write_csv(full_gof_table,'FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/table_gof_watercol_fullyear.csv')
