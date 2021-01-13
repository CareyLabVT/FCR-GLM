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

# Variables to include: temp, oxy, NIT_amm, SRP, DOC, TN, TP for 9 m ONLY
# Include: Calibration period (2013-2018); Validation period (2019); and All (2013-2019)

# Start with summer calculations ONLY (July 15 - Oct 1) - Following summer stratified period as defined in 
# MakeFigure_Boxplots.R

# Create empty dataframe for GOF results
# Using all GOF parameters for now - will cull eventually
summer_9m_gof <- setNames(data.frame(matrix(ncol=8,nrow=20)),c("Parameter","Temp","Oxy","NIT_amm","SRP","DOC","TN","TP"))
summer_9m_gof$Parameter <- c("ME","MAE","MSE","RMSE","NRMSE%","PBIAS%","RSR","rSD","NSE","mNSE","rNSE","d","md","rd","cp","r",
                             "R2","bR2","KGE","VE")
### 9m Summer TEMP ###
# 9m Model temp
temp_9m <- get_temp(baseline,z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST"))) %>% 
  rename(temp_mod = temp_9)
temp_9m_summer <- temp_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# 9 m Obs temp
obs_temp <- read_csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/CleanedObsTemp.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(temp_obs = temp)
obs_temp_9m <- obs_temp %>% 
  filter(Depth==9)
obs_temp_9m_summer <- obs_temp_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later!)
comb_9m_temp <- left_join(obs_temp_9m_summer,temp_9m_summer,by="DateTime") %>% 
  select(-c("Month.x","Month.y"))

summer_9m_gof$Temp <- gof(comb_9m_temp$temp_mod,comb_9m_temp$temp_obs)

### 9m summer OXY ###
# 9m modeled oxy
oxy_9m <- get_var(baseline,'OXY_oxy',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
oxy_9m_summer <- oxy_9m %>% 
  mutate(Month = format(DateTime,"%m-%d")) %>% 
  filter(Month >= "07-15" & Month <= "10-01")

# 9m obs oxy


############################### STOPPED HERE #######################################

NIT_amm_9m <- get_var(baseline,'NIT_amm',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
NIT_nit_9m <- get_var(baseline,'NIT_nit',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
PHS_frp_9m <- get_var(baseline,'PHS_frp',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
OGM_docr_9m <- get_var(baseline,'OGM_docr',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))
OGM_doc_9m <- get_var(baseline,'OGM_doc',z_out=9,reference='surface') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,'%Y-%m-%d',tz="EST")))

# Model total parameters (following MakeFigures_ModeledvsObserved.R)
TN_9m <- get_var(nc_file, "TOT_tn",z_out=9,reference = 'surface')
TP_9m <- get_var(nc_file, "TOT_tp",z_out=9,reference = 'surface')
TOC_9m <- get_var(nc_file, "TOT_toc",z_out=9,reference = 'surface')

cyano_9m <- get_var(nc_file,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoN = CyanoConc*0.12,
         cyanoP = CyanoConc*0.0005,
         cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoN, cyanoP, cyanoC)

green_9m <- get_var(nc_file,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenN = GreenConc*0.12,
         greenP = GreenConc*0.0005,
         greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenN, greenP, greenC)

diatom_9m <- get_var(nc_file,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomN = DiatomConc*0.12,
         diatomP = DiatomConc*0.0005,
         diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth, diatomN, diatomP, diatomC)

data_9m<-as.data.frame(cbind(diatom_9m[1:5],cyano_9m[,3:5],green_9m[,3:5],TN_9m[,2],TP_9m[,2],TOC_9m[,2])) 
colnames(data_9m) = c("DateTime", "Depth", "diatomN", "diatomP", "diatomC",
                   "cyanoN", "cyanoP", "cyanoC", "greenN", "greenP", "greenC",
                   "TN", "TP", "TOC") 
data1_9m <- data_9m %>% 
  mutate(totalN = TN + diatomN + cyanoN + greenN,
         totalP = TP + diatomP + cyanoP + greenP,
         totalC = TOC + diatomC + cyanoC + greenC) %>% 
  select(DateTime, totalN:totalC) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

