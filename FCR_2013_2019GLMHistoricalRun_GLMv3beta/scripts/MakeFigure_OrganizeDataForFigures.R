
### Develop plots for FCR-GLM MS ###
### Original plots from CCC --> Figure development by AGH, ASL, RPM, & WMW

### Last update 06Jan21

#This script is a workflow that organizes the original GLM run and the oxic
#and anoxic scenarios for the MS figure generation. 

# get the packages we will need for the plotting exercises
# install.packages('remotes')
# install.packages('devtools')
# remotes::install_github('usgs-r/glmtools')
# devtools::install_github("GLEON/GLMr")

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, dplyr, lubridate, reshape2, patchwork, ncdf4, glmtools, GLMr)

nc_main <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/output_2013_2019.nc')
nc_oxic <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/output_oxic.nc') 
nc_anoxic <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/output_anoxic.nc')

# Evaluate the modeled and observed data
# water temp
obstemp<-read_csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/CleanedObsTemp.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  filter(Depth==9)

modtemp <- get_temp(nc_main, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

modtemp_oxic <- get_temp(nc_oxic, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

modtemp_anoxic <- get_temp(nc_anoxic, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

#oxygen
obs_oxy<-read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/CleanedObsOxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  filter(Depth==9)

mod_oxy <- get_var(nc_main, "OXY_oxy", reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))   %>%
  mutate(OXYcum = cumsum(OXY_oxy))

mod_oxy_oxic <- get_var(nc_oxic, "OXY_oxy", reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))   %>%
  mutate(OXYcum = cumsum(OXY_oxy))

mod_oxy_anoxic <- get_var(nc_anoxic, "OXY_oxy", reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))   %>%
  mutate(OXYcum = cumsum(OXY_oxy))

sss_oxy<-read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/Calc_HOX_flow_DO_20190916.csv') %>%
  rename(DateTime = ï..time) %>%
  select(DateTime, mmol.O2.m3.day)%>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

#ammonium
var="NIT_amm"
field_file <- file.path('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv') 

obs_amm <-read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9)

mod_amm <- get_var(nc_main, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))   %>%
  mutate(AMMcum = cumsum(NIT_amm))

mod_amm_oxic <- get_var(nc_oxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))   %>%
  mutate(AMMcum = cumsum(NIT_amm))

mod_amm_anoxic <- get_var(nc_anoxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))   %>%
  mutate(AMMcum = cumsum(NIT_amm))

#nitrate
var="NIT_nit"

obs_nit <-read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9)

mod_nit <- get_var(nc_main, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))  %>%
  mutate(NITcum = cumsum(NIT_nit))

mod_nit_oxic <- get_var(nc_oxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(NITcum = cumsum(NIT_nit))

mod_nit_anoxic <- get_var(nc_anoxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(NITcum = cumsum(NIT_nit))

#SRP
var="PHS_frp"

obs_srp <-read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9)

mod_srp <- get_var(nc_main, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

mod_srp_oxic <- get_var(nc_oxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(SRPcum = cumsum(PHS_frp))

mod_srp_anoxic <- get_var(nc_anoxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(SRPcum = cumsum(PHS_frp))

# dissolved organic carbon pools
var="OGM_doc"

obs_doc <-read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9)

mod_doc <- get_var(nc_main, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

mod_doc_oxic <- get_var(nc_oxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

mod_doc_anoxic <- get_var(nc_anoxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

var="OGM_docr"

obs_docr <-read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  filter(Depth==9)

mod_docr <- get_var(nc_main, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

mod_docr_oxic <- get_var(nc_oxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

mod_docr_anoxic <- get_var(nc_anoxic, var, reference="surface", z_out=9) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

obsDOC <- merge(obs_doc, obs_docr, by="DateTime") %>% 
  mutate(DOCall = OGM_doc + OGM_docr)

modDOC <-merge(mod_doc,mod_docr, by="DateTime") %>% 
  mutate(DOCall = OGM_doc + OGM_docr)

modDOC_oxic <-merge(mod_doc_oxic,mod_docr_oxic, by="DateTime") %>% 
  mutate(DOCall = OGM_doc + OGM_docr) %>%
  mutate(DOCcum = cumsum(DOCall))

modDOC_anoxic <-merge(mod_doc_anoxic,mod_docr_anoxic, by="DateTime") %>% 
  mutate(DOCall = OGM_doc + OGM_docr) %>%
  mutate(DOCcum = cumsum(DOCall))

# total N, P, and C pools
TN <- get_var(nc_main, "TOT_tn",z_out=9,reference = 'surface')
TP <- get_var(nc_main, "TOT_tp",z_out=9,reference = 'surface')
TOC <- get_var(nc_main, "TOT_toc",z_out=9,reference = 'surface')

TN_oxic <- get_var(nc_oxic, "TOT_tn",z_out=9,reference = 'surface')
TP_oxic <- get_var(nc_oxic, "TOT_tp",z_out=9,reference = 'surface')
TOC_oxic <- get_var(nc_oxic, "TOT_toc",z_out=9,reference = 'surface')

TN_anoxic <- get_var(nc_anoxic, "TOT_tn",z_out=9,reference = 'surface')
TP_anoxic <- get_var(nc_anoxic, "TOT_tp",z_out=9,reference = 'surface')
TOC_anoxic <- get_var(nc_anoxic, "TOT_toc",z_out=9,reference = 'surface')

cyano <- get_var(nc_main,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoN = CyanoConc*0.12,
         cyanoP = CyanoConc*0.0005,
         cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoN, cyanoP, cyanoC)

cyano_oxic <- get_var(nc_oxic,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoN = CyanoConc*0.12,
         cyanoP = CyanoConc*0.0005,
         cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoN, cyanoP, cyanoC)

cyano_anoxic <- get_var(nc_anoxic,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoN = CyanoConc*0.12,
         cyanoP = CyanoConc*0.0005,
         cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoN, cyanoP, cyanoC)

green <- get_var(nc_main,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenN = GreenConc*0.12,
         greenP = GreenConc*0.0005,
         greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenN, greenP, greenC)

green_oxic <- get_var(nc_oxic,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenN = GreenConc*0.12,
         greenP = GreenConc*0.0005,
         greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenN, greenP, greenC)

green_anoxic <- get_var(nc_anoxic,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenN = GreenConc*0.12,
         greenP = GreenConc*0.0005,
         greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenN, greenP, greenC)

diatom <- get_var(nc_main,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomN = DiatomConc*0.12,
         diatomP = DiatomConc*0.0005,
         diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth, diatomN, diatomP, diatomC)

diatom_oxic <- get_var(nc_oxic,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomN = DiatomConc*0.12,
         diatomP = DiatomConc*0.0005,
         diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth, diatomN, diatomP, diatomC)

diatom_anoxic <- get_var(nc_anoxic,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomN = DiatomConc*0.12,
         diatomP = DiatomConc*0.0005,
         diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth, diatomN, diatomP, diatomC)

# compile total pools
#main data
data<-as.data.frame(cbind(diatom[1:5],cyano[,3:5],green[,3:5],TN[,2],TP[,2],TOC[,2])) 
colnames(data) = c("DateTime", "Depth", "diatomN", "diatomP", "diatomC",
                   "cyanoN", "cyanoP", "cyanoC", "greenN", "greenP", "greenC",
                   "TN", "TP", "TOC")

#oxic scenario data
data_oxic <-as.data.frame(cbind(diatom_oxic[1:5],cyano_oxic[,3:5],green_oxic[,3:5],TN_oxic[,2],TP_oxic[,2],TOC_oxic[,2])) 
colnames(data_oxic) = c("DateTime", "Depth", "diatomN", "diatomP", "diatomC",
                   "cyanoN", "cyanoP", "cyanoC", "greenN", "greenP", "greenC",
                   "TN", "TP", "TOC")

#anoxic scenario data
data_anoxic <-as.data.frame(cbind(diatom_anoxic[1:5],cyano_anoxic[,3:5],green_anoxic[,3:5],TN_anoxic[,2],TP_anoxic[,2],TOC_anoxic[,2])) 
colnames(data_anoxic) = c("DateTime", "Depth", "diatomN", "diatomP", "diatomC",
                        "cyanoN", "cyanoP", "cyanoC", "greenN", "greenP", "greenC",
                        "TN", "TP", "TOC")

mod_total_pools <- data %>% 
  mutate(totalN = TN + diatomN + cyanoN + greenN,
         totalP = TP + diatomP + cyanoP + greenP,
         totalC = TOC + diatomC + cyanoC + greenC,
         TNcum = cumsum(totalN),
         TPcum = cumsum(totalP),
         TCcum = cumsum(totalC)) %>% 
  select(DateTime, totalN:TCcum) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

mod_total_pools_oxic <- data_oxic %>% 
  mutate(totalN = TN + diatomN + cyanoN + greenN,
         totalP = TP + diatomP + cyanoP + greenP,
         totalC = TOC + diatomC + cyanoC + greenC,
         TNcum = cumsum(totalN),
         TPcum = cumsum(totalP),
         TCcum = cumsum(totalC)) %>% 
  select(DateTime, totalN:TCcum) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

mod_total_pools_anoxic <- data_anoxic %>% 
  mutate(totalN = TN + diatomN + cyanoN + greenN,
         totalP = TP + diatomP + cyanoP + greenP,
         totalC = TOC + diatomC + cyanoC + greenC,
         TNcum = cumsum(totalN),
         TPcum = cumsum(totalP),
         TCcum = cumsum(totalC)) %>% 
  select(DateTime, totalN:TCcum) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

obs_total_pools <-read_csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/totalNP.csv') %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  filter(Depth==9) %>% 
  na.omit()



