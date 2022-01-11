#*****************************************************************
#* TITLE:   FCR GLM-AED script to create figure 5            
#* AUTHORS:  C.C. Carey, R.P. McClure                                          
#* DATE:   Originally developed by CCC in summer 2020; updated by RPM
#*         to convert to ggplot in January 2021; Cayelan then modified again
#*         for the parameter sensitivity figure in January 2022                            
#* NOTES:  This script uses the data objects created by the 
#*         "MakeFigure_OrganizeDataForFigures.R" 
#*****************************************************************


# get the packages we will need for the plotting exercises
# install.packages('remotes')
# install.packages('devtools')
devtools::install_github("GLEON/GLMr", INSTALL_opts=c("--no-multiarch"))
remotes::install_github('CareyLabVT/glmtools', force = T, INSTALL_opts=c("--no-multiarch"))

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, dplyr, lubridate, reshape2, patchwork, ncdf4, glmtools, GLM3r, magrittr, ggpubr)

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

sss_oxy<-read.csv('FCR_2013_2019GLMHistoricalRun_GLMv3beta/inputs/HOx_Operations_20190916.csv') %>%
  rename(DateTime = time) %>%
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

DIN_oxic <- left_join(mod_nit_oxic,mod_amm_oxic, by = "DateTime")%>%
  mutate(DIN_oxic = NIT_nit + NIT_amm)%>%
  select(DateTime, DIN_oxic)

DIN_anoxic <- left_join(mod_nit_anoxic,mod_amm_anoxic, by = "DateTime")%>%
  mutate(DIN_anoxic = NIT_nit + NIT_amm)%>%
  select(DateTime, DIN_anoxic)

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


#####make the anoxic vs oxic dataset for the boxplot figures####
#pull out deep-water chemistry from each output file
A_oxy <- get_var(nc_anoxic,'OXY_oxy',z_out=9,reference = 'surface') 
O_oxy <- get_var(nc_oxic,'OXY_oxy',z_out=9,reference = 'surface') 

A_DOCr <- get_var(nc_anoxic, "OGM_docr",z_out=9,reference = 'surface')
A_DOC <- get_var(nc_anoxic, "OGM_doc",z_out=9,reference = 'surface')

O_DOCr <- get_var(nc_oxic, "OGM_docr",z_out=9,reference = 'surface')
O_DOC <- get_var(nc_oxic, "OGM_doc",z_out=9,reference = 'surface')

A_NO3 <- get_var(nc_anoxic, "NIT_nit",z_out=9,reference = 'surface')
O_NO3 <- get_var(nc_oxic, "NIT_nit",z_out=9,reference = 'surface')

A_NH4 <- get_var(nc_anoxic, "NIT_amm",z_out=9,reference = 'surface')
O_NH4 <- get_var(nc_oxic, "NIT_amm",z_out=9,reference = 'surface')

A_PO4 <- get_var(nc_anoxic, "PHS_frp",z_out=9,reference = 'surface')
O_PO4 <- get_var(nc_oxic, "PHS_frp",z_out=9,reference = 'surface')

A_TN <- get_var(nc_anoxic, "TOT_tn",z_out=9,reference = 'surface')
O_TN <- get_var(nc_oxic, "TOT_tn",z_out=9,reference = 'surface')

A_TP <- get_var(nc_anoxic, "TOT_tp",z_out=9,reference = 'surface')
O_TP <- get_var(nc_oxic, "TOT_tp",z_out=9,reference = 'surface')

A_TOC <- get_var(nc_anoxic, "TOT_toc",z_out=9,reference = 'surface')
O_TOC <- get_var(nc_oxic, "TOT_toc",z_out=9,reference = 'surface')

A_cyano <- get_var(nc_anoxic,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(A_cyanoN = CyanoConc*0.12,
         A_cyanoP = CyanoConc*0.0005,
         A_cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,A_cyanoN, A_cyanoP, A_cyanoC)

O_cyano <- get_var(nc_oxic,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(O_cyanoN = CyanoConc*0.12,
         O_cyanoP = CyanoConc*0.0005,
         O_cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,O_cyanoN, O_cyanoP, O_cyanoC)

A_green <- get_var(nc_anoxic,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(A_greenN = GreenConc*0.12,
         A_greenP = GreenConc*0.0005,
         A_greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,A_greenN, A_greenP, A_greenC)

O_green <- get_var(nc_oxic,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(O_greenN = GreenConc*0.12,
         O_greenP = GreenConc*0.0005,
         O_greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,O_greenN, O_greenP, O_greenC)

A_diatom <- get_var(nc_anoxic,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(A_diatomN = DiatomConc*0.12,
         A_diatomP = DiatomConc*0.0005,
         A_diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,A_diatomN, A_diatomP, A_diatomC)

O_diatom <- get_var(nc_oxic,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(O_diatomN = DiatomConc*0.12,
         O_diatomP = DiatomConc*0.0005,
         O_diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,O_diatomN, O_diatomP, O_diatomC)

#bind the data together
data<-as.data.frame(cbind(A_oxy,A_DOCr[,2],A_DOC[,2],A_NO3[,2],A_NH4[,2],A_PO4[,2],
                          A_TN[,2],A_TP[,2],A_TOC[,2],
                          A_diatom[,3:5],A_cyano[,3:5],A_green[,3:5],
                          O_oxy[,2],O_DOCr[,2],O_DOC[,2],O_NO3[,2],O_NH4[,2],O_PO4[,2],
                          O_TN[,2],O_TP[,2],O_TOC[,2],
                          O_diatom[,3:5],O_cyano[,3:5],O_green[,3:5])) 
colnames(data) = c("time", "A_oxy", "A_DOCr", "A_DOC", "A_NO3","A_NH4","A_PO4",
                   "A_TN", "A_TP", "A_TOC", "A_diatomN", "A_diatomP", "A_diatomC",
                   "A_cyanoN", "A_cyanoP", "A_cyanoC", "A_greenN", "A_greenP", "A_greenC",
                   "O_oxy","O_DOCr","O_DOC", "O_NO3","O_NH4","O_PO4",
                   "O_TN", "O_TP", "O_TOC", "O_diatomN", "O_diatomP", "O_diatomC",
                   "O_cyanoN", "O_cyanoP", "O_cyanoC", "O_greenN", "O_greenP", "O_greenC") 
data1 <- data %>% 
  mutate(A_DOCall = A_DOCr + A_DOC,
         O_DOCall = O_DOCr + O_DOC,
         A_totalN = A_TN + A_diatomN + A_cyanoN + A_greenN,
         O_totalN = O_TN + O_diatomN + O_cyanoN + O_greenN,
         A_totalP = A_TP + A_diatomP + A_cyanoP + A_greenP,
         O_totalP = O_TP + O_diatomP + O_cyanoP + O_greenP,
         A_totalC = A_TOC + A_diatomC + A_cyanoC + A_greenC,
         O_totalC = O_TOC + O_diatomC + O_cyanoC + O_greenC) %>% 
  select(time:A_TOC,O_oxy:O_TOC,A_DOCall:O_totalC) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST")))

#####make ratios####
data2 <- data1 %>% 
  mutate(A_DIN = A_NH4 + A_NO3,
         O_DIN = O_NH4 + O_NO3,
         A_TN_TP = A_TN/A_TP,
         O_TN_TP = O_TN/O_TP,
         A_DIN_PO4 = A_DIN/A_PO4,
         O_DIN_PO4 = O_DIN/O_PO4,
         A_TOC_TN = A_TOC/A_TN,
         O_TOC_TN = O_TOC/O_TN,
         A_DOC_DIN = A_DOCall/A_DIN,
         O_DOC_DIN = O_DOCall/O_DIN,
         A_TOC_TP = A_TOC/A_TP,
         O_TOC_TP = O_TOC/O_TP,
         A_DOC_PO4 = A_DOCall/A_PO4,
         O_DOC_PO4 = O_DOCall/O_PO4) %>% 
  select(time, A_oxy, O_oxy, A_TN_TP:O_DOC_PO4)

#only focus on summer stratified period of July 15-Oct 1
data4 <- data1 %>% 
  mutate(A_DIN = A_NH4 + A_NO3,
         O_DIN = O_NH4 + O_NO3,
         A_TN_TP = A_TN/A_TP,
         O_TN_TP = O_TN/O_TP,
         A_DIN_PO4 = A_DIN/A_PO4,
         O_DIN_PO4 = O_DIN/O_PO4,
         A_TOC_TN = A_TOC/A_TN,
         O_TOC_TN = O_TOC/O_TN,
         A_DOC_DIN = A_DOCall/A_DIN,
         O_DOC_DIN = O_DOCall/O_DIN,
         A_DOC_NH4 = A_DOC/A_NH4,
         O_DOC_NH4 = O_DOC/O_NH4,
         A_DOC_NO3 = A_DOC/A_NO3,
         O_DOC_NO3 = O_DOC/O_NO3,
         A_TOC_TP = A_TOC/A_TP,
         O_TOC_TP = O_TOC/O_TP,
         A_DOC_PO4 = A_DOCall/A_PO4,
         O_DOC_PO4 = O_DOCall/O_PO4,
         A_NO3_PO4 = A_NO3/A_PO4,
         O_NO3_PO4 = O_NO3/O_PO4,
         A_NH4_PO4 = A_NH4/A_PO4,
         O_NH4_PO4 = O_NH4/O_PO4) %>%  
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  filter(month_day <= "10-01", month_day >= "07-15") %>%
  select(-month_day)

mediandata <- data4 %>% 
  group_by(year) %>% 
  summarise(med_A_TN_TP = median(A_TN_TP),
            med_O_TN_TP = median(O_TN_TP),
            med_A_DIN_PO4 = median(A_DIN_PO4),
            med_O_DIN_PO4 = median(O_DIN_PO4),
            med_A_TOC_TN = median(A_TOC_TN),
            med_O_TOC_TN = median(O_TOC_TN),
            med_A_DOC_DIN = median(A_DOC_DIN),
            med_O_DOC_DIN = median(O_DOC_DIN),
            med_A_DOC_NH4 = median(A_DOC_NH4),
            med_O_DOC_NH4 = median(O_DOC_NH4),
            med_A_DOC_NO3 = median(A_DOC_NO3),
            med_O_DOC_NO3 = median(O_DOC_NO3),
            med_A_TOC_TP = median(A_TOC_TP),
            med_O_TOC_TP = median(O_TOC_TP),
            med_A_DOC_PO4 = median(A_DOC_PO4),
            med_O_DOC_PO4 = median(O_DOC_PO4),
            med_A_NO3_PO4 = median(A_NO3_PO4),
            med_O_NO3_PO4 = median(O_NO3_PO4),
            med_A_NH4_PO4 = median(A_NH4_PO4),
            med_O_NH4_PO4 = median(O_NH4_PO4),
            med_A_TOC = median(A_TOC),
            med_O_TOC = median(O_TOC),
            med_A_TN = median(A_TN),
            med_O_TN = median(O_TN),
            med_A_TP = median(A_TP),
            med_O_TP = median(O_TP),
            med_A_DIN = median(A_DIN),
            med_O_DIN = median(O_DIN),
            med_A_DOC = median(A_DOCall),
            med_O_DOC = median(O_DOCall),
            med_A_NH4 = median(A_NH4),
            med_O_NH4 = median(O_NH4),
            med_A_NO3 = median(A_NO3),
            med_O_NO3 = median(O_NO3),
            med_A_PO4 = median(A_PO4),
            med_O_PO4 = median(O_PO4))


toc_boxplot <- mediandata%>%
  select(year, med_A_TOC, med_O_TOC)%>%
  rename(Anoxic = med_A_TOC, Oxic = med_O_TOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic TOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "A")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 30, color = "black"),
        plot.title = element_text(face = "bold"))

doc_boxplot <- mediandata%>%
  select(year, med_A_DOC, med_O_DOC)%>%
  rename(Anoxic = med_A_DOC, Oxic = med_O_DOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "B")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

tn_boxplot <- mediandata%>%
  select(year, med_A_TN, med_O_TN)%>%
  rename(Anoxic = med_A_TN, Oxic = med_O_TN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic TN (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "C")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


din_boxplot <- mediandata%>%
  select(year, med_A_DIN, med_O_DIN)%>%
  rename(Anoxic = med_A_DIN, Oxic = med_O_DIN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DIN (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "D")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


nh4_boxplot <- mediandata%>%
  select(year, med_A_NH4, med_O_NH4)%>%
  rename(Anoxic = med_A_NH4, Oxic = med_O_NH4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic NH" [" 4"],""^"+"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "E")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


no3_boxplot <- mediandata%>%
  select(year, med_A_NO3, med_O_NO3)%>%
  rename(Anoxic = med_A_NO3, Oxic = med_O_NO3)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic NO" [" 3"],""^"-"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "F")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


tp_boxplot <- mediandata%>%
  select(year, med_A_TP, med_O_TP)%>%
  rename(Anoxic = med_A_TP, Oxic = med_O_TP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic TP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "G")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

drp_boxplot <- mediandata%>%
  select(year, med_A_PO4, med_O_PO4)%>%
  rename(Anoxic = med_A_PO4, Oxic = med_O_PO4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DRP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "H")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure5_StratifiedPeriodMedianHypoCNP_concetrations_v1.jpg", width = 20, height = 25, units = 'in', res = 1000)
figure4 <- (toc_boxplot|doc_boxplot)/(tn_boxplot|din_boxplot)/(nh4_boxplot|no3_boxplot)/(tp_boxplot|drp_boxplot)
figure4
dev.off()
