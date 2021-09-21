#*****************************************************************
#* TITLE:   FCR GLM-AED script to calculate numbers needed within
#*          the manuscript                     
#* AUTHORS:  C.C. Carey                                          
#* DATE:   Originally developed 16 July 2020; Last modified 13 Sept 2021                            
#* NOTES:  Goal of this script is to provide the ancillary data needed
#*         to write the manuscript. Comments below refer to the section
#*         where different results are reported.
#*****************************************************************

#load packages
library(zoo)
library(tidyverse)
library(lubridate)
library(glmtools)
library(dplyr)
library(pracma)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
#setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#set GLM output files for analysis & figures

#baseline scenario, based on observed SSS practices
nc_file <- file.path(sim_folder, 'output/output_2013_2019.nc')

#anoxic scenario, no SSS activation throughout
anoxic <- file.path(sim_folder, 'output/output_anoxic.nc')

#oxic scenario, SSS on in summer May 15-Nov 15 at full level
oxic <- file.path(sim_folder, 'output/output_oxic.nc')

#observed
observed <- read.csv("./field_data/field_chem_hypo.csv", header=T) %>% 
  mutate(time = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))
totals <- read.csv("./field_data/totalNP.csv", header=T) %>% 
  mutate(time = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

###################################################################
####Abstract####
#first run the "MakeFigure_OrganizeDataForFigure" script, then use this to calculate difference in 
# TN export
tn_export <- retention_stratified_period%>%
  select(year, Fnet_A_TN, Fnet_O_TN)%>%
  rename(Anoxic = Fnet_A_TN, Oxic = Fnet_O_TN) %>% 
  mutate(AnoxicFactor = Anoxic/Oxic)
mean(tn_export$AnoxicFactor)
#for text in the abstract regarding the factor of anoxic vs oxic TN export
###################################################################

###################################################################
####Methods####
#calculate hydraulic residence time
outflow <- read_csv("./inputs/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv") %>% 
  mutate(HRT = 308836/(FLOW*60*60*24)) #HRT = full pond volume in m3 divided by the rate of water leaving in m3/d
mean(outflow$HRT) #281 days
sd(outflow$HRT)/(length(outflow$FLOW)^0.5) #12 days for SE
#for site description paragraph of FCR's hydraulic residence time
###################################################################

###################################################################
####Results####
###############
##calculating the summer anoxic vs oxic observational differences
#did not end up making it into the manuscript, but leaving here for posterity
# data <- merge(observed, totals, by=c('time', "Depth"), all.x=T, all.y=T) %>% 
#   filter(Depth==9) %>% 
#   mutate(month_day = format(as.Date(time), "%m-%d")) %>%
#   mutate(year = year(time))%>%
#   dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
#   mutate(DOC = OGM_doc + OGM_docr) %>% 
#   select(time, year, Depth, NIT_amm:PHS_frp,DOC, TOT_tn:TOT_tp) %>% 
#   group_by(year) %>% drop_na() %>% 
#   summarise(mean_NH4 = mean(NIT_amm),
#             mean_NIT = mean(NIT_nit),
#             mean_FRP = mean(PHS_frp),
#             mean_DOC = mean(DOC),
#             mean_TN = mean(TOT_tn),
#             mean_TP = mean(TOT_tp),
#             max_NH4 = max(NIT_amm),
#             max_NIT = max(NIT_nit),
#             max_FRP = max(PHS_frp),
#             max_DOC = max(DOC),
#             max_TN = max(TOT_tn),
#             max_TP = max(TOT_tp),
#             min_NIT = min(NIT_nit)) %>%
#   mutate(Status = case_when(
#     year == 2014 | year == 2018 | year == 2019 ~ "Anoxic",
#     year == 2015 | year == 2016 | year == 2017 ~ "Oxic")) %>% 
#   group_by(Status) %>% 
#   summarise(mean_annual_NH4 = mean(mean_NH4),
#             mean_annual_NIT = mean(mean_NIT),
#             mean_annual_FRP = mean(mean_FRP),
#             mean_annual_DOC = mean(mean_DOC),
#             mean_annual_TN = mean(mean_TN),
#             mean_annual_TP = mean(mean_TP),
#             max_annual_NH4 = mean(max_NH4),
#             max_annual_NIT = mean(max_NIT),
#             max_annual_FRP = mean(max_FRP),
#             max_annual_DOC = mean(max_DOC),
#             max_annual_TN = mean(max_TN),
#             max_annual_TP = mean(max_TP))
# 
# (data$max_annual_NH4[1]-data$max_annual_NH4[2])/data$max_annual_NH4[2]
# (data$max_annual_FRP[1]-data$max_annual_FRP[2])/data$max_annual_FRP[2]
# (data$max_annual_DOC[1]-data$max_annual_DOC[2])/data$max_annual_DOC[2]
# (data$max_annual_NIT[1]-data$max_annual_NIT[2])/data$max_annual_NIT[2]
# (data$max_annual_TN[1]-data$max_annual_TN[2])/data$max_annual_TN[2]
# (data$max_annual_TP[1]-data$max_annual_TP[2])/data$max_annual_TP[2]
#for second results paragraph that needs numbers on % difference in observed data 
# between oxygenated and non-oxygenated summers


# data <- merge(observed, totals, by=c('time', "Depth"), all.x=T, all.y=T) %>% 
#   filter(Depth==9) %>% 
#   mutate(month_day = format(as.Date(time), "%m-%d")) %>%
#   mutate(year = year(time))%>%
#   dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
#   mutate(DOC = OGM_doc + OGM_docr) %>% 
#   select(time, year, Depth, NIT_amm:PHS_frp,DOC, TOT_tn:TOT_tp) %>% 
#   group_by(year) %>% 
#   drop_na() %>% 
#   mutate(DIN = NIT_amm + NIT_nit,
#          NIT_DIN = NIT_amm/DIN,
#          TN_TP = TOT_tn/TOT_tp,
#          DIN_FRP = DIN/PHS_frp,
#          DOC_DIN = DOC/DIN,
#          DOC_NH4 = DOC/NIT_amm,
#          DOC_NO3 = DOC/NIT_nit,
#          DOC_FRP = DOC/PHS_frp) %>% 
#  # print(mean(data$NIT_DIN)) %>% 
#  # print(sd(data$NIT_DIN))
#   group_by(year) %>% 
#   summarise(mean_NIT_DIN = mean(NIT_DIN),
#             mean_TN_TP = mean(TN_TP),
#             mean_DIN_FRP = mean(DIN_FRP),
#             mean_DOC_DIN = mean(DOC_DIN),
#             mean_DOC_NH4 = mean(DOC_NH4),
#             mean_DOC_NO3 = mean(DOC_NO3),
#             mean_DOC_FRP = mean(DOC_FRP),
#             max_NIT_DIN = max(NIT_DIN),
#             max_TN_TP = max(TN_TP),
#             max_DIN_FRP = max(DIN_FRP),
#             max_DOC_DIN = max(DOC_DIN),
#             max_DOC_NH4 = max(DOC_NH4),
#             max_DOC_NO3 = max(DOC_NO3),
#             max_DOC_FRP = max(DOC_FRP)) %>% drop_na() %>% 
#   mutate(Status = case_when(
#            year == 2014 | year == 2018 | year == 2019 ~ "Anoxic",
#            year == 2015 | year == 2016 | year == 2017 ~ "Oxic")) %>% 
#   group_by(Status) %>% 
#   summarise(mean_annual_NIT_DIN = mean(mean_NIT_DIN),
#             mean_annual_TN_TP = mean(mean_TN_TP),
#             mean_annual_DIN_FRP = mean(mean_DIN_FRP),
#             mean_annual_DOC_DIN = mean(mean_DOC_DIN),
#             mean_annual_DOC_NH4 = mean(mean_DOC_NH4),
#             mean_annual_DOC_NO3 = mean(mean_DOC_NO3),
#             mean_annual_DOC_FRP = mean(mean_DOC_FRP),
#             max_annual_NIT_DIN = mean(max_NIT_DIN),
#             max_annual_TN_TP = mean(max_TN_TP),
#             max_annual_DIN_FRP = mean(max_DIN_FRP),
#             max_annual_DOC_DIN = mean(max_DOC_DIN),
#             max_annual_DOC_NH4 = mean(max_DOC_NH4),
#             max_annual_DOC_NO3 = mean(max_DOC_NO3),
#             max_annual_DOC_FRP = mean(max_DOC_FRP))

###################################################################
#####study the baseline simulation####
# #pull out deep-water chemistry from each output file
# B_oxy <- get_var(nc_file,'OXY_oxy',z_out=9,reference = 'surface') 
# B_DOCr <- get_var(nc_file, "OGM_docr",z_out=9,reference = 'surface')
# B_DOC <- get_var(nc_file, "OGM_doc",z_out=9,reference = 'surface')
# B_NO3 <- get_var(nc_file, "NIT_nit",z_out=9,reference = 'surface')
# B_NH4 <- get_var(nc_file, "NIT_amm",z_out=9,reference = 'surface')
# B_PO4 <- get_var(nc_file, "PHS_frp",z_out=9,reference = 'surface')
# B_TN <- get_var(nc_file, "TOT_tn",z_out=9,reference = 'surface')
# B_TP <- get_var(nc_file, "TOT_tp",z_out=9,reference = 'surface')
# B_TOC <- get_var(nc_file, "TOT_toc",z_out=9,reference = 'surface')
# 
# B_cyano <- get_var(nc_file,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
#   pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
#   mutate(B_cyanoN = CyanoConc*0.12,
#          B_cyanoP = CyanoConc*0.0005,
#          B_cyanoC = CyanoConc) %>% 
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
#   select(DateTime,Depth,B_cyanoN, B_cyanoP, B_cyanoC)
# 
# B_green <- get_var(nc_file,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
#   pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
#   mutate(B_greenN = GreenConc*0.12,
#          B_greenP = GreenConc*0.0005,
#          B_greenC = GreenConc) %>% 
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
#   select(DateTime,Depth,B_greenN, B_greenP, B_greenC)
# 
# B_diatom <- get_var(nc_file,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
#   pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
#   mutate(B_diatomN = DiatomConc*0.12,
#          B_diatomP = DiatomConc*0.0005,
#          B_diatomC = DiatomConc) %>% 
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
#   select(DateTime,Depth,B_diatomN, B_diatomP, B_diatomC)
# 
# #bind the data together
# data<-as.data.frame(cbind(B_oxy,B_DOCr[,2],B_DOC[,2],B_NO3[,2],B_NH4[,2],B_PO4[,2],
#                           B_TN[,2],B_TP[,2],B_TOC[,2],
#                           B_diatom[,3:5],B_cyano[,3:5],B_green[,3:5]))
# colnames(data) = c("time", "B_oxy", "B_DOCr", "B_DOC", "B_NO3","B_NH4","B_PO4",
#                    "B_TN", "B_TP", "B_TOC", "B_diatomN", "B_diatomP", "B_diatomC",
#                    "B_cyanoN", "B_cyanoP", "B_cyanoC", "B_greenN", "B_greenP", "B_greenC") 
# data1 <- data %>% 
#   mutate(B_DOCall = B_DOCr + B_DOC,
#          B_totalN = B_TN + B_diatomN + B_cyanoN + B_greenN,
#          B_totalP = B_TP + B_diatomP + B_cyanoP + B_greenP,
#          B_totalC = B_TOC + B_diatomC + B_cyanoC + B_greenC) %>% 
#   select(time:B_TOC,B_oxy:B_TOC,B_DOCall:B_totalC) %>% 
#   mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
#   mutate(month_day = format(as.Date(time), "%m-%d")) %>%
#   mutate(year = year(time)) %>%
#   dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
#   select(time, year, B_NO3:B_PO4,B_DOCall:B_totalC) %>% 
#   group_by(year) %>% drop_na() %>% 
#   summarise(mean_NH4 = mean(B_NH4),
#             mean_NIT = mean(B_NO3),
#             mean_FRP = mean(B_PO4),
#             mean_DOC = mean(B_DOCall),
#             mean_TN = mean(B_totalN),
#             mean_TP = mean(B_totalP),
#             max_NH4 = max(B_NH4),
#             max_NIT = max(B_NO3),
#             max_FRP = max(B_PO4),
#             max_DOC = max(B_DOCall),
#             max_TN = max(B_totalN),
#             max_TP = max(B_totalP)) %>%
#   mutate(Status = case_when(
#     year == 2014 | year == 2018 | year == 2019 ~ "Anoxic",
#     year == 2015 | year == 2016 | year == 2017 ~ "Oxic",
#     year == 2013 ~ "TheInBetween")) %>% 
#   group_by(Status) %>% 
#   summarise(mean_annual_NH4 = mean(mean_NH4),
#             mean_annual_NIT = mean(mean_NIT),
#             mean_annual_FRP = mean(mean_FRP),
#             mean_annual_DOC = mean(mean_DOC),
#             mean_annual_TN = mean(mean_TN),
#             mean_annual_TP = mean(mean_TP),
#             max_annual_NH4 = mean(max_NH4),
#             max_annual_NIT = mean(max_NIT),
#             max_annual_FRP = mean(max_FRP),
#             max_annual_DOC = mean(max_DOC),
#             max_annual_TN = mean(max_TN),
#             max_annual_TP = mean(max_TP))
# 
# (data1$max_annual_NH4[1]-data1$max_annual_NH4[2])/data1$max_annual_NH4[2]
# (data1$max_annual_FRP[1]-data1$max_annual_FRP[2])/data1$max_annual_FRP[2]
# (data1$max_annual_DOC[1]-data1$max_annual_DOC[2])/data1$max_annual_DOC[2]
# (data1$max_annual_NIT[1]-data1$max_annual_NIT[2])/data1$max_annual_NIT[2]
# (data1$max_annual_TN[1]-data1$max_annual_TN[2])/data1$max_annual_TN[2]
# (data1$max_annual_TP[1]-data1$max_annual_TP[2])/data1$max_annual_TP[2]
##for third paragraph looking at the summers with continuous vs limited oxygenation in baseline sim
###################################################################

###################################################################
#####make the anoxic vs oxic modeled dataset####
#pull out deep-water chemistry from each output file
A_oxy <- get_var(anoxic,'OXY_oxy',z_out=9,reference = 'surface') 
O_oxy <- get_var(oxic,'OXY_oxy',z_out=9,reference = 'surface') 

A_DOCr <- get_var(anoxic, "OGM_docr",z_out=9,reference = 'surface')
A_DOC <- get_var(anoxic, "OGM_doc",z_out=9,reference = 'surface')

O_DOCr <- get_var(oxic, "OGM_docr",z_out=9,reference = 'surface')
O_DOC <- get_var(oxic, "OGM_doc",z_out=9,reference = 'surface')

A_NO3 <- get_var(anoxic, "NIT_nit",z_out=9,reference = 'surface')
O_NO3 <- get_var(oxic, "NIT_nit",z_out=9,reference = 'surface')

A_NH4 <- get_var(anoxic, "NIT_amm",z_out=9,reference = 'surface')
O_NH4 <- get_var(oxic, "NIT_amm",z_out=9,reference = 'surface')

A_PO4 <- get_var(anoxic, "PHS_frp",z_out=9,reference = 'surface')
O_PO4 <- get_var(oxic, "PHS_frp",z_out=9,reference = 'surface')

A_TN <- get_var(anoxic, "TOT_tn",z_out=9,reference = 'surface')
O_TN <- get_var(oxic, "TOT_tn",z_out=9,reference = 'surface')

A_TP <- get_var(anoxic, "TOT_tp",z_out=9,reference = 'surface')
O_TP <- get_var(oxic, "TOT_tp",z_out=9,reference = 'surface')

A_TOC <- get_var(anoxic, "TOT_toc",z_out=9,reference = 'surface')
O_TOC <- get_var(oxic, "TOT_toc",z_out=9,reference = 'surface')

A_cyano <- get_var(anoxic,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(A_cyanoN = CyanoConc*0.12,
         A_cyanoP = CyanoConc*0.0005,
         A_cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,A_cyanoN, A_cyanoP, A_cyanoC)

O_cyano <- get_var(oxic,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(O_cyanoN = CyanoConc*0.12,
         O_cyanoP = CyanoConc*0.0005,
         O_cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,O_cyanoN, O_cyanoP, O_cyanoC)

A_green <- get_var(anoxic,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(A_greenN = GreenConc*0.12,
         A_greenP = GreenConc*0.0005,
         A_greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,A_greenN, A_greenP, A_greenC)

O_green <- get_var(oxic,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(O_greenN = GreenConc*0.12,
         O_greenP = GreenConc*0.0005,
         O_greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,O_greenN, O_greenP, O_greenC)

A_diatom <- get_var(anoxic,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(A_diatomN = DiatomConc*0.12,
         A_diatomP = DiatomConc*0.0005,
         A_diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,A_diatomN, A_diatomP, A_diatomC)

O_diatom <- get_var(oxic,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
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
data2 <- data %>% 
  mutate(A_DOCall = A_DOCr + A_DOC,
         O_DOCall = O_DOCr + O_DOC,
         A_totalN = A_TN + A_diatomN + A_cyanoN + A_greenN,
         O_totalN = O_TN + O_diatomN + O_cyanoN + O_greenN,
         A_totalP = A_TP + A_diatomP + A_cyanoP + A_greenP,
         O_totalP = O_TP + O_diatomP + O_cyanoP + O_greenP,
         A_totalC = A_TOC + A_diatomC + A_cyanoC + A_greenC,
         O_totalC = O_TOC + O_diatomC + O_cyanoC + O_greenC) %>% 
  select(time,A_oxy, A_NO3:A_PO4, A_DOCall, A_totalN, A_totalP, A_totalC, O_oxy, 
         O_NO3:O_PO4, O_DOCall, O_totalN, O_totalP,O_totalC) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST")))
###################################################################

###################################################################
#####run t-tests to compare anoxic vs. oxic variables####
#to calculate t-tests of the median hypolimnetic concentrations for SI
median_yr_concs <- data2 %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
  group_by(year) %>% 
  summarise(median_A_TOC = median(A_totalC),
            median_O_TOC = median(O_totalC),
            median_A_DOC = median(A_DOCall),
            median_O_DOC = median(O_DOCall),
            median_A_TN = median(A_totalN),
            median_O_TN = median(O_totalN),
            median_A_NH4 = median(A_NH4),
            median_O_NH4 = median(O_NH4),
            median_A_NO3 = median(A_NO3),
            median_O_NO3 = median(O_NO3),
            median_A_TP = median(A_totalP),
            median_O_TP = median(O_totalP),
            median_A_DRP = median(A_PO4),
            median_O_DRP = median(O_PO4))

#develop a matrix to fill with t-test statistics
variables <-c("TOC", "DOC", "TN", "NH4", "NO3", "TP", "DRP")
conc_stats <- matrix(-99,length(variables), 4) 

for(i in 1:length(variables)){
  temp<-as.data.frame(median_yr_concs[grep(variables[i],colnames(median_yr_concs))])
  test<-t.test(temp[,1],temp[,2], paired=T)
  conc_stats[i,1]=variables[i]
  conc_stats[i,2]=round(as.numeric(test[1]), digits=2)
  conc_stats[i,3]=as.numeric(test[2])
  conc_stats[i,4]=as.numeric(test[3])
}

conc_stats1<-noquote(as.data.frame(conc_stats))
colnames(conc_stats1)<-c("Variable", "t-value", "df", "p-value")
write.csv(conc_stats1, "output/Hypo_Concs_TtestStats.csv", row.names = F)
#SI Text 2, Table S1
###################################################################

###################################################################
#to calculate t-tests of the median hypolimnetic molar ratios for SI
median_yr_ratios <- data2 %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
  mutate(A_TN = A_totalN,
         A_TP = A_totalP,
         A_TOC = A_totalC,
         O_TN = O_totalN,
         O_TP = O_totalP,
         O_TOC = O_totalC) %>% 
  mutate(A_DIN = A_NH4 + A_NO3,
         O_DIN = O_NH4 + O_NO3,
         A_NH4_DIN = A_NH4/A_DIN,
         O_NH4_DIN = O_NH4/O_DIN,
         A_TN_TP = A_TN/A_TP,
         O_TN_TP = O_TN/O_TP,
         A_DIN_PO4 = A_DIN/A_PO4,
         O_DIN_PO4 = O_DIN/O_PO4,
         A_TOC_TN = A_TOC/A_TN,
         O_TOC_TN = O_TOC/O_TN,
         A_DOC_DIN = A_DOCall/A_DIN,
         O_DOC_DIN = O_DOCall/O_DIN,
         A_DOC_NO3 = A_DOCall/A_NO3,
         O_DOC_NO3 = O_DOCall/O_NO3,
         A_DOC_NH4 = A_DOCall/A_NH4,
         O_DOC_NH4 = O_DOCall/O_NH4,
         A_TOC_TP = A_TOC/A_TP,
         O_TOC_TP = O_TOC/O_TP,
         A_DOC_PO4 = A_DOCall/A_PO4,
         O_DOC_PO4 = O_DOCall/O_PO4) %>% 
  select(time, year, A_NH4_DIN:O_DOC_PO4) %>% 
  group_by(year) %>% 
  summarise(median_A_TOC_TN = median(A_TOC_TN),
            median_O_TOC_TN = median(O_TOC_TN),
            median_A_TOC_TP = median(A_TOC_TP),
            median_O_TOC_TP = median(O_TOC_TP),
            median_A_DOC_DIN = median(A_DOC_DIN),
            median_O_DOC_DIN = median(O_DOC_DIN),
            median_A_DOC_NH4 = median(A_DOC_NH4),
            median_O_DOC_NH4 = median(O_DOC_NH4),
            median_A_DOC_NO3 = median(A_DOC_NO3),
            median_O_DOC_NO3 = median(O_DOC_NO3),
            median_A_DOC_DRP = median(A_DOC_PO4),
            median_O_DOC_DRP = median(O_DOC_PO4),
            median_A_TN_TP = median(A_TN_TP),
            median_O_TN_TP = median(O_TN_TP),
            median_A_DIN_DRP = median(A_DIN_PO4),
            median_O_DIN_DRP = median(O_DIN_PO4))
           
#develop a matrix to fill with t-test statistics
variables <-c("TOC_TN", "TOC_TP", "DOC_DIN", "DOC_NH4", "DOC_DRP", "TN_TP", "DIN_DRP", "DOC_NO3")
conc_stats <- matrix(-99,length(variables), 4) 

for(i in 1:length(variables)){
  temp<-as.data.frame(median_yr_ratios[grep(variables[i],colnames(median_yr_ratios))])
  test<-t.test(temp[,1],temp[,2], paired=T)
  conc_stats[i,1]=variables[i]
  conc_stats[i,2]=round(as.numeric(test[1]), digits=2)
  conc_stats[i,3]=as.numeric(test[2])
  conc_stats[i,4]=as.numeric(test[3])
}
#get an error here, because DOC_NO3 is Inf

conc_stats1<-noquote(as.data.frame(conc_stats))
colnames(conc_stats1)<-c("Variable", "t-value", "df", "p-value")
write.csv(conc_stats1, "output/Hypo_Ratios_TtestStats.csv", row.names = F)
#SI Text 2, Table S2 (just remember that DOC:NO3 is Inf)
###################################################################

###################################################################
##to calculate t-tests of the median retention rates for the SI
weir <- read.csv("inputs/FCR_weir_inflow_2013_2019_20200828_allfractions_2poolsDOC.csv") %>% 
  mutate(TP = FLOW*(PHS_frp + OGM_dop + OGM_dopr + OGM_pop),
         TN = FLOW*(NIT_amm + NIT_nit + OGM_don + OGM_donr + OGM_pon),
         TOC = FLOW*(OGM_doc + OGM_docr + OGM_poc),
         DIN = FLOW*(NIT_amm + NIT_nit),
         NO3 = FLOW*NIT_nit,
         NH4 = FLOW*NIT_amm,
         FRP = FLOW*PHS_frp,
         DOC = FLOW*(OGM_doc + OGM_docr)) %>% 
  select(time,TP:DOC)

wetland <- read.csv("inputs/FCR_wetland_inflow_2013_2019_20200828_allfractions_2DOCpools.csv") %>% 
  mutate(TP = FLOW*(PHS_frp + OGM_dop + OGM_dopr + OGM_pop),
         TN = FLOW*(NIT_amm + NIT_nit + OGM_don + OGM_donr + OGM_pon),
         TOC = FLOW*(OGM_doc + OGM_docr + OGM_poc),
         DIN = FLOW*(NIT_amm + NIT_nit),
         NO3 = FLOW*NIT_nit,
         NH4 = FLOW*NIT_amm,
         FRP = FLOW*PHS_frp,
         DOC = FLOW*(OGM_doc + OGM_docr)) %>% 
  select(time,TP:DOC)

inputs<- merge(weir, wetland, by="time") %>% 
  mutate(TP = TP.x + TP.y,
         TN = TN.x + TN.y,
         TOC = TOC.x + TOC.y,
         DOC = DOC.x + DOC.y,
         DIN = DIN.x + DIN.y,
         FRP = FRP.x + FRP.y,
         NO3 = NO3.x + NO3.y,
         NH4 = NH4.x + NH4.y) %>% 
  select(time,TP:NH4) %>% 
  rename(inputTP = TP,
         inputTN = TN,
         inputTOC = TOC,
         inputDOC = DOC,
         inputDIN = DIN,
         inputFRP = FRP,
         inputNO3 = NO3,
         inputNH4 = NH4) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST")))

outflow <- read.csv('inputs/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv') %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST")))

outputs <- merge(outflow,data2, by="time") %>% 
  select(time, FLOW, A_oxy,A_NO3:A_PO4,O_oxy,O_NO3:O_PO4,A_DOCall:O_totalC) %>% 
  mutate(A_FRP_output = FLOW*A_PO4,
         O_FRP_output = FLOW*O_PO4,
         A_NO3_output = FLOW*A_NO3,
         O_NO3_output = FLOW*O_NO3,
         A_NH4_output = FLOW*A_NH4,
         O_NH4_output = FLOW*O_NH4,
         A_DOC_output = FLOW*A_DOCall,
         O_DOC_output = FLOW*O_DOCall,
         A_DIN_output = FLOW*(A_NO3 + A_NH4),
         O_DIN_output = FLOW*(O_NO3 + O_NH4),
         A_TN_output = FLOW*A_totalN,
         O_TN_output = FLOW*O_totalN,
         A_TP_output = FLOW*A_totalP,
         O_TP_output = FLOW*O_totalP,
         A_TOC_output = FLOW*A_totalC,
         O_TOC_output = FLOW*O_totalC) %>% 
  select(time,A_FRP_output:O_TOC_output) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST")))
fluxdata <- merge(inputs, outputs, by="time")

retention_stratified_period <- fluxdata %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  filter(month_day <= "10-01", month_day >= "07-15") %>%
  select(-month_day)%>%
  group_by(year) %>%
  summarise(Fnet_A_TN = 100*(sum(A_TN_output)-sum(inputTN))/sum(inputTN),
            Fnet_O_TN = 100*(sum(O_TN_output)-sum(inputTN))/sum(inputTN),
            Fnet_A_TP = 100*(sum(A_TP_output)-sum(inputTP))/sum(inputTP),
            Fnet_O_TP = 100*(sum(O_TP_output)-sum(inputTP))/sum(inputTP),
            Fnet_A_TOC = 100*(sum(A_TOC_output)-sum(inputTOC))/sum(inputTOC),
            Fnet_O_TOC = 100*(sum(O_TOC_output)-sum(inputTOC))/sum(inputTOC),
            Fnet_A_DOC = 100*(sum(A_DOC_output)-sum(inputDOC))/sum(inputDOC),
            Fnet_O_DOC = 100*(sum(O_DOC_output)-sum(inputDOC))/sum(inputDOC),
            Fnet_A_DIN = 100*(sum(A_DIN_output)-sum(inputDIN))/sum(inputDIN),
            Fnet_O_DIN = 100*(sum(O_DIN_output)-sum(inputDIN))/sum(inputDIN),
            Fnet_A_NO3 = 100*(sum(A_NO3_output)-sum(inputNO3))/sum(inputNO3),
            Fnet_O_NO3 = 100*(sum(O_NO3_output)-sum(inputNO3))/sum(inputNO3),
            Fnet_A_FRP = 100*(sum(A_FRP_output)-sum(inputFRP))/sum(inputFRP),
            Fnet_O_FRP = 100*(sum(O_FRP_output)-sum(inputFRP))/sum(inputFRP),
            Fnet_A_NH4 = 100*(sum(A_NH4_output)-sum(inputNH4))/sum(inputNH4),
            Fnet_O_NH4 = 100*(sum(O_NH4_output)-sum(inputNH4))/sum(inputNH4))

#develop a matrix to fill with t-test statistics
variables <-c("TOC", "DOC", "TN", "DIN", "NH4", "NO3", "TP", "FRP")
conc_stats <- matrix(-99,length(variables), 4) 

for(i in 1:length(variables)){
  temp<-as.data.frame(retention_stratified_period[grep(variables[i],colnames(retention_stratified_period))])
  test<-t.test(temp[,1],temp[,2], paired=T)
  conc_stats[i,1]=variables[i]
  conc_stats[i,2]=round(as.numeric(test[1]), digits=2)
  conc_stats[i,3]=as.numeric(test[2])
  conc_stats[i,4]=as.numeric(test[3])
}

conc_stats1<-noquote(as.data.frame(conc_stats))
colnames(conc_stats1)<-c("Variable", "t-value", "df", "p-value")
write.csv(conc_stats1, "output/Retention_TtestStats.csv", row.names = F)
#SI Text 2, Table S3 
###################################################################


###################################################################
#comparison of anoxic vs oxic concentrations in model output
data3 <- data2 %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
  mutate(A_O_totalC = A_totalC/O_totalC,
         A_O_totalN = A_totalN/O_totalN,
         A_O_totalP = A_totalP/O_totalP,
         A_O_DOCall = A_DOCall/O_DOCall,
         A_O_NH4 = A_NH4/O_NH4,
         A_O_PO4 = A_PO4/O_PO4,
         O_A_NO3 = O_NO3/A_NO3) %>% 
  summarise(mean_A_O_totalC = mean(A_O_totalC),
            mean_A_O_totalN = mean(A_O_totalN),
            mean_A_O_totalP = mean(A_O_totalP),
            mean_A_O_DOCall = mean(A_O_DOCall),
            mean_A_O_NH4 = mean(A_O_NH4),
            mean_A_O_PO4 = mean(A_O_PO4),
            mean_O_A_NO3 = mean(O_A_NO3))
###paragraph 4 in results, comparison of anoxic vs oxic concentrations in model output
###################################################################

###################################################################
#comparison of anoxic vs oxic ratios in results
#  make ratios
data4 <- data2 %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
  mutate(A_TN = A_totalN,
         A_TP = A_totalP,
         A_TOC = A_totalC,
         O_TN = O_totalN,
         O_TP = O_totalP,
         O_TOC = O_totalC) %>% 
  mutate(A_DIN = A_NH4 + A_NO3,
         O_DIN = O_NH4 + O_NO3,
         A_NH4_DIN = A_NH4/A_DIN,
         O_NH4_DIN = O_NH4/O_DIN,
         A_TN_TP = A_TN/A_TP,
         O_TN_TP = O_TN/O_TP,
         A_DIN_PO4 = A_DIN/A_PO4,
         O_DIN_PO4 = O_DIN/O_PO4,
         A_TOC_TN = A_TOC/A_TN,
         O_TOC_TN = O_TOC/O_TN,
         A_DOC_DIN = A_DOCall/A_DIN,
         O_DOC_DIN = O_DOCall/O_DIN,
         A_DOC_NO3 = A_DOCall/A_NO3,
         O_DOC_NO3 = O_DOCall/O_NO3,
         A_DOC_NH4 = A_DOCall/A_NH4,
         O_DOC_NH4 = O_DOCall/O_NH4,
         A_TOC_TP = A_TOC/A_TP,
         O_TOC_TP = O_TOC/O_TP,
         A_DOC_PO4 = A_DOCall/A_PO4,
         O_DOC_PO4 = O_DOCall/O_PO4,
         A_O_TN_TP = A_TN_TP/O_TN_TP,
         A_O_DIN_PO4 = A_DIN_PO4/O_DIN_PO4,
         O_A_TOC_TN = O_TOC_TN/A_TOC_TN,
         O_A_DOC_DIN = O_DOC_DIN/A_DOC_DIN,
         A_O_DOC_NO3 = A_DOC_NO3/O_DOC_NO3,
         O_A_DOC_NH4 = O_DOC_NH4/A_DOC_NH4,
         A_O_TOC_TP = A_TOC_TP/O_TOC_TP,
         O_A_DOC_PO4 = O_DOC_PO4/A_DOC_PO4) %>% 
  select(time, A_O_TN_TP:O_A_DOC_PO4) %>% 
  summarise(mean_A_O_TN_TP = mean(A_O_TN_TP),
            mean_A_O_DIN_PO4 = mean(A_O_DIN_PO4),
            mean_O_A_TOC_TN = mean(O_A_TOC_TN),
            mean_O_A_DOC_DIN = mean(O_A_DOC_DIN),
            mean_A_O_DOC_NO3 = mean(A_O_DOC_NO3),
            mean_O_A_DOC_NH4 = mean(O_A_DOC_NH4),
            mean_A_O_TOC_TP = mean(A_O_TOC_TP),
            mean_O_A_DOC_PO4 = mean(O_A_DOC_PO4))
#paragraph 5 in Results, comparing anoxic vs oxic ratios + Discussion paragraph 1
###################################################################

###################################################################
#compare biogoechemical rates between anoxic vs. oxic scenarios
#calculate biogeochemical rates from each output file
#nitrogen fractions first
A_anammox <- get_var(anoxic,'NIT_anammox',z_out=9,reference = 'surface') 
O_anammox <- get_var(oxic,'NIT_anammox',z_out=9,reference = 'surface') 

A_denit<- get_var(anoxic,'NIT_denit',z_out=9,reference = 'surface') 
O_denit <- get_var(oxic,'NIT_denit',z_out=9,reference = 'surface') 

A_dnra<- get_var(anoxic,'NIT_dnra',z_out=9,reference = 'surface') 
O_dnra <- get_var(oxic,'NIT_dnra',z_out=9,reference = 'surface') 

A_nitrif<- get_var(anoxic,'NIT_nitrif',z_out=9,reference = 'surface') 
O_nitrif <- get_var(oxic,'NIT_nitrif',z_out=9,reference = 'surface') 

A_sed_nit<- get_var(anoxic,'NIT_sed_nit',z_out=9,reference = 'surface') 
O_sed_nit <- get_var(oxic,'NIT_sed_nit',z_out=9,reference = 'surface') 

A_sed_amm<- get_var(anoxic,'NIT_sed_amm',z_out=9,reference = 'surface') 
O_sed_amm <- get_var(oxic,'NIT_sed_amm',z_out=9,reference = 'surface') 

A_OGM_denit<- get_var(anoxic,'OGM_denit',z_out=9,reference = 'surface') 
O_OGM_denit <- get_var(oxic,'OGM_denit',z_out=9,reference = 'surface') 

A_don_miner<- get_var(anoxic,'OGM_don_miner',z_out=9,reference = 'surface') 
O_don_miner <- get_var(oxic,'OGM_don_miner',z_out=9,reference = 'surface') 

A_donr_miner<- get_var(anoxic,'OGM_donr_miner',z_out=9,reference = 'surface') 
O_donr_miner <- get_var(oxic,'OGM_donr_miner',z_out=9,reference = 'surface') 

A_phyto_nup_nh4<- get_var(anoxic,'PHY_NUP_nh4',z_out=9,reference = 'surface') 
O_phyto_nup_nh4 <- get_var(oxic,'PHY_NUP_nh4',z_out=9,reference = 'surface') 

A_phyto_nup_no3<- get_var(anoxic,'PHY_NUP_no3',z_out=9,reference = 'surface') 
O_phyto_nup_no3 <- get_var(oxic,'PHY_NUP_no3',z_out=9,reference = 'surface') 

#then carbon
A_doc_miner<- get_var(anoxic,'OGM_doc_miner',z_out=9,reference = 'surface') 
O_doc_miner <- get_var(oxic,'OGM_doc_miner',z_out=9,reference = 'surface') 

A_docr_miner<- get_var(anoxic,'OGM_docr_miner',z_out=9,reference = 'surface') 
O_docr_miner <- get_var(oxic,'OGM_docr_miner',z_out=9,reference = 'surface') 

A_poc_hydrol<- get_var(anoxic,'OGM_poc_hydrol',z_out=9,reference = 'surface') 
O_poc_hydrol <- get_var(oxic,'OGM_poc_hydrol',z_out=9,reference = 'surface') 

A_sed_doc<- get_var(anoxic,'OGM_sed_doc',z_out=9,reference = 'surface') 
O_sed_doc <- get_var(oxic,'OGM_sed_doc',z_out=9,reference = 'surface') 

#finally phosphorus
A_dop_miner<- get_var(anoxic,'OGM_dop_miner',z_out=9,reference = 'surface') 
O_dop_miner <- get_var(oxic,'OGM_dop_miner',z_out=9,reference = 'surface') 

A_dopr_miner<- get_var(anoxic,'OGM_dopr_miner',z_out=9,reference = 'surface') 
O_dopr_miner <- get_var(oxic,'OGM_dopr_miner',z_out=9,reference = 'surface') 

A_sed_frp<- get_var(anoxic,'PHS_sed_frp',z_out=9,reference = 'surface') 
O_sed_frp <- get_var(oxic,'PHS_sed_frp',z_out=9,reference = 'surface') 

A_phyto_pup<- get_var(anoxic,'PHY_PUP',z_out=9,reference = 'surface') 
O_phyto_pup <- get_var(oxic,'PHY_PUP',z_out=9,reference = 'surface') 

#these ones for fun and reality checks
A_anaerobic<- get_var(anoxic,'OGM_anaerobic',z_out=9,reference = 'surface') 
O_anaerobic <- get_var(oxic,'OGM_anaerobic',z_out=9,reference = 'surface') 

A_sed_oxy<- get_var(anoxic,'OXY_sed_oxy',z_out=9,reference = 'surface') 
O_sed_oxy <- get_var(oxic,'OXY_sed_oxy',z_out=9,reference = 'surface') 


#bind the data!
data<-as.data.frame(cbind(A_anammox,A_denit[,2],A_dnra[,2],A_nitrif[,2],A_sed_nit[,2],
                          A_sed_amm[,2],A_OGM_denit[,2],A_don_miner[,2],A_donr_miner[,2],
                          A_phyto_nup_nh4[,2],A_phyto_nup_no3[,2],
                          A_doc_miner[,2],A_docr_miner[,2],A_poc_hydrol[,2],A_sed_doc[,2],
                          A_dop_miner[,2],A_dopr_miner[,2],A_sed_frp[,2],A_phyto_pup[,2],
                          A_anaerobic[,2],A_sed_oxy[,2],
                          O_anammox[,2],O_denit[,2],O_dnra[,2],O_nitrif[,2],O_sed_nit[,2],
                          O_sed_amm[,2],O_OGM_denit[,2],O_don_miner[,2],O_donr_miner[,2],
                          O_phyto_nup_nh4[,2],O_phyto_nup_no3[,2],
                          O_doc_miner[,2],O_docr_miner[,2],O_poc_hydrol[,2],O_sed_doc[,2],
                          O_dop_miner[,2],O_dopr_miner[,2],O_sed_frp[,2],O_phyto_pup[,2],
                          O_anaerobic[,2],O_sed_oxy[,2])) 
colnames(data) = c("time", "A_anammox", "A_denit", "A_dnra", "A_nitrif","A_sed_nit",
                   "A_sed_amm","A_OGM_denit", "A_don_miner", "A_donr_miner", 
                   "A_phyto_nup_nh4", "A_phyto_nup_no3", 
                   "A_doc_miner","A_docr_miner", "A_poc_hydrol", "A_sed_doc", 
                   "A_dop_miner", "A_dopr_miner", "A_sed_frp","A_phyto_pup",
                   "A_anaerobic","A_sed_oxy",
                   "O_anammox", "O_denit", "O_dnra", "O_nitrif","O_sed_nit",
                   "O_sed_amm","O_OGM_denit", "O_don_miner", "O_donr_miner", 
                   "O_phyto_nup_nh4", "O_phyto_nup_no3", 
                   "O_doc_miner","O_docr_miner", "O_poc_hydrol", "O_sed_doc", 
                   "O_dop_miner", "O_dopr_miner", "O_sed_frp","O_phyto_pup",
                   "O_anaerobic","O_sed_oxy") 

biogeo_rates <- data %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
  group_by(year) %>% 
  summarise(median_A_sed_amm = median(A_sed_amm),
            median_O_sed_amm = median(O_sed_amm),
            median_A_sed_frp = median(A_sed_frp),
            median_O_sed_frp = median(O_sed_frp),
            median_A_sed_doc = median(A_sed_doc),
            median_O_sed_doc = median(O_sed_doc),
            median_A_don_miner = median(A_don_miner),
            median_A_donr_miner = median(A_donr_miner),
            median_O_don_miner = median(O_don_miner),
            median_O_donr_miner = median(O_donr_miner),
            median_A_dop_miner = median(A_dop_miner),
            median_A_dopr_miner = median(A_dopr_miner),
            median_O_dop_miner = median(O_dop_miner),
            median_O_dopr_miner = median(O_dopr_miner),
            median_A_doc_miner = median(A_doc_miner),
            median_A_docr_miner = median(A_docr_miner),
            median_O_doc_miner = median(O_doc_miner),
            median_O_docr_miner = median(O_docr_miner),
            median_A_nitrif = median(A_nitrif),
            median_O_nitrif = median(O_nitrif)) %>% 
  summarise(A_O_sed_amm = median(median_A_sed_amm/median_O_sed_amm),
         A_O_sed_frp = median(median_A_sed_frp/median_O_sed_frp),
         A_O_sed_doc = median(median_A_sed_doc/median_O_sed_doc),
         O_A_don_miner = median(median_O_don_miner/median_A_don_miner),
         O_A_donr_miner = median(median_O_donr_miner/median_A_donr_miner),
         O_A_dop_miner = median(median_O_dop_miner/median_A_dop_miner),
         O_A_dopr_miner = median(median_O_dopr_miner/median_A_dopr_miner),
         O_A_doc_miner = median(median_O_doc_miner/median_A_doc_miner),
         O_A_docr_miner = median(median_O_docr_miner/median_A_docr_miner),
         O_nitrif = median(median_O_nitrif),
         A_sed_amm = median(median_A_sed_amm),
         A_sed_doc = median(median_A_sed_doc),
         A_doc_miner = median(median_A_doc_miner)) 
#needed for results paragraph comparing rates in anoxic vs oxic scenarios
biogeo_rates$A_sed_amm/biogeo_rates$O_nitrif
biogeo_rates$A_sed_doc/biogeo_rates$A_doc_miner
###################################################################         

###################################################################
# Calculating Sediment burial rates
# particulate organic carbon first
A_POC <- get_var(anoxic, "OGM_Psed_poc",z_out=9.2,reference = 'surface')
O_POC <- get_var(oxic, "OGM_Psed_poc",z_out=9.2,reference = 'surface')
B_POC<- get_var(nc_file, "OGM_Psed_poc",z_out=9.2,reference = 'surface')

plot(A_POC$DateTime, A_POC$OGM_Psed_poc_9, col="red", type="l")
lines(O_POC$DateTime, O_POC$OGM_Psed_poc_9, col="blue")
lines(B_POC$DateTime, B_POC$OGM_Psed_poc_9, col="green")

data<-cbind(A_POC, O_POC, B_POC)
colnames(data)<-c("time", "Anoxic", "time2", "Oxic", "time3", "Baseline")

POC_burial_by_yr <- data %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  mutate(year=year(time)) %>%
  group_by(year) %>%
  summarise(mean_anoxic = mean(Anoxic), mean_oxic=mean(Oxic), mean_baseline = mean(Baseline))
t.test(POC_burial_by_yr$mean_anoxic, POC_burial_by_yr$mean_oxic, paired=T)
#for SI Text 2, Table S4

POC_burial <- POC_burial_by_yr %>% drop_na() %>%
  mutate(total_anoxic = mean_anoxic*12*365*24*60*60/1000,
         total_oxic = mean_oxic*12*365*24*60*60/1000,
         total_baseline= mean_baseline*12*365*24*60*60/1000) %>%
  summarise(mean_yearly_anoxic_C_burial = mean(total_anoxic), sd_anoxic = std_err(total_anoxic),
            mean_yearly_oxic_C_burial = mean(total_oxic), sd_oxic = std_err(total_oxic),
            mean_yearly_baseline_C_burial = mean(total_baseline), sd_base = std_err(total_baseline))
#rates are in mmol/m2/second and need to be converted to g/m2/yr by multiplying by 12*365*24*60*60/1000
#needed for last Results paragraph on sediment C rates and Discussion paragraph on POC burial

#######nitrogen second
A_PON <- get_var(anoxic, "OGM_Psed_pon",z_out=9.2,reference = 'surface')
O_PON <- get_var(oxic, "OGM_Psed_pon",z_out=9.2,reference = 'surface')
B_PON<- get_var(nc_file, "OGM_Psed_pon",z_out=9.2,reference = 'surface')

plot(A_PON$DateTime, A_PON$OGM_Psed_pon_9.2, col="red", type="l")
lines(O_PON$DateTime, O_PON$OGM_Psed_pon_9.2, col="blue")
lines(B_PON$DateTime, B_PON$OGM_Psed_pon_9.2, col="green")

data<-cbind(A_PON, O_PON, B_PON)
colnames(data)<-c("time", "Anoxic", "time2", "Oxic", "time3", "Baseline")

PON_burial_by_yr <- data %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  mutate(year=year(time)) %>%
  group_by(year) %>%
  summarise(mean_anoxic = mean(Anoxic), mean_oxic=mean(Oxic), mean_baseline = mean(Baseline))
t.test(PON_burial_by_yr$mean_anoxic, PON_burial_by_yr$mean_oxic, paired=T)

PON_burial <- PON_burial_by_yr %>% drop_na() %>%
  mutate(total_anoxic = mean_anoxic*14*365*24*60*60/1000,
         total_oxic = mean_oxic*14*365*24*60*60/1000,
         total_baseline= mean_baseline*14*365*24*60*60/1000) %>%
  summarise(mean_yearly_anoxic_N_burial = mean(total_anoxic), sd_anoxic = std_err(total_anoxic),
            mean_yearly_oxic_N_burial = mean(total_oxic), sd_oxic = std_err(total_oxic),
            mean_yearly_baseline_N_burial = mean(total_baseline), sd_base = std_err(total_baseline))
#rates are in mmol/m2/second and need to be converted to g/m2/yr by multiplying by 12*365*24*60*60/1000
#needed for last Results paragraph on sediment N rates and Discussion paragraph on PON burial

#######phosphorus third
A_POP <- get_var(anoxic, "OGM_Psed_pop",z_out=9.2,reference = 'surface')
O_POP <- get_var(oxic, "OGM_Psed_pop",z_out=9.2,reference = 'surface')
B_POP<- get_var(nc_file, "OGM_Psed_pop",z_out=9.2,reference = 'surface')

plot(A_POP$DateTime, A_POP$OGM_Psed_pop_9.2, col="red", type="l")
lines(O_POP$DateTime, O_POP$OGM_Psed_pop_9.2, col="blue")
lines(B_POP$DateTime, B_POP$OGM_Psed_pop_9.2, col="green")

data<-cbind(A_POP, O_POP, B_POP)
colnames(data)<-c("time", "Anoxic", "time2", "Oxic", "time3", "Baseline")

POP_burial_by_yr <- data %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  mutate(year=year(time)) %>%
  group_by(year) %>%
  summarise(mean_anoxic = mean(Anoxic), mean_oxic=mean(Oxic), mean_baseline = mean(Baseline))
t.test(POP_burial_by_yr$mean_anoxic, POP_burial_by_yr$mean_oxic, paired=T)
#for SI Text 2, Table S4

POP_burial <- POP_burial_by_yr %>% drop_na() %>%
  mutate(total_anoxic = mean_anoxic*31*365*24*60*60/1000,
         total_oxic = mean_oxic*31*365*24*60*60/1000,
         total_baseline= mean_baseline*31*365*24*60*60/1000) %>%
  summarise(mean_yearly_anoxic_P_burial = mean(total_anoxic), sd_anoxic = std_err(total_anoxic),
            mean_yearly_oxic_P_burial = mean(total_oxic), sd_oxic = std_err(total_oxic),
            mean_yearly_baseline_P_burial = mean(total_baseline), sd_base = std_err(total_baseline))
#rates are in mmol/m2/second and need to be converted to g/m2/yr by multiplying by 12*365*24*60*60/1000
#needed for last Results paragraph on sediment P rates and Discussion paragraph on POP burial
###################################################################


###################################################################
#####Discussion#####
#getting numbers interspersed in the text

retention_stratified_period
#provides stats for Discussion paragraph on range of boxplot values in Fig. 7

median(retention_stratified_period$Fnet_A_NH4/retention_stratified_period$Fnet_O_NH4)
#provides stats for Discussion paragraph on NH4 retention changes in anoxic vs oxic conditions

#provides data on proportion of DRP/TP for discussion
vals <- merge(observed, totals, by=c("DateTime", "Depth")) %>% 
  filter(Depth ==9) %>% 
  select(DateTime, PHS_frp, TOT_tp) %>% drop_na() %>% 
  mutate(DRP_TP = PHS_frp/TOT_tp)
hist(vals$DRP_TP)
median(vals$DRP_TP)
std_err(vals$DRP_TP)

#Conclusions paragraph
109-95#difference in C fluxes
602-195#difference in N fluxes
12-10#difference in P fluxes
###################################################################
