#to get all of the numbers needed in the manuscript text itself!

#load packages
library(zoo)
library(tidyverse)
library(lubridate)
library(glmtools)
library(dplyr)

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

######calculating the summer anoxic vs oxic years
data <- merge(observed, totals, by=c('time', "Depth"), all.x=T, all.y=T) %>% 
  filter(Depth==9) %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
  mutate(DOC = OGM_doc + OGM_docr) %>% 
  select(time, year, Depth, NIT_amm:PHS_frp,DOC, TOT_tn:TOT_tp) %>% 
  group_by(year) %>% drop_na() %>% 
  summarise(mean_NH4 = mean(NIT_amm),
            mean_NIT = mean(NIT_nit),
            mean_FRP = mean(PHS_frp),
            mean_DOC = mean(DOC),
            mean_TN = mean(TOT_tn),
            mean_TP = mean(TOT_tp),
            max_NH4 = max(NIT_amm),
            max_NIT = max(NIT_nit),
            max_FRP = max(PHS_frp),
            max_DOC = max(DOC),
            max_TN = max(TOT_tn),
            max_TP = max(TOT_tp),
            min_NIT = min(NIT_nit)) %>%
  mutate(Status = case_when(
    year == 2014 | year == 2018 | year == 2019 ~ "Anoxic",
    year == 2015 | year == 2016 | year == 2017 ~ "Oxic")) %>% 
  group_by(Status) %>% 
  summarise(mean_annual_NH4 = mean(mean_NH4),
            mean_annual_NIT = mean(mean_NIT),
            mean_annual_FRP = mean(mean_FRP),
            mean_annual_DOC = mean(mean_DOC),
            mean_annual_TN = mean(mean_TN),
            mean_annual_TP = mean(mean_TP),
            max_annual_NH4 = mean(max_NH4),
            max_annual_NIT = mean(max_NIT),
            max_annual_FRP = mean(max_FRP),
            max_annual_DOC = mean(max_DOC),
            max_annual_TN = mean(max_TN),
            max_annual_TP = mean(max_TP))

(data$max_annual_NH4[1]-data$max_annual_NH4[2])/data$max_annual_NH4[2]
(data$max_annual_FRP[1]-data$max_annual_FRP[2])/data$max_annual_FRP[2]
(data$max_annual_DOC[1]-data$max_annual_DOC[2])/data$max_annual_DOC[2]
(data$max_annual_NIT[1]-data$max_annual_NIT[2])/data$max_annual_NIT[2]
(data$max_annual_TN[1]-data$max_annual_TN[2])/data$max_annual_TN[2]
(data$max_annual_TP[1]-data$max_annual_TP[2])/data$max_annual_TP[2]
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








#####study the observed data####
#pull out deep-water chemistry from each output file
B_oxy <- get_var(anoxic,'OXY_oxy',z_out=9,reference = 'surface') 
B_DOCr <- get_var(anoxic, "OGM_docr",z_out=9,reference = 'surface')
B_DOC <- get_var(anoxic, "OGM_doc",z_out=9,reference = 'surface')
B_NO3 <- get_var(anoxic, "NIT_nit",z_out=9,reference = 'surface')
B_NH4 <- get_var(anoxic, "NIT_amm",z_out=9,reference = 'surface')
B_PO4 <- get_var(anoxic, "PHS_frp",z_out=9,reference = 'surface')
B_TN <- get_var(anoxic, "TOT_tn",z_out=9,reference = 'surface')
B_TP <- get_var(anoxic, "TOT_tp",z_out=9,reference = 'surface')
B_TOC <- get_var(anoxic, "TOT_toc",z_out=9,reference = 'surface')

B_cyano <- get_var(anoxic,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(B_cyanoN = CyanoConc*0.12,
         B_cyanoP = CyanoConc*0.0005,
         B_cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,B_cyanoN, B_cyanoP, B_cyanoC)

B_green <- get_var(anoxic,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(B_greenN = GreenConc*0.12,
         B_greenP = GreenConc*0.0005,
         B_greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,B_greenN, B_greenP, B_greenC)

B_diatom <- get_var(anoxic,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(B_diatomN = DiatomConc*0.12,
         B_diatomP = DiatomConc*0.0005,
         B_diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,B_diatomN, B_diatomP, B_diatomC)

#bind the data together
data<-as.data.frame(cbind(B_oxy,B_DOCr[,2],B_DOC[,2],B_NO3[,2],B_NH4[,2],B_PO4[,2],
                          B_TN[,2],B_TP[,2],B_TOC[,2],
                          B_diatom[,3:5],B_cyano[,3:5],B_green[,3:5]))
colnames(data) = c("time", "B_oxy", "B_DOCr", "B_DOC", "B_NO3","B_NH4","B_PO4",
                   "B_TN", "B_TP", "B_TOC", "B_diatomN", "B_diatomP", "B_diatomC",
                   "B_cyanoN", "B_cyanoP", "B_cyanoC", "B_greenN", "B_greenP", "B_greenC") 
data1 <- data %>% 
  mutate(B_DOCall = B_DOCr + B_DOC,
         B_totalN = B_TN + B_diatomN + B_cyanoN + B_greenN,
         B_totalP = B_TP + B_diatomP + B_cyanoP + B_greenP,
         B_totalC = B_TOC + B_diatomC + B_cyanoC + B_greenC) %>% 
  select(time:B_TOC,B_oxy:B_TOC,B_DOCall:B_totalC) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST")))

#####make ratios####
#only focus on summer stratified period of July 15-Oct 1
data4 <- data1 %>% 
  mutate(B_DIN = B_NH4 + B_NO3,
         B_TN_TP = B_TN/B_TP,
         B_DIN_PO4 = B_DIN/B_PO4,
         B_TOC_TN = B_TOC/B_TN,
         B_DOC_DIN = B_DOCall/B_DIN,
         B_DOC_NH4 = B_DOC/B_NH4,
         B_DOC_NO3 = B_DOC/B_NO3,
         B_TOC_TP = B_TOC/B_TP,
         B_DOC_PO4 = B_DOCall/B_PO4) %>%  
  mutate(year=year(time),
         DOY = yday(time)) %>% 
  dplyr::filter(DOY < 275, DOY > 195)

#write.csv(data4, "output/DailyObservedRates&Ratios_22Feb2021.csv", row.names = F)

#####find max concentrations during the summer

max_concs <- data4 %>% 
  group_by(year) %>% 
  summarise(max_B_TN_TP = max(B_TN_TP),
            max_B_DIN_PO4 = max(B_DIN_PO4),
            max_B_TOC_TN = max(B_TOC_TN),
            max_B_DOC_DIN = max(B_DOC_DIN),
            max_B_DOC_NH4 = max(B_DOC_NH4),
            max_B_DOC_NO3 = max(B_DOC_NO3),
            max_B_TOC_TP = max(B_TOC_TP),
            max_B_DOC_PO4 = max(B_DOC_PO4),
            max_B_TOC = max(B_TOC),
            max_B_TN = max(B_TN),
            max_B_TP = max(B_TP),
            max_B_DIN = max(B_DIN),
            max_B_DOC = max(B_DOCall),
            max_B_NH4 = max(B_NH4),
            max_B_NO3 = max(B_NO3),
            max_B_PO4 = max(B_PO4)) %>% 
  mutate(Status = case_when(
    year == 2014 | year == 2018 | year == 2019 ~ "Anoxic",
    year == 2015 | year == 2016 | year == 2017 ~ "Oxic",
    year == 2013 ~ "Inbetween")) %>% 
  group_by(Status) %>% 
  summarise(mean_max_B_TN = mean(max_B_TN),
            mean_max_B_TP = mean(max_B_TP),
            mean_max_B_DIN = mean(max_B_DIN),
            mean_max_B_DOC = mean(max_B_DOC),
            mean_max_B_NH4 = mean(max_B_NH4),
            mean_max_B_NO3 = mean(max_B_NO3),
            mean_max_B_PO4 = mean(max_B_PO4))
