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


#####study the baseline simulation####
#pull out deep-water chemistry from each output file
B_oxy <- get_var(nc_file,'OXY_oxy',z_out=9,reference = 'surface') 
B_DOCr <- get_var(nc_file, "OGM_docr",z_out=9,reference = 'surface')
B_DOC <- get_var(nc_file, "OGM_doc",z_out=9,reference = 'surface')
B_NO3 <- get_var(nc_file, "NIT_nit",z_out=9,reference = 'surface')
B_NH4 <- get_var(nc_file, "NIT_amm",z_out=9,reference = 'surface')
B_PO4 <- get_var(nc_file, "PHS_frp",z_out=9,reference = 'surface')
B_TN <- get_var(nc_file, "TOT_tn",z_out=9,reference = 'surface')
B_TP <- get_var(nc_file, "TOT_tp",z_out=9,reference = 'surface')
B_TOC <- get_var(nc_file, "TOT_toc",z_out=9,reference = 'surface')

B_cyano <- get_var(nc_file,var_name = 'PHY_cyano',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(B_cyanoN = CyanoConc*0.12,
         B_cyanoP = CyanoConc*0.0005,
         B_cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,B_cyanoN, B_cyanoP, B_cyanoC)

B_green <- get_var(nc_file,var_name = 'PHY_green',z_out=9,reference = 'surface') %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(B_greenN = GreenConc*0.12,
         B_greenP = GreenConc*0.0005,
         B_greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,B_greenN, B_greenP, B_greenC)

B_diatom <- get_var(nc_file,var_name = 'PHY_diatom',z_out=9,reference = 'surface') %>% 
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
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time)) %>%
  dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
  select(time, year, B_NO3:B_PO4,B_DOCall:B_totalC) %>% 
  group_by(year) %>% drop_na() %>% 
  summarise(mean_NH4 = mean(B_NH4),
            mean_NIT = mean(B_NO3),
            mean_FRP = mean(B_PO4),
            mean_DOC = mean(B_DOCall),
            mean_TN = mean(B_totalN),
            mean_TP = mean(B_totalP),
            max_NH4 = max(B_NH4),
            max_NIT = max(B_NO3),
            max_FRP = max(B_PO4),
            max_DOC = max(B_DOCall),
            max_TN = max(B_totalN),
            max_TP = max(B_totalP)) %>%
  mutate(Status = case_when(
    year == 2014 | year == 2018 | year == 2019 ~ "Anoxic",
    year == 2015 | year == 2016 | year == 2017 ~ "Oxic",
    year == 2013 ~ "TheInBetween")) %>% 
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

(data1$max_annual_NH4[1]-data1$max_annual_NH4[2])/data1$max_annual_NH4[2]
(data1$max_annual_FRP[1]-data1$max_annual_FRP[2])/data1$max_annual_FRP[2]
(data1$max_annual_DOC[1]-data1$max_annual_DOC[2])/data1$max_annual_DOC[2]
(data1$max_annual_NIT[1]-data1$max_annual_NIT[2])/data1$max_annual_NIT[2]
(data1$max_annual_TN[1]-data1$max_annual_TN[2])/data1$max_annual_TN[2]
(data1$max_annual_TP[1]-data1$max_annual_TP[2])/data1$max_annual_TP[2]
##for third paragraph looking at the summers with continuous vs limited oxygenation in baseline sim


#####make the anoxic vs oxic dataset####
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
         

#####make ratios####
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
#paragraph 5 in results, comparing anoxic vs oxic ratios






