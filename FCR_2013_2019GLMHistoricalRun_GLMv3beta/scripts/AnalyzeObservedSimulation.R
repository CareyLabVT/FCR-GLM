#to compare scenarios for oxic vs anoxic scenario conditions in FCR
#time series of anoxic vs oxic concentrations

#load packages
library(zoo)
library(tidyverse)
library(lubridate)
library(GLMr)
library(glmtools)
#library(gganimate)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
#setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#set GLM output files for analysis & figures

#baseline scenario, based on observed SSS practices
nc_file <- file.path(sim_folder, 'output/output_2013_2019.nc')

#####make the anoxic vs oxic dataset####
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
  
#write.csv(max_concs, "output/maximumSummerObservedConcentrations_22Feb2021.csv", row.names=F)

####boxplots of median summer ratios####
pdf("figures/BoxplotCNPRatios_AnoxicOxicScenarios.pdf", width=8.5, height=11)
par(mfrow=c(4,2))

#boxplots for TN:TP
boxplot(mediandata$med_A_TN_TP,mediandata$med_O_TN_TP, ylab="median TN:TP", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TN:TP")

#boxplots for DIN:DRP
boxplot(mediandata$med_A_DIN_PO4,mediandata$med_O_DIN_PO4, ylab="median DIN:DRP", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DIN:DRP")

#boxplots for TOC:TN
boxplot(mediandata$med_A_TOC_TN,mediandata$med_O_TOC_TN, ylab="median TOC:TN", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TOC:TN")

#boxplots for DOC:DIN
boxplot(mediandata$med_A_DOC_DIN,mediandata$med_O_DOC_DIN, ylab="median DOC:DIN", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DOC:DIN")

#boxplots for DOC:NH4
boxplot(mediandata$med_A_DOC_NH4,mediandata$med_O_DOC_NH4, ylab="median DOC:NH4", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DOC:NH4")

#boxplots for DOC:NO3
boxplot(mediandata$med_A_DOC_NO3,mediandata$med_O_DOC_NO3, ylab="median DOC:NO3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DOC:NO3")

#boxplots for TOC:TP
boxplot(mediandata$med_A_TOC_TP,mediandata$med_O_TOC_TP, ylab="median TOC:TP", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TOC:TP")

#boxplots for DOC:DRP
boxplot(mediandata$med_A_DOC_PO4,mediandata$med_O_DOC_PO4, ylab="median DOC:DRP", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DOC:DRP")

dev.off()

####stats for median summer ratios####

t.test(mediandata$med_A_TN_TP,mediandata$med_O_TN_TP, paired=TRUE)
t.test(mediandata$med_A_DIN_PO4,mediandata$med_O_DIN_PO4, paired=TRUE)
t.test(mediandata$med_A_TOC_TN,mediandata$med_O_TOC_TN, paired=TRUE)
t.test(mediandata$med_A_DOC_DIN,mediandata$med_O_DOC_DIN, paired=TRUE)
t.test(mediandata$med_A_DOC_NH4,mediandata$med_O_DOC_NH4, paired=TRUE)
t.test(mediandata$med_A_DOC_NO3,mediandata$med_O_DOC_NO3, paired=TRUE)
t.test(mediandata$med_A_TOC_TP,mediandata$med_O_TOC_TP, paired=TRUE)
t.test(mediandata$med_A_DOC_PO4,mediandata$med_O_DOC_PO4, paired=TRUE)


####boxplots of raw (median) summer concentrations####
pdf("figures/BoxplotCNPConcentrations_AnoxicOxicScenarios.pdf", width=8.5, height=11)
par(mfrow=c(4,2))

#boxplots for TOC
boxplot(mediandata$med_A_TOC,mediandata$med_O_TOC, ylab="median TOC mmol/m3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TOC concentration")

#boxplots for DOC
boxplot(mediandata$med_A_DOC,mediandata$med_O_DOC, ylab="median DOC mmol/m3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DOC concentration")

#boxplots for TN
boxplot(mediandata$med_A_TN,mediandata$med_O_TN, ylab="median TN mmol/m3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TN concentration")

#boxplots for DIN
boxplot(mediandata$med_A_DIN,mediandata$med_O_DIN, ylab="median DIN mmol/m3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DIN concentration")

#boxplots for NH4
boxplot(mediandata$med_A_NH4,mediandata$med_O_NH4, ylab="median NH4 mmol/m3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="NH4 concentration")

#boxplots for NO3
boxplot(mediandata$med_A_NO3,mediandata$med_O_NO3, ylab="median NO3 mmol/m3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="NO3 concentration")

#boxplots for TP
boxplot(mediandata$med_A_TP,mediandata$med_O_TP, ylab="median TP mmol/m3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TP concentration")

#boxplots for PO4
boxplot(mediandata$med_A_PO4,mediandata$med_O_PO4, ylab="median P04 mmol/m3", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="PO4 concentration")

dev.off()

####stats of raw (median) summer concs####
t.test(mediandata$med_A_TOC,mediandata$med_O_TOC, paired=T)
t.test(mediandata$med_A_DOC,mediandata$med_O_DOC, paired=T)
t.test(mediandata$med_A_TN,mediandata$med_O_TN, paired=T)
t.test(mediandata$med_A_DIN,mediandata$med_O_DIN, paired=T)
t.test(mediandata$med_A_NH4,mediandata$med_O_NH4, paired=T)
t.test(mediandata$med_A_NO3,mediandata$med_O_NO3, paired=T)
t.test(mediandata$med_A_TP,mediandata$med_O_TP, paired=T)
t.test(mediandata$med_A_PO4,mediandata$med_O_PO4, paired=T)


####calculate retention####
#first need to get inflow concentrations of CNP
nml_file <- paste0(sim_folder,"/glm3.nml")
nml <- read_nml(nml_file) 
inflowfiles<-get_nml_value(nml, arg_name="inflow_fl")

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

outputs <- merge(outflow,data1, by="time") %>% 
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

fluxdata <- merge(inputs, outputs, by="time") %>% 
  mutate(year=year(time),
         DOY = yday(time)) #%>% 
  #write.csv("output/DailyInputsOutputs_8Sep2020.csv", row.names = F)

#from Farrell et al 2020 & Powers et al 2015
#retention for full calendar year
retention <- fluxdata %>% 
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
            Fnet_O_NH4 = 100*(sum(O_NH4_output)-sum(inputNH4))/sum(inputNH4)) %>% 
  write.csv("output/RetentionPerYear_30Nov2020.csv", row.names=F)
#Data indicate net flux (%) for each year; flux < 0 represents net retention and/or removal, 
  #flux > 0 represents net downstream export



####boxplots annual retention####
pdf("figures/BoxplotAnnualDownstreamExport_AnoxicOxicScenarios.pdf", width=8.5, height=11)
par(mfrow=c(4,2))

#boxplots for TOC
boxplot(retention$Fnet_A_TOC,retention$Fnet_O_TOC, ylab="TOC flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TOC export")

#boxplots for DOC
boxplot(retention$Fnet_A_DOC,retention$Fnet_O_DOC, ylab="DOC flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DOC export")

#boxplots for TN
boxplot(retention$Fnet_A_TN,retention$Fnet_O_TN, ylab="TN flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TN export")

#boxplots for DIN
boxplot(retention$Fnet_A_DIN,retention$Fnet_O_DIN, ylab="DIN flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DIN export")

#boxplots for NH4
boxplot(retention$Fnet_A_NH4,retention$Fnet_O_NH4, ylab="NH4 flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="NH4 export")

#boxplots for NO3
boxplot(retention$Fnet_A_NO3,retention$Fnet_O_NO3, ylab="NO3 flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="NO3 export")

#boxplots for TP
boxplot(retention$Fnet_A_TP,retention$Fnet_O_TP, ylab="TP flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TP export")

#boxplots for PO4
boxplot(retention$Fnet_A_FRP,retention$Fnet_O_FRP, ylab="P04 flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="PO4 export")

dev.off()


####boxplots summer retention####
retention <- fluxdata %>% 
  group_by(year) %>% 
  filter(DOY < 275, DOY > 195) %>% 
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
            Fnet_O_NH4 = 100*(sum(O_NH4_output)-sum(inputNH4))/sum(inputNH4)) #%>% 
  #write.csv("output/RetentionSummerOnly_6Jan2020.csv", row.names=F)
#Data indicate net flux (%) for each year; flux < 0 represents net retention and/or removal, 
#flux > 0 represents net downstream export

pdf("figures/BoxplotSummerOnlyDownstreamExport_AnoxicOxicScenarios.pdf", width=8.5, height=11)
par(mfrow=c(4,2))

#boxplots for TOC
boxplot(retention$Fnet_A_TOC,retention$Fnet_O_TOC, ylab="TOC flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TOC export")

#boxplots for DOC
boxplot(retention$Fnet_A_DOC,retention$Fnet_O_DOC, ylab="DOC flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DOC export")

#boxplots for TN
boxplot(retention$Fnet_A_TN,retention$Fnet_O_TN, ylab="TN flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TN export")

#boxplots for DIN
boxplot(retention$Fnet_A_DIN,retention$Fnet_O_DIN, ylab="DIN flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="DIN export")

#boxplots for NH4
boxplot(retention$Fnet_A_NH4,retention$Fnet_O_NH4, ylab="NH4 flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="NH4 export")

#boxplots for NO3
boxplot(retention$Fnet_A_NO3,retention$Fnet_O_NO3, ylab="NO3 flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="NO3 export")

#boxplots for TP
boxplot(retention$Fnet_A_TP,retention$Fnet_O_TP, ylab="TP flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="TP export")

#boxplots for PO4
boxplot(retention$Fnet_A_FRP,retention$Fnet_O_FRP, ylab="P04 flux (%)", col=c("red","blue"),
        names=c("Anoxic", "Oxic"), main="PO4 export")

dev.off()

####stats for summer retention####

t.test(retention$Fnet_A_TOC,retention$Fnet_O_TOC, paired=T)
t.test(retention$Fnet_A_DOC,retention$Fnet_O_DOC, paired=T)
t.test(retention$Fnet_A_TN,retention$Fnet_O_TN, paired=T)
t.test(retention$Fnet_A_DIN,retention$Fnet_O_DIN, paired=T)
t.test(retention$Fnet_A_NH4,retention$Fnet_O_NH4, paired=T)
t.test(retention$Fnet_A_NO3,retention$Fnet_O_NO3, paired=T)
t.test(retention$Fnet_A_TP,retention$Fnet_O_TP, paired=T)
t.test(retention$Fnet_A_FRP,retention$Fnet_O_FRP, paired=T)


