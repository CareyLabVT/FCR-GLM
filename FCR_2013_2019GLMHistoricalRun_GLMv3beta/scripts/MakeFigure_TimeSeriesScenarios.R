#to compare scenarios for oxic vs anoxic summer conditions in FCR
#time series of anoxic vs oxic concentrations

#load packages
library(zoo)
library(tidyverse)
library(lubridate)
#library(gganimate)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#set GLM output files for analysis & figures

#baseline scenario, based on observed SSS practices
#nc_file <- file.path(sim_folder, 'output/output_2013_2019.nc')

#anoxic scenario, no SSS activation throughout
anoxic <- file.path(sim_folder, 'output/output_anoxic.nc')

#oxic scenario, SSS on in summer May 15-Nov 15 at full level
oxic <- file.path(sim_folder, 'output/output_oxic.nc')

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

####make time series plots######

pdf("figures/TimeSeries_AnoxicOxicScenarios.pdf", width=8.5, height=11)
par(mfrow=c(4,2))

#oxygen plot
plot(data1$time, data1$A_oxy, type="l", col="red", ylim=c(0,800), xlab="time", ylab="DO mmol/m3", main="DO mmol/m3")
  points(data1$time, data1$O_oxy, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#DOC plot
plot(data1$time, data1$A_DOCall, type="l", col="red", ylim=c(130,360), xlab="time", ylab="DOC mmol/m3",main="DOC mmol/m3")
  points(data1$time, data1$O_DOCall, type="l", col="blue")
  #legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#NO3 plot
plot(data1$time, data1$A_NO3, type="l", col="red", ylim=c(0,1.2), xlab="time", ylab="NO3 mmol/m3", main="NO3 mmol/m3")
  points(data1$time, data1$O_NO3, type="l", col="blue")
  #legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#NH4 plot
plot(data1$time, data1$A_NH4, type="l", col="red", ylim=c(0,125), xlab="time", ylab="NH4 mmol/m3", main="NH4 mmol/m3")
  points(data1$time, data1$O_NH4, type="l", col="blue")
  #legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#PO4 plot
plot(data1$time, data1$A_PO4, type="l", col="red", ylim=c(0,0.4), xlab="time", ylab="PO4 mmol/m3", main="PO4 mmol/m3")
  points(data1$time, data1$O_PO4, type="l", col="blue")
  #legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#TN plot
plot(data1$time, data1$A_totalN, type="l", col="red", ylim=c(0,130), xlab="time", ylab="TN mmol/m3", main="TN mmol/m3")
  points(data1$time, data1$O_totalN, type="l", col="blue")
  #legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#TP plot
plot(data1$time, data1$A_totalP, type="l", col="red", ylim=c(0,6), xlab="time", ylab="TP mmol/m3", main="TP mmol/m3")
  points(data1$time, data1$O_totalP, type="l", col="blue")
  #legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#TOC plot
plot(data1$time, data1$A_totalC, type="l", col="red", ylim=c(125,400), xlab="time", ylab="TOC mmol/m3", main="TOC mmol/m3")
  points(data1$time, data1$O_totalC, type="l", col="blue")
  #legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
dev.off()
  
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

#TN:TP plot
plot(data2$time, data2$A_TN_TP, type="l", col="red", ylim=c(0,250), xlab="time", ylab="TN:TP molar")
  points(data2$time, data2$O_TN_TP, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#DIN:DRP plot
data2 <- data2 %>% 
  filter(A_DIN_PO4<500, O_DIN_PO4<500)
plot(data2$time, data2$A_DIN_PO4, type="l", col="red", ylim=c(0,400), xlab="time", ylab="DIN:DRP molar")
  points(data2$time, data2$O_DIN_PO4, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#TOC:TN plot
  plot(data2$time, data2$A_TOC_TN, type="l", col="red", ylim=c(0,60), xlab="time", ylab="TOC:TN molar")
  points(data2$time, data2$O_TOC_TN, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#DOC:DIN plot
data2 <- data2 %>% 
    filter(A_DOC_DIN<1500, O_DOC_DIN<2000)
plot(data2$time, data2$A_DOC_DIN, type="l", col="red", ylim=c(0,2000), xlab="time", ylab="DOC:DIN molar")
  points(data2$time, data2$O_DOC_DIN, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#TOC:TP plot
plot(data2$time, data2$A_TOC_TP, type="l", col="red", ylim=c(0,2500), xlab="time", ylab="TOC:TP molar")
  points(data2$time, data2$O_TOC_TP, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#DOC:DRP plot
plot(data2$time, data2$A_DOC_PO4, type="l", col="red", ylim=c(0,8000), xlab="time", ylab="DOC:DRP molar")
  points(data2$time, data2$O_DOC_PO4, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
  
####make boxplots####

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
          O_DOC_PO4 = O_DOCall/O_PO4) %>%  
    mutate(year=year(time),
           DOY = yday(time)) %>% 
    filter(DOY < 275, DOY > 195)
  
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

#boxplots of median summer ratios
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

#looking at boxplots of raw (median) summerconcentrations
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
         DOY = yday(time)) 

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
            Fnet_O_NH4 = 100*(sum(O_NH4_output)-sum(inputNH4))/sum(inputNH4))
#Data indicate net flux (%) for each year; flux < 0 represents net retention and/or removal, 
  #flux > 0 represents net downstream export

#retention for summer period
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
            Fnet_O_NH4 = 100*(sum(O_NH4_output)-sum(inputNH4))/sum(inputNH4))
#Data indicate net flux (%) for each year; flux < 0 represents net retention and/or removal, 
#flux > 0 represents net downstream export


#first pick which retention period you want from above (full calendar year or just summer)
#looking at boxplots of downstream export between anoxic vs oxic scenarios
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


