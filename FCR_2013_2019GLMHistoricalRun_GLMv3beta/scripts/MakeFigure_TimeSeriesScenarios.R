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

#####baseline scenario####
#based on observed SSS practices
#nc_file <- file.path(sim_folder, 'output/output_2013_2019.nc')

#####anoxic scenario#####
#no SSS activation throughout
anoxic <- file.path(sim_folder, 'output/output_anoxic.nc')

#####oxic scenario######
#SSS on in summer May 15-Nov 15 at full level
oxic <- file.path(sim_folder, 'output/output_oxic.nc')

#####pull out deep-water chemistry
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
  select(time:A_TOC,O_oxy:O_TOC,A_DOCall:O_totalC)

#oxygen plot
plot(data1$time, data1$A_oxy, type="l", col="red", ylim=c(0,800), xlab="time", ylab="DO mmol/m3")
  points(data$time, data1$O_oxy, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#DOC plot
plot(data1$time, data1$A_DOCall, type="l", col="red", ylim=c(130,360), xlab="time", ylab="DOC mmol/m3")
  points(data$time, data1$O_DOCall, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#NO3 plot
plot(data1$time, data1$A_NO3, type="l", col="red", ylim=c(0,1.2), xlab="time", ylab="NO3 mmol/m3")
  points(data$time, data1$O_NO3, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#NH4 plot
plot(data1$time, data1$A_NH4, type="l", col="red", ylim=c(0,125), xlab="time", ylab="NH4 mmol/m3")
  points(data$time, data1$O_NH4, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#PO4 plot
plot(data1$time, data1$A_PO4, type="l", col="red", ylim=c(0,0.4), xlab="time", ylab="PO4 mmol/m3")
  points(data$time, data1$O_PO4, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#TN plot
plot(data1$time, data1$A_totalN, type="l", col="red", ylim=c(0,130), xlab="time", ylab="TN mmol/m3")
  points(data$time, data1$O_totalN, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))

#TP plot
plot(data1$time, data1$A_totalP, type="l", col="red", ylim=c(0,6), xlab="time", ylab="TP mmol/m3")
  points(data$time, data1$O_totalP, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
#TOC plot
plot(data1$time, data1$A_totalC, type="l", col="red", ylim=c(125,400), xlab="time", ylab="TOC mmol/m3")
  points(data$time, data1$O_totalC, type="l", col="blue")
  legend("topleft", c("Anoxic", "Oxic"), lwd=2,col=c("red", "blue"))
  
  
    