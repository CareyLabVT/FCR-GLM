library(GLMr)
library(glmtools)
library(tidyverse)
library(lubridate)
library(ncdf4)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
#setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#look at glm and aed nml files
nml_file <- paste0(sim_folder,"/glm3.nml")
aed_file <- paste0(sim_folder,"/aed2/aed2_20210204_2DOCpools.nml")
nml <- read_nml(nml_file) 
aed <- read_nml(aed_file) #you may get a warning about an incomplete final line but it doesn't matter
print(nml)
print(aed)

vals<-c("Knitrif", "Kdenit", "Ksed_amm", "Ksed_nit", "Kanmx_nit","Kdnra_oxy",
        "Ksed_frp","Kpom_hydrol","Kdom_minerl","Ksed_dom")

params<-as.data.frame(matrix(NA, nrow = length(vals), ncol = 4))
colnames(params) <- c("K_Parameters","K_Value","Rates","Rate_Value")

for(i in 1:length(vals)){
  params[i,1] <- vals[i]
  params[i,2] <- get_nml_value(aed, paste0(vals[i]))
}

rates<-c("Rnitrif","Rdenit","Fsed_amm","Fsed_nit","Ranammox","Rdnra",
         "Fsed_frp","Rpom_hydrol","Rdom_minerl","Fsed_doc")

for(i in 1:length(rates)){
  params[i,3] <- rates[i]
  params[i,4] <- get_nml_value(aed, paste0(rates[i]))[1]
}



write.csv(params, "./output/Kvalues_AED20210204.csv", row.names = F)


