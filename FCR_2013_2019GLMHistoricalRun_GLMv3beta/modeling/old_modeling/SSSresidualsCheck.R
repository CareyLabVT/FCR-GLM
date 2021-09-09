# Load packages, set sim folder, load nml file ####
library(GLMr)
library(glmtools)
library(tidyverse)
library(lubridate)
library(ncdf4)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#look at glm and aed nml files
nml_file <- paste0(sim_folder,"/glm3.nml")
aed_file <- paste0(sim_folder,"/aed2/aed2_20200701_2DOCpools.nml")
aed_phytos_file <- paste0(sim_folder,"/aed2/aed2_phyto_pars_30June2020.nml")
nml <- read_nml(nml_file) 
aed <- read_nml(aed_file) #you may get a warning about an incomplete final line but it doesn't matter
aed_phytos <- read_nml(aed_phytos_file)
print(nml)
print(aed)
print(aed_phytos)

#########run the model!
system2(paste0(sim_folder, "/", "glm"), stdout = TRUE, stderr = TRUE, env = paste0("DYLD_LIBRARY_PATH=",sim_folder))
#sometimes, you'll get an error that says "Error in file, 'Time(Date)' is not first column!
#in this case, open the input file in Excel, set the column in Custom ("YYYY-MM-DD") format, resave, and close the file
nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 
#########

SSS <- read.csv("./inputs/HOx_Operations_20190916.csv") 
SSS$time<-as.POSIXct(strptime(SSS$time, "%Y-%m-%d", tz="EST"))

oxygen <- resample_to_field(nc_file, field_file, precision="days", method='interp', 
                            var_name="OXY_oxy")
oxygen1 <-oxygen[complete.cases(oxygen),] %>% #remove missing data
 # filter(DateTime > "2019-04-29") %>% 
# filter(DateTime < "2019-10-01") %>% 
  filter(Depth == 9) %>% 
  mutate(residual = Observed_OXY_oxy - Modeled_OXY_oxy) %>% 
  rename(time = DateTime)

data <- merge(oxygen1, SSS, by="time") %>% 
  select(time:residual,mmol.O2.m3.day) %>% 
  mutate(year = year(time))

for(i in 1:length(unique(data$year))){
  temp<-subset(data,year==unique(data$year)[i])
  reg<-lm(temp$residual~temp$mmol.O2.m3.day)
  plot(temp$mmol.O2.m3.day,temp$residual, main=paste0(unique(data$year)[i],", slope=",coefficients(reg)[2])) 
  abline(reg, col="red")
  print(i)
}

data1<-data %>% 
  filter(year==2019)

plot(data1$time, data1$residual, main="2019 resids Observed-Modeled DO")  
points(data$time, data$mmol.O2.m3.day, col="red")

plot(data$mmol.O2.m3.day,data$residual)  
  




