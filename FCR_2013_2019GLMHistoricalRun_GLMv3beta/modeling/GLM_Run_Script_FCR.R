#*****************************************************************                                                           *
#* TITLE:   Run GLM-AED for FCR
#* AUTHORS:  C.C. Carey                    
#* DATE:   Originally developed by CCC on 16 July 2018; 
#*         Last modified by CCC in 9 Sept 2021                            
#* NOTES:  CCC modified the original script in 2019 for FCR modeling, 
#*        with subsequent tweaks to annotation in summer 2021. 
#*        Code compares modeled output vs. observations for all focal
#*        variables, and calculates RMSE for each depth.
#*****************************************************************

remotes::install_github("CareyLabVT/GLMr", force = T)
remotes::install_github("CareyLabVT/glmtools", force = T)

# Load packages, set sim folder, load nml file ####
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#look at glm and aed nml files
nml_file <- paste0(sim_folder,"/glm3.nml")
aed_file <- paste0(sim_folder,"/aed2/aed2_20210204_2DOCpools.nml")
aed_phytos_file <- paste0(sim_folder,"/aed2/aed2_phyto_pars_30June2020.nml")
nml <- read_nml(nml_file) 
aed <- read_nml(aed_file) #you may get a warning about an incomplete final line but it doesn't matter
aed_phytos <- read_nml(aed_phytos_file)
print(nml)
print(aed)
print(aed_phytos)

##### run the model! #######
system2(paste0(sim_folder, "/", "glm"), stdout = TRUE, stderr = TRUE, env = paste0("DYLD_LIBRARY_PATH=",sim_folder))
#sometimes, you'll get an error that says "Error in file, 'Time(Date)' is not first column!
#in this case, open the input file in Excel, set the column in Custom ("YYYY-MM-DD") format, resave, and close the file
nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 

#reality check of temp heat map
plot_temp(nc_file, col_lim = c(0,30))

#get water level
water_level<-get_surface_height(nc_file, ice.rm = TRUE, snow.rm = TRUE)
plot(water_level$DateTime,water_level$surface_height)

#get WRT
volume<-get_var(nc_file, "Tot_V", reference="surface") #in m3
evap<-get_var(nc_file, "evap", reference="surface")
precip<-get_var(nc_file, "precip", reference="surface")
colnames(volume)[1]<-"time"
colnames(precip)[1]<-"time"
colnames(evap)[1]<-"time"
plot(volume$time, volume$Tot_V)
plot(evap$time, evap$evap)
plot(precip$time, precip$precip)

outflow<-read.csv("inputs/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv", header=T)
inflow_weir<-read.csv("inputs/FCR_weir_inflow_2013_2019_20200828_allfractions_2poolsDOC.csv", header=T)
inflow_wetland<-read.csv("inputs/FCR_wetland_inflow_2013_2019_20200828_allfractions_2DOCpools.csv", header=T)
outflow$time<-as.POSIXct(strptime(outflow$time, "%Y-%m-%d", tz="EST"))
inflow_weir$time<-as.POSIXct(strptime(inflow_weir$time, "%Y-%m-%d", tz="EST"))
inflow_wetland$time<-as.POSIXct(strptime(inflow_wetland$time, "%Y-%m-%d", tz="EST"))

plot(inflow_weir$time,inflow_weir$FLOW)
lines(inflow_wetland$time, inflow_wetland$FLOW, col="red")
sum(inflow_weir$FLOW)/(sum(inflow_weir$FLOW) + sum(inflow_wetland$FLOW))#proportion of wetland:weir inflows over time

volume$time<-as.POSIXct(strptime(volume$time, "%Y-%m-%d", tz="EST"))
wrt<-merge(volume, outflow, by='time')
wrt$wrt <- ((wrt$Tot_V)/(wrt$FLOW))*(1/60)*(1/60)*(1/24) #residence time in days
plot(wrt$time,wrt$wrt)

hist(wrt$wrt)
median(wrt$wrt)
mean(wrt$wrt)
range(wrt$wrt)

#get ice
ice<-get_var(nc_file,"hwice")
iceblue<-get_var(nc_file,"hice")
plot(ice$DateTime,rowSums(cbind(ice$hwice,iceblue$hice)))

############## temperature data #######
#read in cleaned CTD temp file with long-term obs at focal depths
obstemp<-read_csv('field_data/CleanedObsTemp.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

#get modeled temperature readings for focal depths
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
modtemp <- get_temp(nc_file, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

#lets do depth by depth comparisons of the obs vs mod temps for each focal depth
watertemp<-merge(modtemp, obstemp, by=c("DateTime","Depth")) %>%
  rename(modtemp = temp.x, obstemp = temp.y)
for(i in 1:length(unique(watertemp$Depth))){
  tempdf<-subset(watertemp, watertemp$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$obstemp, type='p', col='red',
       ylab='temperature', xlab='time',
       main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]),ylim=c(0,30))
       points(tempdf$DateTime, tempdf$modtemp, type="l",col='black')
}

#thermocline depth comparison
field_file<-file.path(sim_folder,'/field_data/CleanedObsTemp.csv')
therm_depths <- compare_to_field(nc_file, field_file, metric="thermo.depth", precision="days",method='interp',as_value=TRUE, na.rm=T)
compare_to_field(nc_file, field_file, metric="thermo.depth", precision="days", method='interp',as_value=F, na.rm=TRUE) #prints RMSE
plot(therm_depths$DateTime,therm_depths$mod, type="l", ylim=c(1,9),main = paste0("ThermoclineDepth: Obs=Red, Mod=Black"),
     ylab="Thermocline depth, in m")
points(therm_depths$DateTime, therm_depths$obs, type="l",col="red")

#Run sim diagnostics and calculate RMSE using glmtools
field_file<-file.path(sim_folder,'/field_data/CleanedObsTemp.csv')
compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'hypo.temperature', as_value = FALSE,
                 na.rm = TRUE, precision = 'days',method = 'interp')
compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'epi.temperature', as_value = FALSE,
                 na.rm = TRUE, precision = 'days',method = 'interp')
compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'thermo.depth', as_value = FALSE,
                 na.rm = TRUE, precision = 'days',method = 'interp')
compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'water.temperature', as_value = FALSE,
                 na.rm = TRUE, precision = 'days',method = 'interp')#raw temp, assuming each depth is treated equally
compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'whole.lake.temperature', as_value = FALSE,
                 na.rm = TRUE, precision = 'days',method = 'interp')#volume-weighted temp
compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'schmidt.stability', as_value = FALSE,
                 na.rm = TRUE, precision = 'days',method = 'interp')

#can use this function to calculate RMSE at specific depth layers, e.g., from one depth or range of depths
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

field_file<-file.path(sim_folder,'/field_data/CleanedObsTemp.csv')
temps <- resample_to_field(nc_file, field_file, precision="mins", method='interp')
temps<-temps[complete.cases(temps),]

m_temp <- temps$Modeled_temp[temps$Depth==c(1)] #1m depth (epi) RMSE
o_temp <- temps$Observed_temp[temps$Depth==c(1)] 
RMSE(m_temp,o_temp)

m_temp <- temps$Modeled_temp[temps$Depth==c(9)] #9m depth (hypo) RMSE
o_temp <- temps$Observed_temp[temps$Depth==c(9)] 
RMSE(m_temp,o_temp)

m_temp <- temps$Modeled_temp[temps$Depth==c(5)] #5m depth (meta) RMSE
o_temp <- temps$Observed_temp[temps$Depth==c(5)] 
RMSE(m_temp,o_temp)

m_temp <- temps$Modeled_temp[temps$Depth>=0 & temps$Depth<=9.3] #depths from 0.1-9m (all depths)
o_temp <- temps$Observed_temp[temps$Depth>=0 & temps$Depth<=9.3] 
RMSE(m_temp,o_temp)


############## oxygen data #######

#read in cleaned CTD temp file with long-term obs at focal depths
var="OXY_oxy"
obs_oxy<-read.csv('field_data/CleanedObsOxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))
field_file <- file.path(sim_folder,'/field_data/CleanedObsOxy.csv') 
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.2) 
#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,600)) #compare obs vs modeled

#get modeled oxygen concentrations for focal depths
mod_oxy <- get_var(nc_file, "OXY_oxy", reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

#lets do depth by depth comparisons of modeled vs. observed oxygen
oxy_compare <- merge(mod_oxy, obs_oxy, by=c("DateTime","Depth")) %>%
  rename(mod_oxy = OXY_oxy.x, obs_oxy = OXY_oxy.y)
for(i in 1:length(unique(oxy_compare$Depth))){
  tempdf<-subset(oxy_compare, oxy_compare$Depth==depths[i])
  plot(tempdf$DateTime,tempdf$obs_oxy, type='p', col='red',
       ylab='Oxygen mmol/m3', xlab='time',
       main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]),ylim=c(0,600))
  points(tempdf$DateTime, tempdf$mod_oxy, type="l",col='black')
}


#can use this function to calculate RMSE at specific depth layers, e.g., from one depth or range of depths
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#calculate RMSE for oxygen
oxygen <- resample_to_field(nc_file, field_file, precision="days", method='interp', 
                            var_name="OXY_oxy")
oxygen <-oxygen[complete.cases(oxygen),] #remove missing data

m_oxygen <- oxygen$Modeled_OXY_oxy[oxygen$Depth>=1 & oxygen$Depth<=1] #1m depth
o_oxygen <- oxygen$Observed_OXY_oxy[oxygen$Depth>=1 & oxygen$Depth<=1] 
RMSE(m_oxygen,o_oxygen)

m_oxygen <- oxygen$Modeled_OXY_oxy[oxygen$Depth>=9 & oxygen$Depth<=9] #9m depth
o_oxygen <- oxygen$Observed_OXY_oxy[oxygen$Depth>=9 & oxygen$Depth<=9] 
RMSE(m_oxygen,o_oxygen)

m_oxygen <- oxygen$Modeled_OXY_oxy[oxygen$Depth>=5 & oxygen$Depth<=5] #5 m depth
o_oxygen <- oxygen$Observed_OXY_oxy[oxygen$Depth>=5 & oxygen$Depth<=5] 
RMSE(m_oxygen,o_oxygen)

m_oxygen <- oxygen$Modeled_OXY_oxy[oxygen$Depth>=0 & oxygen$Depth<=9.3] #all depths
o_oxygen <- oxygen$Observed_OXY_oxy[oxygen$Depth>=0 & oxygen$Depth<=9.3] 
RMSE(m_oxygen,o_oxygen)

mod_oxy9 <- get_var(nc_file, "OXY_oxy", reference="surface", z_out=c(9.2)) 
plot(mod_oxy9$DateTime, mod_oxy9$OXY_oxy_9.2, type="l")
#diagnostic plot of DO at 9.2 m (just above sediments)



#### dissolved inorganic carbon (DIC) data ###########
var="CAR_dic"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 
#note: DIC is tricky because there are no observational data between 2013-2017, so 
# this script has only one year of data during the model calibration period

obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  group_by(DateTime, Depth) %>%
  summarise(CAR_dic = mean(CAR_dic)) %>%
  na.omit() 
obs<-as.data.frame(obs)
#write.csv(obs, "field_data/field_DIC.csv", row.names =F)

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,1000)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- as.numeric(unique(obs$Depth))
mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth = as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))

compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf>1)){
  plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='l', col='red',
       ylab=var, xlab='time',
       main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]), ylim=c(0,1000))
  points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}  

#calculate RMSE 
newdata <- resample_to_field(nc_file, field_file, precision="mins", method='interp', 
                         var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] #depths from 6-9m
obs <- newdata$Observed_CAR_dic[newdata$Depth>=0.1 & newdata$Depth<=0.1] #depths from 6-9m
RMSE(m_DIC,o_DIC)

m_DIC <- DIC$Modeled_CAR_dic[DIC$Depth>=9 & DIC$Depth<=9] #depths from 6-9m
o_DIC <- DIC$Observed_CAR_dic[DIC$Depth>=9 & DIC$Depth<=9] #depths from 6-9m
RMSE(m_DIC,o_DIC)

m_DIC <- DIC$Modeled_CAR_dic[DIC$Depth>=5 & DIC$Depth<=5] #depths from 6-9m
o_DIC <- DIC$Observed_CAR_dic[DIC$Depth>=5 & DIC$Depth<=5] #depths from 6-9m
RMSE(m_DIC,o_DIC)

m_DIC <- DIC$Modeled_CAR_dic[DIC$Depth>=0 & DIC$Depth<=9.3] #depths from 6-9m
o_DIC <- DIC$Observed_CAR_dic[DIC$Depth>=0 & DIC$Depth<=9.3] #depths from 6-9m
RMSE(m_DIC,o_DIC)



#### dissolved methane data #######
var="CAR_ch4"
field_file <- file.path(sim_folder,'/field_data/field_gases.csv') 

obs<-read.csv('field_data/field_gases.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,50)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
  plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='l', col='red',
       ylab=var, xlab='time',
       main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
  points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE
newdata <- resample_to_field(nc_file, field_file, precision="mins", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)



##### silica #######

var="SIL_rsi"
field_file <- file.path(sim_folder,'/field_data/field_silica.csv') 

obs<-read.csv('field_data/field_silica.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,1000)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='l', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE
newdata <- resample_to_field(nc_file, field_file, precision="mins", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)



######## ammonium #######

var="NIT_amm"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 

obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,1000)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='l', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)


###### nitrate #########################################

var="NIT_nit"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 

obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,1000)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='l', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)



#### phosphate ########################################

var="PHS_frp"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 

obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,1)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='l', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE 
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)




#### dissolved organic carbon: recalcitrant ###########

var="OGM_docr"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 

obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,300)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='o', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)



#### dissolved organic carbon: labile ###########

var="OGM_doc"
field_file <- file.path(sim_folder,'/field_data/field_chem.csv') 

obs<-read.csv('field_data/field_chem.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,400)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE for DOC
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)


#### total nitrogen ###########

var="TOT_tn"
field_file <- file.path(sim_folder,'/field_data/totalNP.csv') 
cyanoNcon = 0.12
greenNcon = 0.12
diatomNcon = 0.12

obs<-read.csv('field_data/totalNP.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,400)) #compare obs vs modeled

#now to include the phyto NP chemistry at 9m!
cyano <- get_var(file=nc_file,var_name = 'PHY_cyano',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_cyano_5, PHY_cyano_6.2:PHY_cyano_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoN = CyanoConc*cyanoNcon) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoN)

green <- get_var(file=nc_file,var_name = 'PHY_green',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_green_5, PHY_green_6.2:PHY_green_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenN = GreenConc*greenNcon) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenN)

diatom <- get_var(file=nc_file,var_name = 'PHY_diatom',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_diatom_5, PHY_diatom_6.2:PHY_diatom_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomN = DiatomConc*diatomNcon) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,diatomN)

phytos<-cbind.data.frame(cyano, green, diatom) 
phytos<-phytos[-c(4,5,7,8)] %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  mutate(summedN = cyanoN + greenN + diatomN) %>% 
  mutate(Depth=as.numeric(Depth)) %>%
  select(DateTime, Depth, summedN)

#get modeled concentrations for focal depths
depths<- sort(unique(phytos$Depth))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

allmod<-merge(mod, phytos, by=c("DateTime","Depth")) %>% 
  mutate(TOT_tn=TOT_tn + summedN) %>% 
  select(DateTime,Depth,TOT_tn)
#write.csv(allmod,"output/SummedwPhytos_allTNModeled.csv", row.names=F)

#lets do depth by depth comparisons of the sims
compare<-merge(allmod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#if you want phytos to be part of the N calculation for totals
field_file <- file.path(sim_folder,"output/SummedwPhytos_allTNModeled.csv")

#calculate RMSE for TN
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)


#### total phosphorus ###########

var="TOT_tp"
field_file <- file.path(sim_folder,'/field_data/totalNP.csv') 
cyanoPcon = 0.0005
greenPcon = 0.0005
diatomPcon = 0.0005

obs<-read.csv('field_data/totalNP.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()
depths<- sort(as.numeric(unique(obs$Depth)))
#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,400)) #compare obs vs modeled

#now to include the phyto NP chemistry at 9m!
cyano <- get_var(file=nc_file,var_name = 'PHY_cyano',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_cyano_5, PHY_cyano_6.2:PHY_cyano_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoP = CyanoConc*cyanoPcon) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoP)

green <- get_var(file=nc_file,var_name = 'PHY_green',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_green_5, PHY_green_6.2:PHY_green_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenP = GreenConc*greenPcon) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenP)

diatom <- get_var(file=nc_file,var_name = 'PHY_diatom',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_diatom_5, PHY_diatom_6.2:PHY_diatom_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomP = DiatomConc*diatomPcon) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,diatomP)

phytos<-cbind.data.frame(cyano, green, diatom) 
phytos<-phytos[-c(4,5,7,8)] %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  mutate(summedP = cyanoP + greenP + diatomP) %>% 
  mutate(Depth=as.numeric(Depth)) %>%
  select(DateTime, Depth, summedP)

#get modeled concentrations for focal depths
depths<- sort(unique(phytos$Depth))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

allmod<-merge(mod, phytos, by=c("DateTime","Depth")) %>% 
  mutate(TOT_tp=TOT_tp + summedP) %>% 
  select(DateTime,Depth,TOT_tp)
#write.csv(allmod,"output/SummedwPhytos_allTPModeled.csv", row.names=F)

#lets do depth by depth comparisons of the sims
compare<-merge(allmod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#if you want phytos to be part of the P calculation for totals
field_file <- file.path(sim_folder,"output/SummedwPhytos_allTPModeled.csv")

#calculate RMSE for TP
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)



#### total organic carbon ###########

var="TOT_toc"
depths<- c(0.1, 1.6, 2.8, 3.8, 5, 6.2, 8, 9)

#now to include the phyto NP chemistry at 9m!
cyano <- get_var(file=nc_file,var_name = 'PHY_cyano',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_cyano_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_cyano_")), names_to="Depth", names_prefix="PHY_cyano_",values_to = "CyanoConc") %>%
  mutate(cyanoC = CyanoConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,cyanoC)

green <- get_var(file=nc_file,var_name = 'PHY_green',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_green_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_green_")), names_to="Depth", names_prefix="PHY_green_",values_to = "GreenConc") %>%
  mutate(greenC = GreenConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,greenC)

diatom <- get_var(file=nc_file,var_name = 'PHY_diatom',z_out=depths,reference = 'surface') %>% 
  select(DateTime:PHY_diatom_9) %>% 
  pivot_longer(cols=starts_with(paste0("PHY_diatom_")), names_to="Depth", names_prefix="PHY_diatom_",values_to = "DiatomConc") %>%
  mutate(diatomC = DiatomConc) %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  select(DateTime,Depth,diatomC)

phytos<-cbind.data.frame(cyano, green, diatom) 
phytos<-phytos[-c(4,5,7,8)] %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  mutate(summedC = cyanoC + greenC + diatomC) %>% 
  mutate(Depth=as.numeric(Depth)) %>%
  select(DateTime, Depth, summedC)

#get modeled concentrations for focal depths
depths<- sort(unique(phytos$Depth))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

allmod<-merge(mod, phytos, by=c("DateTime","Depth")) %>% 
  mutate(TOT_toc=TOT_toc + summedC) %>% 
  select(DateTime,Depth,TOT_toc)
#write.csv(allmod,"output/SummedwPhytos_allTOCModeled.csv", row.names=F)

#lets do depth by depth comparisons of the sims

for(i in 1:length(depths)){
  tempdf<-subset(allmod, allmod$Depth==depths[i])
    plot(tempdf$DateTime,tempdf$TOT_toc, type='l', col='black',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
}


#### chlorophyll a #######

var="PHY_TCHLA"
field_file <- file.path(sim_folder,'/field_data/CleanedObsChla.csv') 

obs<-read.csv('field_data/CleanedObsChla.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,50)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='l', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=0.1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=9 & newdata$Depth<=9] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=5 & newdata$Depth<=5] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=0.1 & newdata$Depth<=9.3] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=1 & newdata$Depth<=1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=1 & newdata$Depth<=1] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=2 & newdata$Depth<=2] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=2 & newdata$Depth<=2] 
RMSE(mod,obs)

r2 <-lm(mod ~ obs)
summary(r2)
#plot individual phyto groups
#plot_var(file=nc_file,"PHY_TCHLA",reference="surface", col_lim=c(0,30))
#plot_var(file=nc_file,"PHY_CYANOPCH1",reference="surface", col_lim=c(0,50))
#plot_var(file=nc_file,"PHY_CYANONPCH2",reference="surface", col_lim=c(0,50))
#plot_var(file=nc_file,"PHY_CHLOROPCH3",reference="surface", col_lim=c(0,50))
#plot_var(file=nc_file,"PHY_DIATOMPCH4",reference="surface", col_lim=c(0,50))

cyano <- get_var(file=nc_file,var_name = 'PHY_cyano',z_out=1.0,reference = 'surface') 
plot(cyano$DateTime, cyano$PHY_cyano_1, col="cyan", type="l", ylab="Phyto C mmol/m3", ylim=c(0,30))
green <- get_var(file=nc_file,var_name = 'PHY_green',z_out=1.0,reference = 'surface') 
lines(green$DateTime, green$PHY_green_1, col="green")
diatoms <- get_var(file=nc_file,var_name = 'PHY_diatom',z_out=1.0,reference = 'surface') 
lines(diatoms$DateTime, diatoms$PHY_diatom_1, col="brown")
legend("topleft", legend=c("Cyano", "Greens", "Diatoms"), fill= c("cyan", "green","brown"), cex=0.8)
chla <- get_var(file=nc_file,var_name = 'PHY_TCHLA',z_out=1.0,reference = 'surface') 
lines(chla$DateTime, chla$PHY_TCHLA_1, col="red")


plot_var(nc_file, "PHY_cyano_fNit")
plot_var(nc_file, "PHY_cyano_fPho")
plot_var(nc_file, "PHY_cyano_NtoP")
plot_var(nc_file, "PHY_cyano_fT")
plot_var(nc_file, "PHY_cyano_fI")

plot_var(nc_file, "PHY_diatom_fNit")
plot_var(nc_file, "PHY_diatom_fPho")
plot_var(nc_file, "PHY_diatom_NtoP")
plot_var(nc_file, "PHY_diatom_fSil")
plot_var(nc_file, "PHY_diatom_fT")
plot_var(nc_file, "PHY_diatom_fI")

plot_var(nc_file, "PHY_green_fNit")
plot_var(nc_file, "PHY_green_fPho")
plot_var(nc_file, "PHY_green_NtoP")
plot_var(nc_file, "PHY_green_fT")
plot_var(nc_file, "PHY_green_fI")



phytos <- get_var(file=nc_file,var_name = 'PHY_AGGREGATE',z_out=0.1,reference = 'surface') 
plot(phytos$DateTime, phytos$PHY_AGGREGATE_0.1, col="cyan", type="l", ylab="Phyto C mmol/m3", ylim=c(0,30))

cyano1 <- get_var(file=nc_file,var_name = 'PHY_CYANOPCH1',z_out=0.1,reference = 'surface') 
plot(cyano1$DateTime, cyano1$PHY_CYANOPCH1_0.1, col="cyan", type="l", ylab="Phyto C mmol/m3", ylim=c(0,30))
cyano2 <- get_var(file=nc_file,var_name = 'PHY_CYANONPCH2',z_out=0.1,reference = 'surface') 
lines(cyano2$DateTime, cyano2$PHY_CYANONPCH2_0.1, col="blue")
greens <- get_var(file=nc_file,var_name = 'PHY_CHLOROPCH3',z_out=0.1,reference = 'surface') 
lines(greens$DateTime, greens$PHY_CHLOROPCH3_0.1, col="green")
diatoms <- get_var(file=nc_file,var_name = 'PHY_DIATOMPCH4',z_out=0.1,reference = 'surface') 
lines(diatoms$DateTime, diatoms$PHY_DIATOMPCH4_0.1, col="brown")
legend("topleft", legend=c("Cyano1", "Cyano2","Greens", "Diatoms"), fill= c("cyan", "blue","green","brown"), cex=0.8)
chla <- get_var(file=nc_file,var_name = 'PHY_TCHLA',z_out=0.1,reference = 'surface') 
lines(chla$DateTime, chla$PHY_TCHLA_0.1, col="red")

phytos <- get_var(file=nc_file,var_name = 'PHY_AGGREGATE',z_out=0.1,reference = 'surface') 
plot(phytos$DateTime, phytos$PHY_AGGREGATE_0.1, col="cyan", type="l", ylab="Phyto C mmol/m3", ylim=c(0,30))


#######################################################
#### ancillary code #######

#plot Secchi depth & light extinction
lec <- get_var(file=nc_file,var_name = 'extc_coef',z_out=1,reference = 'surface')
plot(lec$DateTime, 1.7/lec$extc_coef_1)
plot(lec$DateTime, lec$extc_coef_1)

#see what vars are available for diagnostics
sim_metrics(with_nml = TRUE)

#plot pH
pH <- get_var(file=nc_file,var_name = 'CAR_pH',z_out=0.1,reference = 'surface') 
plot(pH$DateTime, pH$CAR_pH_0.1)

#plot to make SSS oxygen addition 
inflow<-read.csv("FCR_SSS_inflow_2013_2017_20181001.csv", header=TRUE)
inflow$time<-as.POSIXct(strptime(inflow$time, "%Y-%m-%d", tz="EST")) 
par(mar = c(5,5,2,5))
plot(inflow$time,inflow$OXY_oxy, type='l', ylab="BLACK: Oxygen added by SSS (mmol/m3 added every day)",lwd=2)
par(new = T)
with(inflow, plot(time, FLOW, axes=F, xlab=NA, ylab=NA, type='l', col="red", lwd=1, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'RED: Flow rate of SSS (m3/day)')


#Particulate organic P
pop9 <- get_var(file=nc_file,var_name = 'OGM_pop',z_out=9,reference = 'surface') 
plot(pop9$DateTime, pop9$OGM_pop_9, ylim=c(0,0.5))

#get CH4 atmospheric flux
get_var(file=nc_file,var_name = 'CAR_atm_ch4_flux',reference = "bottom",z_out = 1) #, z_out=1, reference='surface')
nc<-nc_open("output/output.nc")
names(nc$var)#get list of variables in data
names(nc$dim)#get list of variables in data
time <- ncvar_get(nc, 'time')
time_1 <- as.POSIXct('2013-05-15 00:00:00' , tz = 'EST') + time*60*60
ch4flux<-t(as.data.frame(ncvar_get(nc,varid="CAR_atm_ch4_flux")))
data<-as.data.frame(cbind(time_1,ch4flux[,1]))
data$time_1<-as.POSIXct(strptime(data$time_1, "%Y-%m-%d", tz="EST"))
plot(time_1,ch4flux[,1], ylim=c(0,2.5), ylab="CH4 flux, mmol/m2/d", xlab="Date")
points(time_1,(ch4flux[,1]+sample(rnorm(1,mean=0.3,sd=0.3))), col="red")
legend("topleft", c("Baseline","+2 degrees"), fill=c("black","red"))
nc_close(nc)
