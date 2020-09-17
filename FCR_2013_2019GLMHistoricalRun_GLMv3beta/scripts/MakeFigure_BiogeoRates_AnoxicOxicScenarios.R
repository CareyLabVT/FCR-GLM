#look at biogeochemical rates between anoxic/oxic scenarios

#load packages
library(zoo)
library(tidyverse)
library(lubridate)
library(ncdf4)

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
#pull out deep-water rates from each output file
A_anammox <- get_var(anoxic,'NIT_anammox',z_out=9,reference = 'surface') 
O_anammox <- get_var(oxic,'NIT_anammox',z_out=9,reference = 'surface') 

plot(A_anammox$DateTime, A_anammox$NIT_anammox_9)
plot(O_anammox$DateTime, O_anammox$NIT_anammox_9)

A_denit<- get_var(anoxic,'NIT_denit',z_out=9,reference = 'surface') 
O_denit <- get_var(oxic,'NIT_denit',z_out=9,reference = 'surface') 

plot(A_denit$DateTime, A_denit$NIT_denit_9)
plot(O_denit$DateTime, O_denit$NIT_denit_9)

A_dnra<- get_var(anoxic,'NIT_dnra',z_out=9,reference = 'surface') 
O_dnra <- get_var(oxic,'NIT_dnra',z_out=9,reference = 'surface') 

plot(A_dnra$DateTime, A_dnra$NIT_dnra_9)
plot(O_dnra$DateTime, O_dnra$NIT_dnra_9)

A_nitrif<- get_var(anoxic,'NIT_nitrif',z_out=9,reference = 'surface') 
O_nitrif <- get_var(oxic,'NIT_nitrif',z_out=9,reference = 'surface') 

plot(A_nitrif$DateTime, A_nitrif$NIT_nitrif_9)
plot(O_nitrif$DateTime, O_nitrif$NIT_nitrif_9)

A_sed_cpom<- get_var(anoxic,'OGM_Psed_cpom',z_out=9,reference = 'surface') 
O_sed_cpom <- get_var(oxic,'OGM_Psed_cpom',z_out=9,reference = 'surface') 

plot(A_sed_cpom$DateTime, A_sed_cpom$OGM_Psed_cpom_9)
plot(O_sed_cpom$DateTime, O_sed_cpom$OGM_Psed_cpom_9)

A_sed_poc<- get_var(anoxic,'OGM_Psed_poc',z_out=9,reference = 'surface') 
O_sed_poc <- get_var(oxic,'OGM_Psed_poc',z_out=9,reference = 'surface') 

plot(A_sed_poc$DateTime, A_sed_poc$OGM_Psed_poc_9)
plot(O_sed_poc$DateTime, O_sed_poc$OGM_Psed_poc_9)

A_sed_phy<- get_var(anoxic,'PHY_Psed_phy',z_out=9,reference = 'surface') 
O_sed_phy <- get_var(oxic,'PHY_Psed_phy',z_out=9,reference = 'surface') 

plot(A_sed_phy$DateTime, A_sed_phy$PHY_Psed_phy_9)
plot(O_sed_phy$DateTime, O_sed_phy$PHY_Psed_phy_9)


#### open up the sediment flux rates ####

nc_a <- ncdf4::nc_open(anoxic)

A_sed_oxy<- ncdf4::ncvar_get(nc_a,'OXY_sed_oxy') 
A_SDF_sed_oxy<- ncdf4::ncvar_get(nc_a,'SDF_Fsed_oxy') #first value = hypo, second value = epi???

A_sed_frp <- ncdf4::ncvar_get(nc_a,'PHS_sed_frp')
A_SDF_sed_frp <- ncdf4::ncvar_get(nc_a,'SDF_Fsed_frp')

A_sed_amm<- ncdf4::ncvar_get(nc_a,'NIT_Fsed_amm') #first value = hypo, second value = epi
A_SDF_sed_amm <- ncdf4::ncvar_get(nc_a,'SDF_Fsed_amm')

A_sed_nit<- ncdf4::ncvar_get(nc_a,'SDF_Fsed_nit') #first value = hypo, second value = epi
A_SDF_sed_nit <- ncdf4::ncvar_get(nc_a,'SDF_Fsed_nit')

ncdf4::nc_close(nc_a)
names(nc_a$var)#get list of variables in data
names(nc_a$dim)

#lets make plots
plot(A_sed_oxy[1,]) #there is some pattern here
plot(A_sed_oxy[2,]) #nearly identical to the first row
plot(A_sed_oxy[1,], A_sed_oxy[2,]) #why are these slightly different?
plot(A_SDF_sed_oxy[1,], A_SDF_sed_oxy[2,]) #these 2 rows are mostly identical
plot(A_SDF_sed_oxy[1,]) #why is this positive? what is the first row of data vs 2nd?
points(A_SDF_sed_oxy[2,], col="red") #mostly identical values 
#what is the difference between 'OXY_sed_oxy' and 'SDF_Fsed_oxy'?
#why is flux positive? (shouldn't it be negative?)


plot(A_sed_frp[1,])#all set to 1
plot(A_sed_frp[2,])#all set to 1
plot(A_sed_frp[2,],A_sed_frp[1,]) #identical; all values =1
plot(A_SDF_sed_frp[1,])
points(A_SDF_sed_frp[2,], col="red")#what is difference between the 1st and 2nd row, and why does this vary every so slightly?
#all togehter not too concerning though?

plot(A_sed_amm[1,])#mostly values at 0 and then some intermittent large negative values at -15
plot(A_sed_amm[2,])#positive very small values, but only at end of time series
plot(A_sed_amm[2,],A_sed_amm[1,]) #values aren't identical
plot(A_SDF_sed_amm[1,])
points(A_SDF_sed_amm[2,], col="red")#what is difference between the 1st and 2nd row?
#why is FRP flux positive but NH4 negative? #why is NH4 positive for one row of the output but not the other?

plot(A_sed_nit[1,])#pattern looks mostly good?
plot(A_sed_nit[2,])#mostly the same, minus 
plot(A_sed_nit[2,],A_sed_nit[1,]) #values aren't identical
plot(A_SDF_sed_nit[1,]) #this is identical to A_sed_nit- why????
points(A_SDF_sed_nit[2,], col="red")#these are 
#why is SDF_sed_nit and sed_nit identical but none of the other variables show this pattern?


#SDF_Fsed_nit
#SDF_Fsed_ch4

###############
#oxic one
nc_o <- ncdf4::nc_open(oxic)

O_sed_oxy<- ncdf4::ncvar_get(nc_o,'OXY_sed_oxy') 
O_SDF_sed_oxy<- ncdf4::ncvar_get(nc_o,'SDF_Fsed_oxy') #first value = hypo, second value = epi???

O_sed_frp <- ncdf4::ncvar_get(nc_o,'PHS_sed_frp')
O_SDF_sed_frp <- ncdf4::ncvar_get(nc_o,'SDF_Fsed_frp')

O_sed_amm<- ncdf4::ncvar_get(nc_o,'SDF_Fsed_amm') #first value = hypo, second value = epi
O_SDF_sed_amm <- ncdf4::ncvar_get(nc_o,'SDF_Fsed_amm')

ncdf4::nc_close(nc_o)

#lets make oxic plots
plot(O_sed_oxy[1,]) #all zero
plot(O_sed_oxy[2,]) #all zero
plot(O_sed_oxy[1,], O_sed_oxy[2,]) 
plot(O_SDF_sed_oxy[1,], O_SDF_sed_oxy[2,]) #these 2 rows are completely identical 
plot(O_SDF_sed_oxy[1,]) #why is this positive? what is the first row of data vs 2nd?
points(O_SDF_sed_oxy[2,], col="red") #mostly identical values 
#what is the difference between 'OXY_sed_oxy' and 'SDF_Fsed_oxy'?
#why is flux positive? (shouldn't it be negative?)


plot(O_sed_frp[1,])#all set to 1
plot(O_sed_frp[2,])#all set to 1
plot(O_sed_frp[2,],O_sed_frp[1,]) #identical; all values =1
plot(O_SDF_sed_frp[1,])
points(O_SDF_sed_frp[2,], col="red")#what is difference between the 1st and 2nd row, and why does this vary every so slightly?
#all together FRP is not too concerning though?

plot(O_sed_amm[1,])#mostly values at 0 and then some intermittent large negative values at -15
plot(O_sed_amm[2,])#positive very small values, but only at end of time series
plot(O_sed_amm[2,],A_sed_amm[1,]) #values aren't identical
plot(O_SDF_sed_amm[1,])
points(O_SDF_sed_amm[2,], col="red")#what is difference between the 1st and 2nd row?
#why is FRP flux positive but NH4 negative? #why is NH4 positive for one row of the output but not the other?

