#*****************************************************************                                                           *
#* TITLE:   Sensitivity and numerical optimization calibration script
#*           for GLM-AED
#* AUTHORS:  R. Ladwig and C.C. Carey                    
#* DATE:   Originally developed by R. Ladwig in 2018 and given to CCC in 2019; 
#*         Last modified by CCC in 9 Sept 2021                            
#* NOTES:  CCC modified the original script in 2019 for FCR modeling, 
#*        with subsequent tweaks to annotation in summer 2021. 
#*****************************************************************


rm(list = ls()) #let's clean up that workspace!

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta") #if pulling from github, sets it to proper wd
source('modeling/functions-glm.R') #source the helper functions
read.packages() 

# RUN GLM AND READ RESULTS  ---------------------------
filename = 'FCR'
out = 'output/output.nc' 
sim_vars(out)

sim_folder<-getwd()

run_glm('Compiled') #using Cayelan's GLM-AED dynamic libraries in this repo
plot_temp(out, col_lim = c(0,30))
plot_var(file=out,"OXY_oxy",reference="surface")

# GET FIELD DATA FOR CALIBRATION AND VALIDATION  ---------------------------
# WTR AND OXY DATA
field_temp<-read.csv("field_data/CleanedObsTemp1.csv", header=T)
field_oxy <-read.csv("field_data/CleanedObsOxy1.csv", header=T)
field_temp$DateTime <-as.POSIXct(strptime(field_temp$DateTime, "%Y-%m-%d", tz="EST"))
field_oxy$DateTime <-as.POSIXct(strptime(field_oxy$DateTime, "%Y-%m-%d", tz="EST"))

# CHEMISTRY: GET ALL NUTRIENTS, SILICA, CH4, & CO2
chem <- read.csv('field_data/field_chem.csv', header=T)
chem$DateTime <-as.POSIXct(strptime(chem$DateTime, "%Y-%m-%d", tz="EST"))
silica<-read.csv('field_data/field_silica.csv', header=T)
silica$DateTime <-as.POSIXct(strptime(silica$DateTime, "%Y-%m-%d", tz="EST"))
gases<-read.csv('field_data/field_gases.csv', header=T)
gases$DateTime <-as.POSIXct(strptime(gases$DateTime, "%Y-%m-%d", tz="EST"))


#######################################################
# RUN SENSITIVITY ANALYSIS  ---------------------------
# 1) water temperature
#first, copy & paste your glm3.nml and aed2.nml within their respective directories
# and rename as glm4.nml and aed4.nml; these 4.nml versions are going to be rewritten
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'temp'

#build a matrix with all potential parameters for sensitivity analysis here
calib <- matrix(c('par', 'lb', 'ub', 'x0', 
                  'wind_factor', 0.75, 1.25, 1,
                  'sw_factor', 0.75, 1.25, 1,
                  'lw_factor', 0.75, 1.25, 1,
                  'coef_mix_conv', 0.1, 0.5, 0.2,
                  'coef_wind_stir', 0.1, 0.5, 0.23,
                  'coef_mix_shear', 0.1, 0.6, 0.3,
                  'coef_mix_turb', 0.2, 0.8, 0.51,
                  'coef_mix_KH', 0.1, 0.6, 0.3,
                  'coef_mix_hyp', 0.2, 0.8, 0.5,
                  'ce', 0.0005, 0.002, 0.0013,
                  'ch', 0.0005, 0.002, 0.0013,
                  'cd', 0.0005, 0.002, 0.0013,
                  'zone_heights', 0.1,9.5,5,
                  'zone_heights', 0.1,9.5,9,
                  'rain_factor', 0.75, 1.25, 1,
                  'at_factor', 0.75, 1.25, 1,
                  'rh_factor', 0.75, 1.25, 1,
                  'sed_temp_mean',3,20,11,
                  'sed_temp_mean',3,20,17,
                  'sed_temp_amplitude',2,12,6,
                  'sed_temp_amplitude',2,12,6,
                  'sed_temp_peak_doy',250,280,272,
                  'sed_temp_peak_doy',250,280,272), nrow = 24,ncol = 4, byrow = TRUE) 
#Be sure to edit the nrow value if you decrease the number of rows in matrix above
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_FCR.csv', var)
nml_file = 'glm3.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)

# 2) dissolved oxygen
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'OXY_oxy'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Fsed_oxy', -2, -40, -21,
                  'Fsed_oxy', -2, -40, -21,
                  'Ksed_oxy', 1, 15, 7,
                  'theta_sed_oxy', 0.8, 1.2, 1.08,
                  'Rdom_minerl', 0.0001, 0.10, 0.001,
                  'Rpom_hydrol', 0.0001, 0.10, 0.03,
                  'Rdomr_minerl', 0.00001,0.10,0.0001,
                  'Rcpom_bdown', 0.0001, 0.10, 0.001), nrow = 9, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_FCR.csv', var)
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)

# 3) dissolved inorganic carbon
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'CAR_dic'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Fsed_dic', 0.001, 100, 4,
                  'Ksed_dic', 0.001, 100, 30,
                  'theta_sed_dic', 0.9, 1.2, 1.08), nrow = 4, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_chem.csv', var)
obs <- completeFun(obs, 'CAR_dic')
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 3b) dissolved methane
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'CAR_ch4'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Rch4ox', 0.0001, 5, 1,
                  'Kch4ox', 0.0001, 5, 0.5,
                  'vTch4ox', 0.9, 1.2, 1.08,
                  'Fsed_ch4', 0.001, 300, 30,
                  'Ksed_ch4', 0.001, 100, 30,
                  'theta_sed_ch4', 1, 1.15, 1.08), nrow = 7, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_gases.csv', var)
obs <- completeFun(obs, 'CAR_ch4')
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 4) silica
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'SIL_rsi'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Fsed_rsi', 0.1, 100, 25,
                  'Ksed_rsi', 0.1, 300, 50,
                  'theta_sed_rsi', 1, 1.15, 1.08), nrow = 4, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_silica.csv', var)
obs <- completeFun(obs, 'SIL_rsi')
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)

# 5a) ammonium
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'NIT_amm'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Rnitrif', 0.001, 1, 0.075,
                  'Knitrif', 1, 80, 20,
                  'theta_nitrif', 1, 1.15, 1.08,
                  'Rnh4o2', 0.0001, 2, 1,
                  'Rnh4no2', 0.0001, 2, 0.001,
                  'Ranammox', 0.0001, 2, 0.001,
                  'Kanmx_amm', 0.001, 5, 2,
                  'Rdnra', 0.0001, 2, 0.001,
                  'Kdnra_oxy', 0.01, 5, 2,
                  'Fsed_amm', 1, 30, 4,
                  'Ksed_amm', 1, 30, 10,
                  'theta_sed_amm', 1, 1.15, 1.08), nrow = 13, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_chem.csv', var)
obs <- completeFun(obs, 'NIT_amm')
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)

# 5b) nitrate
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'NIT_nit'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Rnitrif', 0.001, 1, 0.011,
                  'Knitrif', 1, 80, 61,
                  'theta_nitrif', 1, 1.15, 1.08,
                  'Rnh4o2', 0.0001, 2, 1,
                  'Rno2o2', 0.1, 2, 1,
                  'Ranammox', 0.0001, 2, 0.001,
                  'Kanmx_nit', 0.001, 5, 2,
                  'Rdenit', 0.001, 2, 0.065,
                  'Kdenit', 1, 50, 20,
                  'theta_denit',1, 1.15, 1.08,
                  'Rdnra', 0.0001, 2, 0.01,
                  'Kdnra_oxy', 0.01, 5, 2,
                  'Fsed_nit', -10,-0.001, -1.755,
                  'Ksed_nit', 10, 210, 100,
                  'theta_sed_nit', 1, 1.15, 1.08), nrow = 16, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_chem.csv', var)
obs <- completeFun(obs, 'NIT_nit')
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 6) phosphorus
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'PHS_frp'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Fsed_frp', 0.0001,0.5, 0.001,
                  'Ksed_frp', 1, 150, 10,
                  'theta_sed_frp', 1, 1.15, 1.08), nrow = 4, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_chem.csv', var)
obs <- completeFun(obs, 'PHS_frp')
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 7a) dissolved organic carbon labile
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'OGM_doc'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Rdom_minerl', 0.0001, 0.10, 0.001,
                  'Rpom_hydrol', 0.0001, 0.10, 0.03,
                  'theta_minerl', 1, 1.15, 1.08,
                  'theta_hydrol', 1, 1.15, 1.08,
                  'Kdom_minerl', 1, 100, 31.25,
                  'Kpom_hydrol', 1, 100, 31.25,
                  'Rdomr_minerl', 0.00001,0.10,0.0001,
                  'Rcpom_bdown', 0.0001, 0.10, 0.001,
                  'w_pom', -2, 0, -0.06,
                  'sedimentOMfrac', 0.0001, 1, 0.0002,
                  'Fsed_doc', 0.1, 50, 10), nrow = 12, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_chem.csv', var)
obs <- completeFun(obs, 'OGM_doc')
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 7b) dissolved organic carbon recalcitrant
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'OGM_docr'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Rdom_minerl', 0.0001, 0.10, 0.001,
                  'Rpom_hydrol', 0.0001, 0.10, 0.03,
                  'theta_minerl', 1, 1.15, 1.08,
                  'theta_hydrol', 1, 1.15, 1.08,
                  'Kdom_minerl', 1, 100, 31.25,
                  'Kpom_hydrol', 1, 100, 31.25,
                  'Rdomr_minerl', 0.00001,0.10,0.0001,
                  'Rcpom_bdown', 0.0001, 0.10, 0.001,
                  'w_pom', -2, 0, -0.06,
                  'sedimentOMfrac', 0.0001, 1, 0.0002,
                  'Fsed_doc', 0.1, 50, 10), nrow = 12, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_chem.csv', var)
obs <- completeFun(obs, 'OGM_docr')
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 8) chlorophyll a
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
file.copy('aed2/aed4_phyto_pars_30June2020.nml', 'aed2/aed2_phyto_pars_30June2020.nml', overwrite = TRUE) 
var = 'PHY_TCHLA'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'pd%w_p', -0.1, 0.10, -0.001,
                  'pd%Xcc', 30, 500, 100,
                  'pd%R_growth', 0.3, 2.0, 1.1,
                  'pd%theta_growth', 1.02, 1.2, 1.08, #causes it to bomb
                  'pd%T_std', 15, 35, 20, #causes it to bomb
                  'pd%T_opt', 15, 35, 27, #causes it to bomb
                  'pd%T_max', 25, 42, 37, #causes it to bomb
                  'pd%I_K', 10, 400, 50, #causes it to bomb
                  'pd%I_S', 10, 500, 250,
                  'pd%KePHY', 0.001, 0.1, 0.0045, #causes it to bomb
                  'pd%f_pr', 0.001, 0.95, 0.05,
                  'pd%R_resp', 0.001, 0.5, 0.02,
                  'pd%theta_resp', 1.01, 1.15, 1.1, #causes it to bomb
                  'pd%k_fres', 0.01, 1, 0.8,
                  'pd%k_fdom', 0.01, 0.9, 0.1,
                  'pd%K_N', 0.001, 10, 1.5, #causes it to bomb
                  'pd%X_ncon', 0.01, 1, 0.035,
                  'pd%X_nmin', 0, 1, 0.07,
                  'pd%X_nmax', 0.01, 1, 0.2,
                  'pd%R_nuptake', 0.001, 1, 0.06,
                  'pd%K_P', 0.001, 1.5, 0.6,
                  'pd%X_pcon', 0.0001, 0.1, 0.0015,
                  'pd%X_pmin', 0, 0.1, 0.002,
                  'pd%X_pmax', 0.001, 1, 0.05,
                  'pd%R_puptake', 0.0001, 0.1, 0.004), nrow =26, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/CleanedObsChla.csv', var)
obs <- completeFun(obs, 'PHY_TCHLA')
nml_file = 'aed2/aed2_phyto_pars_30June2020.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)



# START CALIBRATION  ---------------------------
# 1) water temperature
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'temp'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(1,1,0.5, 0.0013,4,7,11,17,265,275) - lb) *10 /(ub-lb) # NEEDS TO BE UPDATED WITH STARTING VALUES FROM YOUR CALIBRATION FILE
obs <- read_field_obs('field_data/CleanedObsTemp.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = 'Compiled' 
target_fit = -Inf#1.55
target_iter = 1000 
nml_file = 'glm3.nml'
run_calibvalid(var, var_unit = 'degreesC', var_seq = seq(-5,35,1), cal_pars, pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c()) #var_seq is contour color plot range


#to visualize how parameters are optimized during calibration
# par(mfrow=c(3,4))
# data<-read.csv("results/calib_results_RMSE_temp.csv", header=T)
# temp<-seq(1,length(data$DateTime),1)
# for(i in 1:length(init.val)){
#   plot(temp,data[,i+1],type="l",xlab="Iteration", ylab=(colnames(data[i+1])))
# }


# 2) dissolved oxygen
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'OXY_oxy'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(4.1,-25,-8) - lb) *10 /(ub-lb) # Paul's values
#obs <- read_field_obs('field_data/field_FCR.csv', var)
obs <- read_field_obs('field_data/CleanedObsOxy.csv',var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled" 
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, var_unit = 'mmol/m3', var_seq = seq(0,600,50), cal_pars, pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 3) dissolved inorganic carbon
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'CAR_dic'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(1, 50, 1.08) - lb) *10 /(ub-lb) # Paul's values
obs <- read_field_obs('field_data/field_chem.csv', var)
obs <- na.omit(obs)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,2000,250), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 3b) dissolved methane
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'CAR_ch4'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(250, 150, 1) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_gases_hypo.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,500,25), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 4) silica
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'SIL_rsi'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(5, 50) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_silica.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,1000,50), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 5a) ammonium
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'NIT_amm'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(2.8,1.6,48,1.07) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_chem.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,250,25), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 5b) nitrate
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'NIT_nit'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(5, 15,1.08) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_chem.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,50,5), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 6) phosphate
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'PHS_frp'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(0.011, 0.2, 25) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_chem.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,0.5,0.05), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 7) dissolved organic carbon - recalcitrant fraction
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'OGM_docr'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(31,31) - lb) *10 /(ub-lb)
obs <- read_field_obs('field_data/field_chem.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,500,50), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 8) dissolved organic carbon - labile fraction
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'OGM_doc'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(31,31) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_chem.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,100,10), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())

# 8a) Secchi
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'extc_coef'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'KeDOM',0.00001,0.5,0.0025,
                  'KePOM',0.00001,0.5,0.0015,
                  'KeDOMR', 0.00001, 0.5, 0.01), nrow = 4, ncol = 4, byrow = TRUE)
cal_pars = data.frame(calib)
colnames(cal_pars) <- as.character(unlist(cal_pars[1,]))
cal_pars <- cal_pars[-1,]
#Reload ub, lb for calibration
pars <- as.character(cal_pars$par)
ub <- as.numeric(levels(cal_pars$ub))[cal_pars$ub] 
lb <- as.numeric(levels(cal_pars$lb))[cal_pars$lb] 
cal_pars$x0  <- as.numeric(levels(cal_pars$x0))[cal_pars$x0] 
#Create initial files
init.val <- (cal_pars$x0 - lb) *10 /(ub-lb)
obs <- read_field_obs('field_data/field_secchi.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,1,10), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 9) chlorophyll- my attempt!
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
file.copy('aed2/aed4_phyto_pars_30June2020.nml', 'aed2/aed2_phyto_pars_30June2020.nml', overwrite = TRUE)
var = 'PHY_TCHLA'
calib <- read.csv(paste0('sensitivity/calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(0.01) - lb) *10 /(ub-lb) 
obs <- read_field_obs('field_data/field_chem.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000
nml_file = 'aed2/aed2_phyto_pars_30June2020.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,100,10), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())

