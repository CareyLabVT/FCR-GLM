#Sensitivity & automated calibration script written by Robert Ladwig, June 2019
#modified for FCR-specific applications by CCC
#last updated 3 June 2020

rm(list = ls()) #let's clean up that workspace!

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta") #if pulling from github, sets it to proper wd
source('scripts/functions-glm.R') #source the helper functions
read.packages() 

# RUN GLM AND READ RESULTS  ---------------------------
filename = 'FCR'
out = 'output/output.nc' 
sim_vars(out)

sim_folder<-getwd()

run_glm('Compiled') #pulling from Cayelan's version
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
# 1) water temperature, following ISIMIP approach
#first, copy & paste your glm3.nml and aed2.nml within their respective directories
# and rename as glm4.nml and aed4.nml; these 4.nml versions are going to be rewritten
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
#file.copy('aed2/aed4_1OGMpool_27Aug2019.nml', 'aed2/aed2_1OGMpool_27Aug2019.nml', overwrite = TRUE)
var = 'temp'
calib <- matrix(c('par', 'lb', 'ub', 'x0', #THIS LIST WILL BE EDITED BUT START WITH ALL VARS
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
                  #'zone_heights', 0.1,9.5,5,
                  #'zone_heights', 0.1,9.5,9,
                  #'rain_factor', 0.75, 1.25, 1,
                  #'at_factor', 0.75, 1.25, 1,
                  #'rh_factor', 0.75, 1.25, 1,
                  'sed_temp_mean',3,20,11,
                  'sed_temp_mean',3,20,17,
                  'sed_temp_amplitude',2,12,6,
                  'sed_temp_amplitude',2,12,6,
                  'sed_temp_peak_doy',250,280,272,
                  'sed_temp_peak_doy',250,280,272
                  ), nrow = 19,ncol = 4, byrow = TRUE) #EDIT THE NROW TO REFLECT # OF ROWS IN ANALYSIS
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
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
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
nml_file = 'aed2/aed2_20200612.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)

# 3) dissolved inorganic carbon
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200602.nml', 'aed2/aed2_20200602.nml', overwrite = TRUE)
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
nml_file = 'aed2/aed2_20200602.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 3b) dissolved methane
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200602.nml', 'aed2/aed2_20200602.nml', overwrite = TRUE)
var = 'CAR_ch4'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Rch4ox', 0.0001, 5, 1,
                  'Kch4ox', 0.0001, 5, 0.5,
                  'vTch4ox', 0.9, 1.2, 1.08,
                  'Fsed_ch4', 0.001, 300, 30,
                  'Ksed_ch4', 0.001, 100, 30,
                  'theta_sed_ch4', 1, 1.15, 1.08), nrow = 7, ncol = 4, byrow = TRUE)#BE SURE TO EDIT ROW N!
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
nml_file = 'aed2/aed2_20200602.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 4) silica
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200602.nml', 'aed2/aed2_20200602.nml', overwrite = TRUE)
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
nml_file = 'aed2/aed2_20200602.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)

# 5a) ammonium
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200602.nml', 'aed2/aed2_20200602.nml', overwrite = TRUE)
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
nml_file = 'aed2/aed2_20200602.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)

# 5b) nitrate
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200602.nml', 'aed2/aed2_20200602.nml', overwrite = TRUE)
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
nml_file = 'aed2/aed2_20200602.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 6) phosphorus
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200602.nml', 'aed2/aed2_20200602.nml', overwrite = TRUE)
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
nml_file = 'aed2/aed2_20200602.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 7a) dissolved organic carbon labile
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200611_1DOCpool.nml', 'aed2/aed2_20200611_1DOCpool.nml', overwrite = TRUE)
var = 'OGM_doc'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'Rdom_minerl', 0.0001, 0.10, 0.001,
                  'Rpom_hydrol', 0.0001, 0.10, 0.03,
                  'theta_minerl', 1, 1.15, 1.08,
                  'theta_hydrol', 1, 1.15, 1.08,
                  'Kdom_minerl', 1, 100, 31.25,
                  'Kpom_hydrol', 1, 100, 31.25,
                 # 'Rdomr_minerl', 0.00001,0.10,0.0001,
                 #  'Rcpom_bdown', 0.0001, 0.10, 0.001,
                 #  'w_pom', -2, 0, -0.06,
                 #  'sedimentOMfrac', 0.0001, 1, 0.0002,
                  'Fsed_doc', 0.1, 50, 10), nrow = 8, ncol = 4, byrow = TRUE)
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read_field_obs('field_data/field_chem_1DOCpool.csv', var)
obs <- completeFun(obs, 'OGM_doc')
nml_file = 'aed2/aed2_20200611_1DOCpool.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 7b) dissolved organic carbon recalcitrant
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200602.nml', 'aed2/aed2_20200602.nml', overwrite = TRUE)
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
nml_file = 'aed2/aed2_20200602.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)


# 8) chlorophyll a
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200602.nml', 'aed2/aed2_20200602.nml', overwrite = TRUE)
file.copy('aed2/aed4_phyto_pars_1group_27Aug2019.nml', 'aed2/aed2_phyto_pars_1group_27Aug2019.nml', overwrite = TRUE) #FIX THIS
var = 'PHY_TCHLA'
calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'pd%w_p', -0.1, 0.10, -0.001,
                  'pd%Xcc', 30, 500, 100,
                  'pd%R_growth', 0.3, 2.0, 1.1,
                  #'pd%theta_growth', 1.02, 1.2, 1.08, #causes it to bomb
                  #'pd%T_std', 15, 35, 20, #causes it to bomb
                  #'pd%T_opt', 15, 35, 27, #causes it to bomb
                  #'pd%T_max', 25, 42, 37, #causes it to bomb
                  # 'pd%I_K', 10, 400, 50, #causes it to bomb
                  # 'pd%I_S', 10, 500, 250,
                  # 'pd%KePHY', 0.001, 0.1, 0.0045 #causes it to bomb
                   'pd%f_pr', 0.001, 0.95, 0.05,
                   'pd%R_resp', 0.001, 0.5, 0.02,
                  # 'pd%theta_resp', 1.01, 1.15, 1.1, #not trying this one
                   'pd%k_fres', 0.01, 1, 0.8,
                   'pd%k_fdom', 0.01, 0.9, 0.1,
                   'pd%K_N', 0.001, 10, 1.5, #causes it to bomb
                  # 'pd%X_ncon', 0.01, 1, 0.035,
                  # 'pd%X_nmin', 0, 1, 0.07,
                  # 'pd%X_nmax', 0.01, 1, 0.2,
                   'pd%R_nuptake', 0.001, 1, 0.06,
                 #  'pd%K_P', 0.001, 1.5, 0.6
                  # 'pd%X_pcon', 0.0001, 0.1, 0.0015,
                  # 'pd%X_pmin', 0, 0.1, 0.002,
                  # 'pd%X_pmax', 0.001, 1, 0.05,
                   'pd%R_puptake', 0.0001, 0.1, 0.004
              ), nrow =11, ncol = 4, byrow = TRUE)
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
nml_file = 'aed2/aed2_phyto_pars_1group_27Aug2019.nml'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)



# START CALIBRATION  ---------------------------
# 1) water temperature
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
var = 'temp'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
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
os = 'Compiled' #Changed from Unix
target_fit = -Inf#1.55
target_iter = 500 #1000*length(init.val)^2
nml_file = 'glm3.nml'
run_calibvalid(var, var_unit = 'degreesC', var_seq = seq(-5,35,1), cal_pars, pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c()) #var_seq is contour color plot range


#to visualize how params can converge
par(mfrow=c(3,4))
data<-read.csv("results/calib_results_RMSE_temp.csv", header=T)
temp<-seq(1,length(data$DateTime),1)
for(i in 1:length(init.val)){
  plot(temp,data[,i+1],type="l",xlab="Iteration", ylab=(colnames(data[i+1])))
}


# 2) dissolved oxygen
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
var = 'OXY_oxy'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(4.1,-25,-8) - lb) *10 /(ub-lb) # Paul's values
#obs <- read_field_obs('field_data/field_FCR.csv', var)
obs <- read_field_obs('field_data/CleanedObsOxy_9m.csv',var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled" #Changed from Unix
target_fit = -Inf#2.50 * 1000/32
target_iter = 500#1000*length(init.val)^2
#nml_file = 'aed2/aed2.nml'
nml_file = 'aed2/aed2_20200701_2DOCpools.nml'
run_calibvalid(var, var_unit = 'mmol/m3', var_seq = seq(0,600,50), cal_pars, pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 3) dissolved inorganic carbon
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
var = 'CAR_dic'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
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
target_iter = 500#1000#1000*length(init.val)^2
nml_file = 'aed2/aed2_20200701_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,2000,250), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 3b) dissolved methane
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
var = 'CAR_ch4'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(250, 150, 1) - lb) *10 /(ub-lb) # EDIT THESE
obs <- read_field_obs('field_data/field_gases_hypo.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 500#1000#1000*length(init.val)^2
nml_file = 'aed2/aed2_20200701_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,500,25), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 4) silica
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
var = 'SIL_rsi'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(5, 50) - lb) *10 /(ub-lb) # EDIT THESE
obs <- read_field_obs('field_data/field_silica.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 300#1000*length(init.val)^2
nml_file = 'aed2/aed2_20200701_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,1000,50), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 5a) ammonium
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
var = 'NIT_amm'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(2.8,1.6,48,1.07) - lb) *10 /(ub-lb) # EDIT THESE
obs <- read_field_obs('field_data/field_chem_hypo.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000#1000*length(init.val)^2
nml_file = 'aed2/aed2_20200701_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,250,25), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 5b) nitrate
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'NIT_nit'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
init.val <- (c(5, 15,1.08) - lb) *10 /(ub-lb) # EDIT THESE
obs <- read_field_obs('field_data/field_chem_hypo.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 250#1000*length(init.val)^2
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,50,5), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 6) phosphate
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
var = 'PHS_frp'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(0.011, 0.2, 25) - lb) *10 /(ub-lb) # EDIT THESE
obs <- read_field_obs('field_data/field_chem.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 500#1000*length(init.val)^2
nml_file = 'aed2/aed2_20200701_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,0.5,0.05), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 7) dissolved organic carbon - recalcitrant fraction
#file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20210204_2DOCpools.nml', 'aed2/aed2_20210204_2DOCpools.nml', overwrite = TRUE)
var = 'OGM_docr'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(0.5,0.01, 0.001) - lb) *10 /(ub-lb) # EDIT THESE
obs <- read_field_obs('field_data/field_chem_hypo.csv', var)
#obs$PHS_frp <- obs$PHS_frp * 1000/31
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 500#1000*length(init.val)^2
nml_file = 'aed2/aed2_20210204_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,500,50), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


# 8) dissolved organic carbon - labile fraction
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
var = 'OGM_doc'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(4,31.25,1.08) - lb) *10 /(ub-lb) # EDIT THESE
obs <- read_field_obs('field_data/field_chem.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 500#1000*length(init.val)^2
nml_file = 'aed2/aed2_20200701_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,100,10), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())

# 8a) Secchi
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200701_2DOCpools.nml', 'aed2/aed2_20200701_2DOCpools.nml', overwrite = TRUE)
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
#init.val <- rep(5, nrow(cal_pars))
init.val <- (cal_pars$x0 - lb) *10 /(ub-lb)# EDIT THESE
obs <- read_field_obs('field_data/field_secchi.csv', var)
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 300#1000*length(init.val)^2
nml_file = 'aed2/aed2_20200701_2DOCpools.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,1,10), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())




# 9) chlorophyll- my attempt!
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4_20200612_2DOCpools.nml', 'aed2/aed2_20200612_2DOCpools.nml', overwrite = TRUE)
file.copy('aed2/aed4_phyto_pars_28June2019.nml', 'aed2/aed2_phyto_pars_28June2019.nml', overwrite = TRUE)
var = 'PHY_TCHLA'
calib <- read.csv(paste0('calibration_file_',var,'.csv'), stringsAsFactors = F)
cal_pars = calib
#Reload ub, lb for calibration
pars <- cal_pars$par
ub <- cal_pars$ub
lb <- cal_pars$lb
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (c(0.01) - lb) *10 /(ub-lb) # EDIT THESE
obs <- read_field_obs('field_data/field_chem.csv', var)
#obs$PHS_frp <- obs$PHS_frp * 1000/31
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 25#1000*length(init.val)^2
nml_file = 'aed2/aed2_phyto_pars_28June2019.nml'
run_calibvalid(var, cal_pars, var_unit = 'mmol/m3', var_seq = seq(0,100,10), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())



#### Chlorophyll-a: Robert's version!!!
library(plyr); library(dplyr)#important to load plyr first, then dplyr!
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
file.copy('aed2/aed4_phyto_pars_28June2019.nml', 'aed2/aed2_phyto_pars_28June2019.nml', overwrite = TRUE)
var = 'PHY_TCHLA'

calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  'pd%f_pr',0.001,0.9,0.05,
                  'pd%f_pr',0.001,0.9,0.05,
                  'pd%f_pr',0.001,0.9,0.05,
                  'pd%f_pr',0.001,0.9,0.05,
                  'pd%R_growth',0.3,2,1.1,
                  'pd%R_growth',0.3,2,1.1,
                  'pd%R_growth',0.3,2,1.1,
                  'pd%R_growth',0.3,2,1.1,
                  'pd%R_resp', 0.001, 0.5, 0.02,
                  'pd%R_resp', 0.001, 0.5, 0.02,
                  'pd%R_resp', 0.001, 0.5, 0.02,
                  'pd%R_resp', 0.001, 0.5, 0.02,
                  'pd%w_p', -0.1, 0.15, -0.001,
                  'pd%w_p', -0.1, 0.15, -0.001,
                  'pd%w_p', -0.1, 0.15, -0.001,
                  'pd%w_p', -0.1, 0.15, -0.001,
                  'pd%Xcc', 30, 500, 100,
                  'pd%Xcc', 30, 500, 100,
                  'pd%Xcc', 30, 500, 100,
                  'pd%Xcc', 30, 500, 100), nrow = 21, ncol = 4, byrow = TRUE)
cal_pars = data.frame(calib)
colnames(cal_pars) <- as.character(unlist(cal_pars[1,]))
cal_pars <- cal_pars[-1,]
#Reload ub, lb for calibration
pars <- as.character(cal_pars$par)
ub <- as.numeric(levels(cal_pars$ub))[cal_pars$ub] 
lb <- as.numeric(levels(cal_pars$lb))[cal_pars$lb] 
cal_pars$x0  <- as.numeric(levels(cal_pars$x0))[cal_pars$x0] 
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (cal_pars$x0 - lb) *10 /(ub-lb) # Paul's values
obs <- read_field_obs('field_data/field_chem.csv', var)
# obs <- obs %>%
#   dplyr::filter(depth_range_m == '0-2') %>%
#   select(sampledate, correct_chl_fluor) %>%
#   dplyr::rename(datetime = sampledate, chla = correct_chl_fluor)
#obs<-ddply(obs,.(datetime),summarize, chla=mean(chla))
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 1000#1000*length(init.val)^2
nml_file = 'aed2/aed2_phyto_pars_28June2019.nml'
run_calibvalid(var, cal_pars, var_unit = 'ug/L', var_seq = seq(0,80,5), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())

#### Chlorophyll-a: Robert's version WITH ONE FUNCTIONAL GROUP!!
library(plyr); library(dplyr)#important to load plyr first, then dplyr!
file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
file.copy('aed2/aed4_1OGMpool_27Aug2019.nml', 'aed2/aed2_1OGMpool_27Aug2019.nml', overwrite = TRUE)
file.copy('aed2/aed4_phyto_pars_1group_27Aug2019.nml', 'aed2/aed2_phyto_pars_1group_27Aug2019.nml', overwrite = TRUE)
var = 'PHY_TCHLA'

calib <- matrix(c('par', 'lb', 'ub', 'x0',
                  #'pd%f_pr',0.001,0.9,0.05,
                  'pd%R_growth',0.3,2.5,1.5,
                  'pd%R_resp', 0.001, 0.5, 0.02,
                  'pd%w_p', -0.1, 0.15, -0.001,
                  'pd%Xcc', 1, 300, 50), 
                  #'pd%R_puptake', 0.0001, 0.1, 0.007,
                  #'pd%R_nuptake', 0.0001, 1, 0.13), 
                  nrow = 5, ncol = 4, byrow = TRUE)
cal_pars = data.frame(calib)
colnames(cal_pars) <- as.character(unlist(cal_pars[1,]))
cal_pars <- cal_pars[-1,]
#Reload ub, lb for calibration
pars <- as.character(cal_pars$par)
ub <- as.numeric(levels(cal_pars$ub))[cal_pars$ub] 
lb <- as.numeric(levels(cal_pars$lb))[cal_pars$lb] 
cal_pars$x0  <- as.numeric(levels(cal_pars$x0))[cal_pars$x0] 
#Create initial files
#init.val <- rep(5, nrow(cal_pars))
init.val <- (cal_pars$x0 - lb) *10 /(ub-lb) # Paul's values
obs <- read_field_obs('field_data/field_chem_1DOCpool_1&1.6mONLY.csv', var)
# obs <- obs %>%
#   dplyr::filter(depth_range_m == '0-2') %>%
#   select(sampledate, correct_chl_fluor) %>%
#   dplyr::rename(datetime = sampledate, chla = correct_chl_fluor)
#obs<-ddply(obs,.(datetime),summarize, chla=mean(chla))
method = 'cmaes'
calib.metric = 'RMSE'
os = "Compiled"
target_fit = -Inf#2.50 * 1000/32
target_iter = 300#1000*length(init.val)^2
nml_file = 'aed2/aed2_phyto_pars_1group_27Aug2019.nml'
run_calibvalid(var, cal_pars, var_unit = 'ug/L', var_seq = seq(0,80,5), pars, ub, lb, init.val, obs, method, 
               calib.metric, os, target_fit, target_iter, nml_file, flag = c())


#PLOTS TO CHECK THAT IT WENT ALL OK!

# compare simulated to modeled
var <- "CAR_ch4"
var_unit<-"mmol/m3"
var_seq <- seq(0,100,5)
obs <- read_field_obs('field_data/field_chem.csv', var)
mods <- mod2obs(out, obs, reference = 'surface', var)

g2 <- diag.plots(mods, obs)
ggsave(file=paste0('results/mods_obs',var,'_',filename,'.png'), g2, dpi = 300,width = 384,height = 216, units = 'mm')

h <- paste(filename,', RMSE',round(get_rmse(mods, obs),2),var_unit,'NSE',round(get_nse(mods, obs),2),sep=" ")
png(paste0('results/',var,"_",filename,'.png'), width = 1600, height = 900)
plot_contour(mod_nc = out, reference = 'surface', h, var, var_unit, var_seq) 
dev.off()

data_all <- data.frame('DateTime' = obs$DateTime, 'Depth' = obs$Depth, 'obs' = paste0("obs$",var), 
                       'mods' = paste0("mods$",var))
data_all <- reshape2::melt(data_all, id.var = c("DateTime","Depth"))
g4 <- ggplot(data_all, aes(x = DateTime, y = value, col = variable))+
  geom_line(aes(linetype = variable)) +
  facet_wrap(~Depth,  ncol=1, strip.position = "left", drop = TRUE, scales = "free_y")+
  theme_bw()
ggsave(file=paste0('results/deps_obs_',var,'_',filename,'.pdf'), g4, dpi = 300,width = 600,height = 1000, units = 'mm')


# COMPARE OBSERVATIONS TO MODELED WTR AND OXY USING MANY DIFFERENT VISUALIZATIONS
temp_obs <- read_field_obs('field_data/field_FCR.csv', 'temp')
temp_mods <- mod2obs(out, temp_obs, reference = 'surface', 'temp')

oxy_obs <- read_field_obs('field_data/field_FCR.csv', 'OXY_oxy')
oxy_mods <- mod2obs(out, oxy_obs, reference = 'surface', 'OXY_oxy') 

h <- paste(filename,', RMSE',round(get_rmse(temp_mods, temp_obs),2),'deg C,','NSE',round(get_nse(temp_mods, temp_obs),2),sep=" ")
png(paste0('results/wtemp_',filename,'.png'), width = 1600, height = 900)
plot_contour(mod_nc = out, reference = 'surface', h , 'temp', 'deg C',seq(-5,35,1)) 
dev.off()

h <- paste(filename,', RMSE',round(get_rmse(oxy_mods, oxy_obs),2),'mg/L,','NSE',round(get_nse(oxy_mods, oxy_obs),2),sep=" ")
png(paste0('results/oxy_',filename,'.png'), width = 1600, height = 900)
plot_contour(mod_nc = out, reference = 'surface', h, 'OXY_oxy', 'mmol/m3',seq(0, 600, 50)) 
dev.off()

#create_timeseries(temp_mods, temp_obs, 'temp', c(0,30), "WTR deg C") 
#this makes CRAZY long figures comparing modeled vs observed for every day
#create_timeseries(oxy_mods,oxy_obs, 'OXY_oxy', c(0,15), "DO mg/L")

g1 <- diag.plots(temp_mods, temp_obs)
ggsave(file=paste0('results/mod_obs_wtemp_',filename,'.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')

g2 <- diag.plots(oxy_mods, oxy_obs)
ggsave(file=paste0('results/mod_obs_oxy_',filename,'.png'), g2, dpi = 300,width = 384,height = 216, units = 'mm')

temp_all <- data.frame("DateTime" = temp_obs$DateTime, "Depth" = temp_obs$Depth, "obs" = temp_obs$temp, 
                       'mods' = temp_mods$temp)
temp_all <- reshape2::melt(temp_all, id.var = c("DateTime","Depth"))
g4 <- ggplot(temp_all, aes(x = DateTime, y = value, col = variable))+
  geom_line(aes(linetype = variable)) +
  facet_wrap(~Depth,  ncol=1, strip.position = "left", drop = TRUE, scales = "free_y")+
  theme_bw()
ggsave(file=paste0('results/deps_obs_wtemp_',filename,'.pdf'), g4, dpi = 300,width = 600,height = 1000, units = 'mm')

oxy_all <- data.frame("DateTime" = oxy_obs$DateTime, "Depth" = oxy_obs$Depth, "obs" = oxy_obs$OXY_oxy, 
                      'mods' = oxy_mods$OXY_oxy)
oxy_all <- reshape2::melt(oxy_all, id.var = c("DateTime","Depth"))
g4 <- ggplot(oxy_all, aes(x = DateTime, y = value, col = variable))+
  geom_line(aes(linetype = variable)) +
  facet_wrap(~Depth,  ncol=1, strip.position = "left", drop = TRUE, scales = "free_y")+
  theme_bw()
ggsave(file=paste0('results/deps_obs_oxy_',filename,'.pdf'), g4, dpi = 300,width = 600,height = 1000, units = 'mm')






# VERIFICATION
source('scripts/functions-glm.R')
var_dep = 5

var = 'OXY_oxy'
mult = 1000/32
var_unit = 'mg/L'#'mmol/m3'
g3 <- compare_depths(read_field_obs('field_data/field_FCR.csv', var), 
                     mod2obs(out, read_field_obs('field_data/field_FCR.csv', var), 
                             reference = 'surface', var), var_dep, var,var_unit,mult); g3
ggsave(file=paste0('results/surf_',var,'_',var_dep,'.png'), g3, dpi = 300,width = 484,height = 116, units = 'mm')

var = 'CAR_dic'
mult = 1000/12
var_unit = 'mg/L'#'mmol/m3'
g3 <- compare_depths(read_field_obs('field_data/field_chemistry.csv', var), 
                     mod2obs(out, read_field_obs('field_data/field_chemistry.csv', var), 
                             reference = 'surface', var), var_dep, var,var_unit,mult); g3
ggsave(file=paste0('results/surf_',var,'_',var_dep,'.png'), g3, dpi = 300,width = 484,height = 116, units = 'mm')

var = 'NIT_nit'
mult = 1000/14
g3 <- compare_depths(read_field_obs('field_data/field_chemistry.csv', var), 
                     mod2obs(out, read_field_obs('field_data/field_chemistry.csv', var), 
                             reference = 'surface', var), var_dep, var,var_unit,mult); g3
ggsave(file=paste0('results/surf_',var,'_',var_dep,'.png'), g3, dpi = 300,width = 484,height = 116, units = 'mm')

var = 'PHS_frp'
mult = 1000/31
g3 <- compare_depths(read_field_obs('field_data/field_chemistry.csv', var), 
                     mod2obs(out, read_field_obs('field_data/field_chemistry.csv', var), 
                             reference = 'surface', var), var_dep, var,var_unit,mult); g3
ggsave(file=paste0('results/surf_',var,'_',var_dep,'.png'), g3, dpi = 300,width = 484,height = 116, units = 'mm')


diatoms<- get_var(out, 'PHY_DIATOMPCH4', z_out = 2)
chloro<- get_var(out, 'PHY_CHLOROPCH3', z_out = 2)
cyano2<- get_var(out, 'PHY_CYANONPCH2', z_out = 2)
cyano1<- get_var(out, 'PHY_CYANOPCH1', z_out = 2)

phyto_all <- data.frame("DateTime" = diatoms$DateTime, 'diatoms' = diatoms$PHY_DIATOMPCH4.elv_2,
                        'chloro' = chloro$PHY_CHLOROPCH3.elv_2,
                        'cyano2' = cyano2$PHY_CYANONPCH2.elv_2,
                        'cyano1' = cyano1$PHY_CYANOPCH1.elv_2)
phyto_all <- reshape2::melt(phyto_all, id.var = c("DateTime"))
g5 <- ggplot(phyto_all, aes(x = DateTime, y = value, col = variable )) + geom_line() +
  theme_bw() +
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
ggsave(file=paste0('results/mod_obs_phyto_',filename,'.png'), g5, dpi = 300,width = 484,height = 116, units = 'mm')



temp_obs <- read_field_obs('field_data/field_FCR.csv', 'temp')
deps_interp <- seq(0,24.5,0.5)
temp_interp <- matrix(0, ncol = length(deps_interp), nrow= length(unique(temp_obs$DateTime)))
for (ii in 1:length(unique(temp_obs$DateTime))){
  data <- temp_obs %>%
    dplyr::filter(DateTime == unique(temp_obs$DateTime)[ii])
  if (is.na(mean(data$temp, na.rm = TRUE))){
    temp_interp[ii,] <- deps_interp * NA
  } else {
    temp_interp[ii,] <- signal::interp1(x = data$Depth,
                                        y = data$temp,
                                        xi = deps_interp,
                                        method = 'spline', extrap = TRUE)
  }
}
df_temp_interp <- cbind(as.Date(unique(temp_obs$DateTime)), data.frame(temp_interp))
names(df_temp_interp) <- c("DateTime", sapply('wtr_', paste, deps_interp, sep=""))

temp_sim <- get_wattemp(out, reference = "surface", h, 'temp')
sim=t(temp_sim$sim)
sim=data.matrix(sim)[,rev(seq_len(ncol(data.matrix(sim))))]
#df_temp_sim <- cbind(as.Date(temp_sim$time), apply((t(temp_sim$sim)),1,rev))
df_temp_sim <- cbind(as.Date(temp_sim$time), sim)
df_temp_sim <- data.frame(df_temp_sim)
names(df_temp_sim) <- c("DateTime", sapply('wtr_', paste, rev(temp_sim$depth), sep=""))
head(df_temp_sim)

thermo_obs <- ts.thermo.depth(df_temp_interp, Smin = 0.1, na.rm = FALSE)
thermo_sim <- ts.thermo.depth(df_temp_sim, Smin = 0.1, na.rm = TRUE)

source('scripts/functions-glm.R')
h <- paste('DO_thermodepth')
png(paste0('results/stratif_',filename,'.png'), width = 1800, height = 600)
plot_contour_thermodep(mod_nc = out, reference = 'surface', h, var='OXY_oxy', 
                         unit='mmol/m3',tlevels=seq(0, 600, 50), td=thermo_sim) 
dev.off()

# CHLOROPHYLL-A DATA
# metabolism_all <- read_csv("field_data/ntl129_1_v5.csv")
# colnames(metabolism_all)
# field_metabol <- metabolism_all %>%
#   select(sampledate,avg_chlor) 
sim_metabol <- get_var(out, 'PHY_TCHLA', z_out = 1)

# SECCHI DEPTH DATA
secchi <- read.csv('field_data/Secchi_depth.csv')
secchi <- secchi %>%
  rename(datetime = DateTime, secchidep = Secchi_m)
secchi$datetime <-as.POSIXct(strptime(secchi$datetime, "%Y-%m-%d", tz="EST"))
sim_kz <- get_var(out, 'extc_coef', z_out = 1)
sim_secchi <- 1.7 / sim_kz$extc_coef.elv_1

secchi <- secchi %>%
  dplyr::filter(datetime <= max(as.Date(sim_kz$DateTime)))

same_dates <- c()
for (ii in 1:length(secchi$datetime)){
  same_dates <- append(same_dates, which(secchi$datetime[ii] == as.Date(sim_kz$DateTime)))
}

kz_all <- data.frame("DateTime" = secchi$datetime, "obs" = secchi$secchidep, "mods" = sim_secchi[same_dates])
kz_all <- reshape2::melt(kz_all, id.var = c("DateTime"))
g5 <- ggplot(kz_all, aes(x = DateTime, y = value, col = variable )) + geom_line() +theme_bw() 
ggsave(file=paste0('results/mod_obs_kz_',filename,'10June2019.png'), g5, dpi = 300,width = 584,height = 216, units = 'mm')

h <- paste(filename,'DIATOMS',sep="_")
png(paste0('results/_',h,'.png'), width = 1600, height = 900)
plot_contour(mod_nc = out, reference = 'surface', h , 'PHY_DIATOMPCH4', 'mmol/m3',seq(0,50,5)) 
dev.off()

h <- paste(filename,'CHLORO',sep="_")
png(paste0('results/_',h,'.png'), width = 1600, height = 900)
plot_contour(mod_nc = out, reference = 'surface', h , 'PHY_CHLOROPCH3', 'mmol/m3',seq(0,10,1)) 
dev.off()

h <- paste(filename,'CYANO2',sep="_")
png(paste0('results/_',h,'.png'), width = 1600, height = 900)
plot_contour(mod_nc = out, reference = 'surface', h , 'PHY_CYANONPCH2', 'mmol/m3',seq(0,50,5)) 
dev.off()

h <- paste(filename,'CYANO1',sep="_")
png(paste0('results/_',h,'.png'), width = 1600, height = 900)
plot_contour(mod_nc = out, reference = 'surface', h , 'PHY_CYANOPCH1', 'mmol/m3',seq(0,200,10)) 
dev.off()

# IS THERE A SPRING CLEAR WATER STATE?
ts_secchi <- ts(sim_secchi, frequency =365, start = as.Date(sim_kz$DateTime[1]))
dec_ts_secchi <- decompose(ts_secchi)
png(paste0('results/decompose_secchi',filename,'.png'), width = 1600, height = 900)
plot(dec_ts_secchi)
dev.off()



# DOCKER

system('docker run -it -d -v /Users/robertladwig/Documents/lakemendota-glm/docker:/GLM/lakemendota hydrobert/glm-aed2 /bin/bash')

dockerps <- system('docker ps',intern = TRUE)
dockerid <- strsplit(dockerps, split = " ")
dockerid <- dockerid[[2]][1]

system(paste('docker exec -i',dockerid,'/bin/bash -c \"cd lakemendota; /GLM/glm\"'))
