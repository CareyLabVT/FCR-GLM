#look at biogeochemical rates between anoxic/oxic scenarios

#load packages
library(zoo)
library(tidyverse)
library(lubridate)
library(ncdf4)
library(patchwork)
library(glmtools)

nc_main <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/output_2013_2019.nc')
oxic <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/output_oxic.nc') 
anoxic <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/output_anoxic.nc')

#####make the anoxic vs oxic dataset####
#pull out deep-water rates from each output file
#nitrogen fractions first
A_anammox <- get_var(anoxic,'NIT_anammox',z_out=9,reference = 'surface') 
O_anammox <- get_var(oxic,'NIT_anammox',z_out=9,reference = 'surface') 

A_denit<- get_var(anoxic,'NIT_denit',z_out=9,reference = 'surface') 
O_denit <- get_var(oxic,'NIT_denit',z_out=9,reference = 'surface') 

A_dnra<- get_var(anoxic,'NIT_dnra',z_out=9,reference = 'surface') 
O_dnra <- get_var(oxic,'NIT_dnra',z_out=9,reference = 'surface') 

A_nitrif<- get_var(anoxic,'NIT_nitrif',z_out=9,reference = 'surface') 
O_nitrif <- get_var(oxic,'NIT_nitrif',z_out=9,reference = 'surface') 

A_sed_nit<- get_var(anoxic,'NIT_sed_nit',z_out=9,reference = 'surface') 
O_sed_nit <- get_var(oxic,'NIT_sed_nit',z_out=9,reference = 'surface') 

A_sed_amm<- get_var(anoxic,'NIT_sed_amm',z_out=9,reference = 'surface') 
O_sed_amm <- get_var(oxic,'NIT_sed_amm',z_out=9,reference = 'surface') 

A_OGM_denit<- get_var(anoxic,'OGM_denit',z_out=9,reference = 'surface') 
O_OGM_denit <- get_var(oxic,'OGM_denit',z_out=9,reference = 'surface') 

A_don_miner<- get_var(anoxic,'OGM_don_miner',z_out=9,reference = 'surface') 
O_don_miner <- get_var(oxic,'OGM_don_miner',z_out=9,reference = 'surface') 

A_donr_miner<- get_var(anoxic,'OGM_donr_miner',z_out=9,reference = 'surface') 
O_donr_miner <- get_var(oxic,'OGM_donr_miner',z_out=9,reference = 'surface') 

A_phyto_nup_nh4<- get_var(anoxic,'PHY_NUP_nh4',z_out=9,reference = 'surface') 
O_phyto_nup_nh4 <- get_var(oxic,'PHY_NUP_nh4',z_out=9,reference = 'surface') 

A_phyto_nup_no3<- get_var(anoxic,'PHY_NUP_no3',z_out=9,reference = 'surface') 
O_phyto_nup_no3 <- get_var(oxic,'PHY_NUP_no3',z_out=9,reference = 'surface') 

#then carbon
A_doc_miner<- get_var(anoxic,'OGM_doc_miner',z_out=9,reference = 'surface') 
O_doc_miner <- get_var(oxic,'OGM_doc_miner',z_out=9,reference = 'surface') 

A_docr_miner<- get_var(anoxic,'OGM_docr_miner',z_out=9,reference = 'surface') 
O_docr_miner <- get_var(oxic,'OGM_docr_miner',z_out=9,reference = 'surface') 

A_poc_hydrol<- get_var(anoxic,'OGM_poc_hydrol',z_out=9,reference = 'surface') 
O_poc_hydrol <- get_var(oxic,'OGM_poc_hydrol',z_out=9,reference = 'surface') 

A_sed_doc<- get_var(anoxic,'OGM_sed_doc',z_out=9,reference = 'surface') 
O_sed_doc <- get_var(oxic,'OGM_sed_doc',z_out=9,reference = 'surface') 

#finally phosphorus
A_dop_miner<- get_var(anoxic,'OGM_dop_miner',z_out=9,reference = 'surface') 
O_dop_miner <- get_var(oxic,'OGM_dop_miner',z_out=9,reference = 'surface') 

A_dopr_miner<- get_var(anoxic,'OGM_dopr_miner',z_out=9,reference = 'surface') 
O_dopr_miner <- get_var(oxic,'OGM_dopr_miner',z_out=9,reference = 'surface') 

A_sed_frp<- get_var(anoxic,'PHS_sed_frp',z_out=9,reference = 'surface') 
O_sed_frp <- get_var(oxic,'PHS_sed_frp',z_out=9,reference = 'surface') 

A_phyto_pup<- get_var(anoxic,'PHY_PUP',z_out=9,reference = 'surface') 
O_phyto_pup <- get_var(oxic,'PHY_PUP',z_out=9,reference = 'surface') 

#these ones for fun and reality checks
A_anaerobic<- get_var(anoxic,'OGM_anaerobic',z_out=9,reference = 'surface') 
O_anaerobic <- get_var(oxic,'OGM_anaerobic',z_out=9,reference = 'surface') 

A_sed_oxy<- get_var(anoxic,'OXY_sed_oxy',z_out=9,reference = 'surface') 
O_sed_oxy <- get_var(oxic,'OXY_sed_oxy',z_out=9,reference = 'surface') 


#bind the data!
data<-as.data.frame(cbind(A_anammox,A_denit[,2],A_dnra[,2],A_nitrif[,2],A_sed_nit[,2],
                          A_sed_amm[,2],A_OGM_denit[,2],A_don_miner[,2],A_donr_miner[,2],
                          A_phyto_nup_nh4[,2],A_phyto_nup_no3[,2],
                          A_doc_miner[,2],A_docr_miner[,2],A_poc_hydrol[,2],A_sed_doc[,2],
                          A_dop_miner[,2],A_dopr_miner[,2],A_sed_frp[,2],A_phyto_pup[,2],
                          A_anaerobic[,2],A_sed_oxy[,2],
                          O_anammox[,2],O_denit[,2],O_dnra[,2],O_nitrif[,2],O_sed_nit[,2],
                          O_sed_amm[,2],O_OGM_denit[,2],O_don_miner[,2],O_donr_miner[,2],
                          O_phyto_nup_nh4[,2],O_phyto_nup_no3[,2],
                          O_doc_miner[,2],O_docr_miner[,2],O_poc_hydrol[,2],O_sed_doc[,2],
                          O_dop_miner[,2],O_dopr_miner[,2],O_sed_frp[,2],O_phyto_pup[,2],
                          O_anaerobic[,2],O_sed_oxy[,2])) 
colnames(data) = c("time", "A_anammox", "A_denit", "A_dnra", "A_nitrif","A_sed_nit",
                   "A_sed_amm","A_OGM_denit", "A_don_miner", "A_donr_miner", 
                   "A_phyto_nup_nh4", "A_phyto_nup_no3", 
                   "A_doc_miner","A_docr_miner", "A_poc_hydrol", "A_sed_doc", 
                   "A_dop_miner", "A_dopr_miner", "A_sed_frp","A_phyto_pup",
                   "A_anaerobic","A_sed_oxy",
                   "O_anammox", "O_denit", "O_dnra", "O_nitrif","O_sed_nit",
                   "O_sed_amm","O_OGM_denit", "O_don_miner", "O_donr_miner", 
                   "O_phyto_nup_nh4", "O_phyto_nup_no3", 
                   "O_doc_miner","O_docr_miner", "O_poc_hydrol", "O_sed_doc", 
                   "O_dop_miner", "O_dopr_miner", "O_sed_frp","O_phyto_pup",
                   "O_anaerobic","O_sed_oxy") 

######subsetting July 15-Oct 1 summer rates

data1 <- data %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time))%>%
  dplyr::filter(month_day <= "10-01", month_day >= "07-15") %>%
  select(-month_day)

#write.csv(data1, "AllnutdataForQuinn.csv", row.names = F)

######
#ammonium first

nh4data <- data1 %>% 
  select(A_sed_amm, A_don_miner,A_donr_miner,A_nitrif,A_anammox,A_dnra,
         O_sed_amm, O_don_miner,O_donr_miner,O_nitrif,O_anammox,O_dnra) %>% 
  summarise_all(list(median)) %>% 
  pivot_longer(cols = A_sed_amm:O_dnra, names_to = c("Scenario", "Process"), names_sep ="_") %>% 
  mutate(value = ifelse(Process == "nitrif", -value, value),
         value = ifelse(Process == "anammox", -value, value)) %>% 
  #pivot_wider(names_from = Scenario, values_from = value) %>% 
  mutate(Scenario=recode(Scenario, A = "Anoxic", O = "Oxic")) %>% 
  mutate(Process=recode(Process, sed = "Sediment flux",don="DON mineralization", 
                     donr="DON mineralization (recalc.)",nitrif="Nitrification",
                     anammox = "Anammox", dnra = "DNRA")) %>% 
  mutate(nutrient = "NH4")

###then nitrate
no3data <- data1 %>% 
  select(A_sed_nit, A_nitrif,A_denit,A_OGM_denit,A_dnra,
         O_sed_nit, O_nitrif,O_denit,O_OGM_denit,O_dnra) %>% 
  summarise_all(list(median)) %>%
  mutate(A_Denitrification = A_denit + A_OGM_denit, O_Denitrification = O_denit + O_OGM_denit) %>% 
  select(-A_denit,-A_OGM_denit,-O_denit,-O_OGM_denit) %>% 
  pivot_longer(cols = A_sed_nit:O_Denitrification, names_to = c("Scenario", "Process"), names_sep ="_") %>% 
  mutate(value = ifelse(Process == "Denitrification", -value, value),
         value = ifelse(Process == "dnra", -value, value)) %>% 
  mutate(Scenario=recode(Scenario, A = "Anoxic", O = "Oxic")) %>% 
  mutate(Process=recode(Process, sed = "Sediment flux",nitrif="Nitrification",
                     dnra = "DNRA")) %>% 
  mutate(nutrient = "NO3")

nitrogen <- rbind(nh4data, no3data)

###now phosphorus
po4data <- data1 %>% 
  select(A_dop_miner, A_dopr_miner, A_sed_frp,
         O_dop_miner, O_dopr_miner, O_sed_frp) %>% 
  summarise_all(list(median)) %>%
  pivot_longer(cols = A_dop_miner:O_sed_frp, names_to = c("Scenario", "Process"), names_sep ="_") %>% 
  #pivot_wider(names_from = Scenario, values_from = value) %>% 
  mutate(Scenario=recode(Scenario, A = "Anoxic", O = "Oxic")) %>% 
  mutate(Process=recode(Process, sed = "Sediment flux", dop="DOP mineralization",
                     dopr = "DOP mineralization (recalc.)"))%>% 
  mutate(nutrient = "DRP")

###finally carbon
docdata <- data1 %>% 
  select(A_doc_miner,A_docr_miner, A_poc_hydrol, A_sed_doc, 
         O_doc_miner,O_docr_miner, O_poc_hydrol, O_sed_doc) %>% 
  summarise_all(list(median)) %>%
  pivot_longer(cols = A_doc_miner:O_sed_doc, names_to = c("Scenario", "Process"), names_sep ="_") %>% 
  mutate(value = ifelse(Process == "doc", -value, value),
         value = ifelse(Process == "docr", -value, value)) %>% 
  #pivot_wider(names_from = Scenario, values_from = value) %>% 
  mutate(Scenario=recode(Scenario, A = "Anoxic", O = "Oxic")) %>% 
  mutate(Process=recode(Process, sed = "Sediment flux",doc="DOC mineralization",
                     docr = "DOC mineralization (recalc.)", poc= "POC hydrolysis"))%>% 
  mutate(nutrient = "DOC")


# Using the "Safe" color palette from rcartocolor: https://github.com/Nowosad/rcartocolor

cols_c <- c("Sediment flux" = "#D55E00", 
            "DOC mineralization" = "#6699CC", 
            "DOC mineralization (recalc.)" = "#888888",
            "POC hydrolysis" = "#E58606")

cols_p <- c("Sediment flux" = "#D55E00", 
          "DOP mineralization" = "#6699CC", 
          "DOP mineralization (recalc.)" = "#888888")

cols_n <- c("Sediment flux" = "#D55E00", 
            "DON mineralization" = "#6699CC", 
            "DON mineralization (recalc.)" = "#888888",
            "Anammox" = "#AA4499",
            "Nitrification" = "#332288",
            "DNRA" = "#117733",
            "Denitrification" = "#88CCEE")   

p1 <- nitrogen %>% 
  ggplot(aes(x = Scenario, y = value, fill = Process)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols_n, name = "Process") +
  facet_wrap(facets = vars(nutrient),
             labeller = as_labeller(c("NH4" = "NH[4]", "NO3" = "NO[3]"), default = label_parsed)) +
  labs(title = "Nitrogen", y = "", x = "") +
  geom_hline(yintercept = 0, lty = "dashed")+
  labs(x = "", y = expression(mmol~m^{-2}~day^{-1}), title = "Nitrogen") +
  ylim(-0.5, 2.1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p2 <- po4data %>% 
  ggplot(aes(x = Scenario, y = value, fill = Process)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols_p, name = "") +
  facet_wrap(facets = vars(nutrient))+
  labs(x = "Scenario", y = "", title = "Phosphorus") +
  geom_hline(yintercept = 0, lty = "dashed")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p3 <- docdata %>% 
  ggplot(aes(x = Scenario, y = value, fill = Process)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols_c, name = "") +
  facet_wrap(facets = vars(nutrient))+
  labs(x = "Scenario", y = "", title = "Carbon") +
  ylim(-0.5, 2.0)+
  geom_hline(yintercept = 0, lty = "dashed")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure7_BiogeoRates.jpg", width = 6, height = 8, units = 'in', res = 1000)
draft <- p3 / p1 / p2
draft
dev.off()

# 
# ###############Calculating Sediment burial rates
# #####carbon first
# A_POC <- get_var(anoxic, "OGM_Psed_poc",z_out=9.2,reference = 'surface')
# O_POC <- get_var(oxic, "OGM_Psed_poc",z_out=9.2,reference = 'surface')
# B_POC<- get_var(nc_file, "OGM_Psed_poc",z_out=9.2,reference = 'surface')
# 
# plot(A_POC$DateTime, A_POC$OGM_Psed_poc_9, col="red", type="l")
# lines(O_POC$DateTime, O_POC$OGM_Psed_poc_9, col="blue")
# plot(B_POC$DateTime, B_POC$OGM_Psed_poc_9, col="green")
# 
# data<-cbind(A_POC, O_POC, B_POC)
# colnames(data)<-c("time", "Anoxic", "time2", "Oxic", "time3", "Baseline")
# 
# # data<-B_POC %>% 
# #   mutate(time = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
# #   mutate(year=year(time)) %>% 
# #   group_by(year) %>% 
# #   summarise(total = sum(OGM_Psed_poc_9.2))
# 
# # data1<-data %>%
# #   mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
# #   mutate(year=year(time),
# #          DOY = yday(time)) %>%
# #   dplyr::filter(DOY < 275, DOY > 195) %>% #July 15-Oct 1
# #   group_by(year) %>% 
# #   summarise(total_anoxic = sum(Anoxic), total_oxic=sum(Oxic))
# # t.test(data1$total_anoxic, data1$total_oxic, paired=T)
# 
# data1 <- data %>% 
#   mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
#   mutate(year=year(time)) %>% 
#   group_by(year) %>% 
#   summarise(mean_anoxic = mean(Anoxic), mean_oxic=mean(Oxic), mean_baseline = mean(Baseline))
# t.test(data1$mean_anoxic, data1$mean_oxic, paired=T)
# 
# data2 <- data1 %>% drop_na() %>% 
#   mutate(total_anoxic = mean_anoxic*12*365*24*60*60/1000,
#          total_oxic = mean_oxic*12*365*24*60*60/1000,
#          total_baseline= mean_baseline*12*365*24*60*60/1000) %>% 
#   summarise(mean_yearly_anoxic_C_burial = mean(total_anoxic), sd_anoxic = std_err(total_anoxic),
#             mean_yearly_oxic_C_burial = mean(total_oxic), sd_oxic = std_err(total_oxic),
#             mean_yearly_baseline_C_burial = mean(total_baseline), sd_base = std_err(total_baseline)) 
# #rates are in mmol/m2/second and need to be converted to g/m2/yr by multiplying by
# #12*365*24*60*60/1000
# 
# 
# #######nitrogen second
# 
# A_PON <- get_var(anoxic, "OGM_Psed_pon",z_out=9.2,reference = 'surface')
# O_PON <- get_var(oxic, "OGM_Psed_pon",z_out=9.2,reference = 'surface')
# B_PON<- get_var(nc_file, "OGM_Psed_pon",z_out=9.2,reference = 'surface')
# 
# plot(A_PON$DateTime, A_PON$OGM_Psed_pon_9.2, col="red", type="l")
# lines(O_PON$DateTime, O_PON$OGM_Psed_pon_9.2, col="blue")
# plot(B_PON$DateTime, B_PON$OGM_Psed_pon_9.2, col="green")
# 
# data<-cbind(A_PON, O_PON, B_PON)
# colnames(data)<-c("time", "Anoxic", "time2", "Oxic", "time3", "Baseline")
# 
# data1 <- data %>% 
#   mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
#   mutate(year=year(time)) %>% 
#   group_by(year) %>% 
#   summarise(mean_anoxic = mean(Anoxic), mean_oxic=mean(Oxic), mean_baseline = mean(Baseline))
# t.test(data1$mean_anoxic, data1$mean_oxic, paired=T)
# 
# data2 <- data1 %>% drop_na() %>% 
#   mutate(total_anoxic = mean_anoxic*14*365*24*60*60/1000,
#          total_oxic = mean_oxic*14*365*24*60*60/1000,
#          total_baseline= mean_baseline*14*365*24*60*60/1000) %>% 
#   summarise(mean_yearly_anoxic_N_burial = mean(total_anoxic), sd_anoxic = std_err(total_anoxic),
#             mean_yearly_oxic_N_burial = mean(total_oxic), sd_oxic = std_err(total_oxic),
#             mean_yearly_baseline_N_burial = mean(total_baseline), sd_base = std_err(total_baseline)) 
# #rates are in mmol/m2/second and need to be converted to g/m2/yr by multiplying by
# #12*365*24*60*60/1000
# 
# #######phosphorus third
# A_POP <- get_var(anoxic, "OGM_Psed_pop",z_out=9.2,reference = 'surface')
# O_POP <- get_var(oxic, "OGM_Psed_pop",z_out=9.2,reference = 'surface')
# B_POP<- get_var(nc_file, "OGM_Psed_pop",z_out=9.2,reference = 'surface')
# 
# plot(A_POP$DateTime, A_POP$OGM_Psed_pop_9.2, col="red", type="l")
# lines(O_POP$DateTime, O_POP$OGM_Psed_pop_9.2, col="blue")
# plot(B_POP$DateTime, B_POP$OGM_Psed_pop_9.2, col="green")
# 
# data<-cbind(A_POP, O_POP, B_POP)
# colnames(data)<-c("time", "Anoxic", "time2", "Oxic", "time3", "Baseline")
# 
# # data<-B_POC %>% 
# #   mutate(time = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
# #   mutate(year=year(time)) %>% 
# #   group_by(year) %>% 
# #   summarise(total = sum(OGM_Psed_poc_9.2))
# 
# # data1<-data %>%
# #   mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
# #   mutate(year=year(time),
# #          DOY = yday(time)) %>%
# #   dplyr::filter(DOY < 275, DOY > 195) %>% #July 15-Oct 1
# #   group_by(year) %>% 
# #   summarise(total_anoxic = sum(Anoxic), total_oxic=sum(Oxic))
# # t.test(data1$total_anoxic, data1$total_oxic, paired=T)
# 
# data1 <- data %>% 
#   mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
#   mutate(year=year(time)) %>% 
#   group_by(year) %>% 
#   summarise(mean_anoxic = mean(Anoxic), mean_oxic=mean(Oxic), mean_baseline = mean(Baseline))
# t.test(data1$mean_anoxic, data1$mean_oxic, paired=T)
# 
# data2 <- data1 %>% drop_na() %>% 
#   mutate(total_anoxic = mean_anoxic*31*365*24*60*60/1000,
#          total_oxic = mean_oxic*31*365*24*60*60/1000,
#          total_baseline= mean_baseline*31*365*24*60*60/1000) %>% 
#   summarise(mean_yearly_anoxic_P_burial = mean(total_anoxic), sd_anoxic = std_err(total_anoxic),
#             mean_yearly_oxic_P_burial = mean(total_oxic), sd_oxic = std_err(total_oxic),
#             mean_yearly_baseline_P_burial = mean(total_baseline), sd_base = std_err(total_baseline)) 
# 
# 
# ###############
# #playing with netcdf file
# nc_o <- ncdf4::nc_open(oxic)
# 
# O_sed_oxy<- ncdf4::ncvar_get(nc_o,'OXY_sed_oxy') 
# O_SDF_sed_oxy<- ncdf4::ncvar_get(nc_o,'SDF_Fsed_oxy') #first value = hypo, second value = epi???
# 
# O_sed_frp <- ncdf4::ncvar_get(nc_o,'PHS_sed_frp')
# O_SDF_sed_frp <- ncdf4::ncvar_get(nc_o,'SDF_Fsed_frp')
# 
# O_sed_amm<- ncdf4::ncvar_get(nc_o,'SDF_Fsed_amm') #first value = hypo, second value = epi
# O_SDF_sed_amm <- ncdf4::ncvar_get(nc_o,'SDF_Fsed_amm')
# 
# ncdf4::nc_close(nc_o)
# names(nc_o$var)#get list of variables in data
# names(nc_o$dim)
# 
