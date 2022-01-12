#*****************************************************************
#* TITLE:   FCR GLM-AED script to create figure 5            
#* AUTHORS:  C.C. Carey, R.P. McClure                                          
#* DATE:   Originally developed by CCC in summer 2020; updated by RPM
#*         to convert to ggplot in January 2021; Cayelan then modified again
#*         for the parameter sensitivity figure in January 2022                            
#* NOTES:  This script uses the data objects created by the 
#*         "MakeFigure_OrganizeDataForFigures.R" 
#*****************************************************************


# get the packages we will need for the plotting exercises
# install.packages('remotes')
# install.packages('devtools')
devtools::install_github("GLEON/GLMr", INSTALL_opts=c("--no-multiarch"))
remotes::install_github('CareyLabVT/glmtools', force = T, INSTALL_opts=c("--no-multiarch"))

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, dplyr, lubridate, reshape2, patchwork, ncdf4, glmtools, GLM3r, magrittr, ggpubr)

nc_anoxic_2xDOC <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_anoxic_2xDOC.nc')
nc_anoxic_2xDRP <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_anoxic_2xDRP.nc')
nc_anoxic_2xNH4 <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_anoxic_2xNH4.nc')
nc_anoxic_2xNO3 <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_anoxic_2xNO3.nc')
nc_anoxic_halfDOC <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_anoxic_halfDOC.nc')
nc_anoxic_halfDRP <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_anoxic_halfDRP.nc')
nc_anoxic_halfNH4 <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_anoxic_halfNH4.nc')
nc_anoxic_halfNO3 <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_anoxic_halfNO3.nc')
nc_oxic_2xDOC <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_oxic_2xDOC.nc')
nc_oxic_2xDRP <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_oxic_2xDRP.nc')
nc_oxic_2xNH4 <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_oxic_2xNH4.nc')
nc_oxic_2xNO3 <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_oxic_2xNO3.nc')
nc_oxic_halfDOC <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_oxic_halfDOC.nc')
nc_oxic_halfDRP <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_oxic_halfDRP.nc')
nc_oxic_halfNH4 <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_oxic_halfNH4.nc')
nc_oxic_halfNO3 <- file.path('./FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ParameterSensitivity/output_oxic_halfNO3.nc')

#####make the anoxic vs oxic dataset for the boxplot figures####
#pull out deep-water chemistry from each output file
A_2x_DOCr <- get_var(nc_anoxic_2xDOC, "OGM_docr",z_out=9,reference = 'surface')
A_2x_DOC <- get_var(nc_anoxic_2xDOC, "OGM_doc",z_out=9,reference = 'surface')
O_2x_DOCr <- get_var(nc_oxic_2xDOC, "OGM_docr",z_out=9,reference = 'surface')
O_2x_DOC <- get_var(nc_oxic_2xDOC, "OGM_doc",z_out=9,reference = 'surface')

A_2x_NO3 <- get_var(nc_anoxic_2xNO3, "NIT_nit",z_out=9,reference = 'surface')
O_2x_NO3 <- get_var(nc_oxic_2xNO3, "NIT_nit",z_out=9,reference = 'surface')

A_2x_NH4 <- get_var(nc_anoxic_2xNH4, "NIT_amm",z_out=9,reference = 'surface')
O_2x_NH4 <- get_var(nc_oxic_2xNH4, "NIT_amm",z_out=9,reference = 'surface')

A_2x_DRP <- get_var(nc_anoxic_2xDRP, "PHS_frp",z_out=9,reference = 'surface')
O_2x_DRP <- get_var(nc_oxic_2xDRP, "PHS_frp",z_out=9,reference = 'surface')

A_half_DOCr <- get_var(nc_anoxic_halfDOC, "OGM_docr",z_out=9,reference = 'surface')
A_half_DOC <- get_var(nc_anoxic_halfDOC, "OGM_doc",z_out=9,reference = 'surface')
O_half_DOCr <- get_var(nc_oxic_halfDOC, "OGM_docr",z_out=9,reference = 'surface')
O_half_DOC <- get_var(nc_oxic_halfDOC, "OGM_doc",z_out=9,reference = 'surface')

A_half_NO3 <- get_var(nc_anoxic_halfNO3, "NIT_nit",z_out=9,reference = 'surface')
O_half_NO3 <- get_var(nc_oxic_halfNO3, "NIT_nit",z_out=9,reference = 'surface')

A_half_NH4 <- get_var(nc_anoxic_halfNH4, "NIT_amm",z_out=9,reference = 'surface')
O_half_NH4 <- get_var(nc_oxic_halfNH4, "NIT_amm",z_out=9,reference = 'surface')

A_half_DRP <- get_var(nc_anoxic_halfDRP, "PHS_frp",z_out=9,reference = 'surface')
O_half_DRP <- get_var(nc_oxic_halfDRP, "PHS_frp",z_out=9,reference = 'surface')

#bind the data together
data<-as.data.frame(cbind(A_2x_DOCr,A_2x_DOC[,2],O_2x_DOCr[,2],O_2x_DOC[,2],
                          A_2x_NO3[,2],O_2x_NO3[,2],
                          A_2x_NH4[,2],O_2x_NH4[,2],
                          A_2x_DRP[,2],O_2x_DRP[,2],
                          A_half_DOCr[,2],A_half_DOC[,2],O_half_DOCr[,2],O_half_DOC[,2],
                          A_half_NO3[,2],O_half_NO3[,2],
                          A_half_NH4[,2],O_half_NH4[,2],
                          A_half_DRP[,2],O_half_DRP[,2])) 
colnames(data) = c("time", "A_2x_DOCr", "A_2x_DOC", "O_2x_DOCr","O_2x_DOC",
                   "A_2x_NO3","O_2x_NO3", 
                   "A_2x_NH4", "O_2x_NH4", 
                   "A_2x_DRP", "O_2x_DRP", 
                   "A_half_DOCr","A_half_DOC","O_half_DOCr", "O_half_DOC", 
                   "A_half_NO3", "O_half_NO3", 
                   "A_half_NH4","O_half_NH4",
                   "A_half_DRP","O_half_DRP") 
data1 <- data %>% 
  mutate(A_2x_DOCall = A_2x_DOCr + A_2x_DOC,
         O_2x_DOCall = O_2x_DOCr + O_2x_DOC,
         A_half_DOCall = A_half_DOCr + A_half_DOC,
         O_half_DOCall = O_half_DOCr + O_half_DOC) %>% 
  select(time,A_2x_NO3:O_2x_DRP,A_half_NO3:O_half_DOCall) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
#only focus on summer stratified period of July 15-Oct 1
  mutate(month_day = format(as.Date(time), "%m-%d")) %>%
  mutate(year = year(time)) %>%
  filter(month_day <= "10-01", month_day >= "07-15") %>%
  select(-month_day)

mediandata <- data1 %>% 
  group_by(year) %>% 
  summarise(med_A_2x_DOC = median(A_2x_DOCall),
            med_O_2x_DOC = median(O_2x_DOCall),
            med_A_half_DOC = median(A_half_DOCall),
            med_O_half_DOC = median(O_half_DOCall),
            med_A_2x_NH4 = median(A_2x_NH4),
            med_O_2x_NH4 = median(O_2x_NH4),
            med_A_half_NH4 = median(A_half_NH4),
            med_O_half_NH4 = median(O_half_NH4),
            med_A_2x_NO3 = median(A_2x_NO3),
            med_O_2x_NO3 = median(O_2x_NO3),
            med_A_half_NO3 = median(A_half_NO3),
            med_O_half_NO3 = median(O_half_NO3),
            med_A_2x_DRP = median(A_2x_DRP),
            med_O_2x_DRP = median(O_2x_DRP),
            med_A_half_DRP = median(A_half_DRP),
            med_O_half_DRP = median(O_half_DRP))

doc_2x_boxplot <- mediandata%>%
  select(year, med_A_2x_DOC, med_O_2x_DOC)%>%
  rename(Anoxic = med_A_2x_DOC, Oxic = med_O_2x_DOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "A, Double DOC mineralization")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 30, color = "black"),
        plot.title = element_text(face = "bold"))

doc_half_boxplot <- mediandata%>%
  select(year, med_A_half_DOC, med_O_half_DOC)%>%
  rename(Anoxic = med_A_half_DOC, Oxic = med_O_half_DOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "B, Half DOC mineralization")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

nh4_2x_boxplot <- mediandata%>%
  select(year, med_A_2x_NH4, med_O_2x_NH4)%>%
  rename(Anoxic = med_A_2x_NH4, Oxic = med_O_2x_NH4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic NH" [" 4"],""^"+"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "C, Double sed flux")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

nh4_half_boxplot <- mediandata%>%
  select(year, med_A_half_NH4, med_O_half_NH4)%>%
  rename(Anoxic = med_A_half_NH4, Oxic = med_O_half_NH4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic NH" [" 4"],""^"+"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "D, Half sed flux")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

no3_2x_boxplot <- mediandata%>%
  select(year, med_A_2x_NO3, med_O_2x_NO3)%>%
  rename(Anoxic = med_A_2x_NO3, Oxic = med_O_2x_NO3)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic NO" [" 3"],""^"-"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "E, Double nitrification")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

no3_half_boxplot <- mediandata%>%
  select(year, med_A_half_NO3, med_O_half_NO3)%>%
  rename(Anoxic = med_A_half_NO3, Oxic = med_O_half_NO3)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic NO" [" 3"],""^"-"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "F, Half nitrification")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

drp_2x_boxplot <- mediandata%>%
  select(year, med_A_2x_DRP, med_O_2x_DRP)%>%
  rename(Anoxic = med_A_2x_DRP, Oxic = med_O_2x_DRP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DRP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "G, Double sed flux")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

drp_half_boxplot <- mediandata%>%
  select(year, med_A_half_DRP, med_O_half_DRP)%>%
  rename(Anoxic = med_A_half_DRP, Oxic = med_O_half_DRP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DRP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "H, Half sed flux")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/supp_figs/Supp_Figure_ParameterSensitivity.jpg", width = 20, height = 25, units = 'in', res = 1000)
figure4 <- (doc_2x_boxplot|doc_half_boxplot)/(nh4_2x_boxplot|nh4_half_boxplot)/(no3_2x_boxplot|no3_half_boxplot)/(drp_2x_boxplot|drp_half_boxplot)
figure4
dev.off()
