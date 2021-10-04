#*****************************************************************
#* TITLE:   FCR GLM-AED script to create figure 3             
#* AUTHORS:  R.P. McClure and C.C. Carey                                          
#* DATE:   Originally developed by CCC in summer 2020; updated by RPM
#*         to convert to ggplot in January 2021; Last modified 4 Oct 2021                            
#* NOTES:  This script uses the data objects created by the 
#*         "MakeFigure_OrganizeDataForFigures.R" 
#*****************************************************************

#temp plot
temp <- ggplot()+
  geom_line(data = modtemp, aes(DateTime, temp, color = "Model"), size = 1.3)+
  geom_point(data = obstemp, aes(DateTime, temp, color = "Observed"), pch = 16, size = 2.5)+
  theme_classic()+
  ylim(2,20)+
  ylab(expression('Hypolimnetic Temperature ('*~degree*C*')'))+
  xlab("")+
  labs(title = "A")+
  scale_color_manual(breaks = c("Model", "Observed"),
                     values = c("Model" = "black", "Observed" = "red"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 16),
                                                              color = c("black","red"))))+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = c(0.15,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 30, color = "black"),
        plot.title = element_text(face = "bold"))

#Oxygen plot
oxy <- ggplot(sss_oxy, aes(DateTime, mmol.O2.m3.day))+
  #geom_line(size = 1)+
  geom_area(color = "grey80", alpha = 0.3)+
  geom_line(data = mod_oxy, aes(DateTime, OXY_oxy), size = 1, color = "black")+
  geom_point(data = obs_oxy, aes(DateTime, OXY_oxy),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic Oxygen (mmol m" ^"-3",")")))+
  xlab("")+
  scale_y_continuous(sec.axis = sec_axis( trans=~.*1, name=expression(paste("HOx oxygen addition (mmol m" ^"-3","d" ^"-1",")"))))+
  labs(title = "B")+
  coord_cartesian(ylim = c(0,650))+
  #labs(title = expression(paste("RMSE = 100.5 mmol m" ^"-3","")))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        axis.title.y.right = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#ammonia plot
NIT_amm <- ggplot(mod_amm, aes(DateTime, NIT_amm))+
  geom_line(size = 1)+
  geom_point(data = obs_amm, aes(DateTime, NIT_amm),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic NH" [" 4"],""^"+"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "E")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#nitrate plot
NIT_nit <- ggplot(mod_nit, aes(DateTime, NIT_nit))+
  geom_line(size = 1)+
  geom_point(data = obs_nit, aes(DateTime, NIT_nit),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic NO" [" 3"],""^"-"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "F")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#dissolved reactive phosphorus plot
SRP <- ggplot(mod_srp, aes(DateTime, PHS_frp))+
  geom_line(size = 1)+
  geom_point(data = obs_srp, aes(DateTime, PHS_frp),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic DRP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "H")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

# doc plot
DOC <- ggplot(modDOC, aes(DateTime, DOCall))+
  geom_line(size = 1)+
  geom_point(data = obsDOC, aes(DateTime, DOCall),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic DOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "C")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#total nitrogen plot
TN <- ggplot(mod_total_pools, aes(DateTime, totalN))+
  geom_line(size = 1)+
  geom_point(data = obs_total_pools, aes(DateTime, TOT_tn),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic TN (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "D")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#total phosphorus plot
TP <- ggplot(mod_total_pools, aes(DateTime, totalP))+
  geom_line(size = 1)+
  geom_point(data = obs_total_pools, aes(DateTime, TOT_tp),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic TP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "G")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#patch all of the figures together using patchwork
#this resolution might be a bit much but I am going to leave it for now (RPM:06 January 21)
# Ryan reduced the resolution from 800 to 300 dpi (which is the general requirement for most papers) (RPM:25Mar21)
jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure3_Model_vs_Observed_FCR_GLM.jpg", width = 20, height = 25, units = 'in', res = 1000)
figure2 <- (temp|oxy)/(DOC|TN)/(NIT_amm|NIT_nit)/(TP|SRP)
figure2
dev.off()
