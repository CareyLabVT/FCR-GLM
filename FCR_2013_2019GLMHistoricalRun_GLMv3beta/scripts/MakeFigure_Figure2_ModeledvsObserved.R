### Develop plots for FCR-GLM MS ###
### Original plots from CCC --> Figure development by AGH, ASL, RPM, & WMW

### Figure #2 --> Modeled vs Observed

### Last update 06Jan21

#temp plot
temp <- ggplot()+
  geom_line(data = modtemp, aes(DateTime, temp, color = "Model"), size = 1.3)+
  geom_point(data = obstemp, aes(DateTime, temp, color = "Observed"), pch = 16, size = 2.5)+
  theme_classic()+
  ylim(2,20)+
  ylab(expression('Temperature ('*~degree*C*')'))+
  xlab("")+
  labs(title = "a")+
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
        legend.text = element_text(size = 24, color = "black"))

#Oxygen plot
oxy <- ggplot(sss_oxy, aes(DateTime, mmol.O2.m3.day))+
  #geom_line(size = 1)+
  geom_area(color = "grey80", alpha = 0.3)+
  geom_line(data = mod_oxy, aes(DateTime, OXY_oxy), size = 1, color = "black")+
  geom_point(data = obs_oxy, aes(DateTime, OXY_oxy),pch = 21, fill = "red", size = 2.5, color = "grey10")+
  theme_classic()+
  ylim(0,750)+
  ylab(expression(paste("Oxygen (mmol m" ^"-3",")")))+
  xlab("")+
  scale_y_continuous(sec.axis = sec_axis( trans=~.*1, name=expression(paste("SSS Oxygen addition (mmol m" ^"-3","d" ^"-1",")"))))+
  labs(title = "b")+
  #labs(title = expression(paste("RMSE = 100.5 mmol m" ^"-3","")))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        axis.title.y.right = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"))

#ammonia plot
NIT_amm <- ggplot(mod_amm, aes(DateTime, NIT_amm))+
  geom_line(size = 1)+
  geom_point(data = obs_amm, aes(DateTime, NIT_amm),pch = 21, fill = "red", size = 2.5, color = "grey10")+
  theme_classic()+
  ylab(expression(paste("Ammonium (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "c")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"))

#nitrate plot
NIT_nit <- ggplot(mod_nit, aes(DateTime, NIT_nit))+
  geom_line(size = 1)+
  geom_point(data = obs_nit, aes(DateTime, NIT_nit),pch = 21, fill = "red", size = 2.5, color = "grey10")+
  theme_classic()+
  ylab(expression(paste("Nitrate (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "d")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"))

#dissolved reactive phosphorus plot
SRP <- ggplot(mod_srp, aes(DateTime, PHS_frp))+
  geom_line(size = 1)+
  geom_point(data = obs_srp, aes(DateTime, PHS_frp),pch = 21, fill = "red", size = 2.5, color = "grey10")+
  theme_classic()+
  ylab(expression(paste("Dissolved Reactive Phosphorus (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "e")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"))

# doc plot
DOC <- ggplot(modDOC, aes(DateTime, DOCall))+
  geom_line(size = 1)+
  geom_point(data = obsDOC, aes(DateTime, DOCall),pch = 21, fill = "red", size = 2.5, color = "grey10")+
  theme_classic()+
  ylab(expression(paste("Dissolved Organic Carbon (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "f")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"))

#total nitrogen plot
TN <- ggplot(mod_total_pools, aes(DateTime, totalN))+
  geom_line(size = 1)+
  geom_point(data = obs_total_pools, aes(DateTime, TOT_tn),pch = 21, fill = "red", size = 2.5, color = "grey10")+
  theme_classic()+
  ylab(expression(paste("Total Nitrogen (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "g")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"))

#total phosphorus plot
TP <- ggplot(mod_total_pools, aes(DateTime, totalP))+
  geom_line(size = 1)+
  geom_point(data = obs_total_pools, aes(DateTime, TOT_tp),pch = 21, fill = "red", size = 2.5, color = "grey10")+
  theme_classic()+
  ylab(expression(paste("Total Phosphorus (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "h")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"))

#patch all of the figures together using patchwork
#this resolution might be a bit much but I am going to leave it for now (RPM:06 January 21)
jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure2_Model_vs_Observed_FCR_GLM.jpg", width = 20, height = 25, units = 'in', res = 800)
figure2 <- (temp|oxy)/(NIT_amm|NIT_nit)/(SRP|DOC)/(TN|TP)
figure2
dev.off()
