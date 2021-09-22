#*****************************************************************
#* TITLE:   FCR GLM-AED script to create figure 2             
#* AUTHORS:  R.P. McClure and C.C. Carey                                          
#* DATE:   Originally developed by CCC in summer 2020; updated by RPM
#*         to convert to ggplot in January 2021; Last modified 13 Sept 2021                            
#* NOTES:  This script uses the data objects created by the 
#*         "MakeFigure_OrganizeDataForFigures.R" 
#*****************************************************************

#DOC:DIN ratio
mod_doc_din_ts <- left_join(mod_doc, mod_amm, by = "DateTime")%>%
  left_join(., mod_nit, by = "DateTime")%>%
  mutate(mod_doc_din_ratio = OGM_doc/(NIT_amm+NIT_nit))

obs_doc_din_ts <- left_join(obs_doc, obs_amm, by = "DateTime")%>%
  left_join(., obs_nit, by = "DateTime")%>%
  mutate(obs_doc_din_ratio = OGM_doc/(NIT_amm+NIT_nit))

doc_din_ts <- ggplot(mod_doc_din_ts, aes(DateTime, mod_doc_din_ratio, color = "Modeled Ratios"))+
  geom_line(size = 1)+
  geom_point(data = obs_doc_din_ts, aes(DateTime, obs_doc_din_ratio, color = "Observed Ratios"), pch = 16, size = 2.5)+
  theme_classic()+
  ylab("Hypolimnetic DOC:DIN")+
  xlab("")+
  labs(title = "A")+
  scale_color_manual(breaks = c("Modeled Ratios", "Observed Ratios"),
                                      values = c("Modeled Ratios" = "black", "Observed Ratios" = "red"),
                                      guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                                               shape = c(NA, 16),
                                                                               color = c("black","red"))))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = c(0.3,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 30, color = "black"),
        plot.title = element_text(face = "bold"))

#DOC:NH4 ratio
mod_doc_nh4_ts <- left_join(mod_doc, mod_amm, by = "DateTime")%>%
  mutate(mod_doc_nh4_ratio = OGM_doc/NIT_amm)

obs_doc_nh4_ts <- left_join(obs_doc, obs_amm, by = "DateTime")%>%
  mutate(obs_doc_nh4_ratio = OGM_doc/NIT_amm)

doc_nh4_ts <- ggplot(mod_doc_nh4_ts, aes(DateTime, mod_doc_nh4_ratio))+
  geom_line(size = 1)+
  geom_point(data = obs_doc_nh4_ts, aes(DateTime, obs_doc_nh4_ratio),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic DOC:NH" [" 4"],""^"+")))+
  xlab("")+
  labs(title = "B")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#DOC:NO3 ratio
mod_doc_no3_ts <- left_join(mod_doc, mod_nit, by = "DateTime")%>%
  mutate(mod_doc_no3_ratio = OGM_doc/NIT_nit)%>%
  filter(!is.infinite(mod_doc_no3_ratio))

obs_doc_no3_ts <- left_join(obs_doc, obs_nit, by = "DateTime")%>%
  mutate(obs_doc_no3_ratio = OGM_doc/NIT_nit)

doc_no3_ts <- ggplot(mod_doc_no3_ts, aes(DateTime, mod_doc_no3_ratio))+
  geom_line(size = 1)+
  geom_point(data = obs_doc_no3_ts, aes(DateTime, obs_doc_no3_ratio),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic DOC:NO" [" 3"],""^"-")))+
  xlab("")+
  labs(title = "C")+
  coord_cartesian(ylim = c(0,2000))+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#DOC:DRP ratio
mod_doc_srp_ts <- left_join(mod_doc, mod_srp, by = "DateTime")%>%
  mutate(mod_doc_srp_ratio = OGM_doc/PHS_frp)%>%
  filter(!is.infinite(mod_doc_srp_ratio))

obs_doc_srp_ts <- left_join(obs_doc, obs_srp, by = "DateTime")%>%
  mutate(obs_doc_srp_ratio = OGM_doc/PHS_frp)

doc_srp_ts <- ggplot(mod_doc_srp_ts, aes(DateTime, mod_doc_srp_ratio))+
  geom_line(size = 1)+
  geom_point(data = obs_doc_srp_ts, aes(DateTime, obs_doc_srp_ratio),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab("Hypolimnetic DOC:DRP")+
  xlab("")+
  labs(title = "D")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#TN:TP ratio
mod_tn_tp_ts <- cbind(TN, TP[2])%>%
  mutate(mod_tn_tp_ratio = TOT_tn_9/TOT_tp_9)

obs_tn_tp_ts <- obs_total_pools%>%
  mutate(obs_tn_tp_ratio = TOT_tn/TOT_tp)

tn_tp_ts <- ggplot(mod_tn_tp_ts, aes(DateTime, mod_tn_tp_ratio))+
  geom_line(size = 1)+
  geom_point(data = obs_tn_tp_ts, aes(DateTime, obs_tn_tp_ratio),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab("Hypolimnetic TN:TP")+
  xlab("")+
  labs(title = "E")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#DIN:DRP
mod_din_srp_ts <- left_join(mod_srp, mod_amm, by = "DateTime")%>%
  left_join(., mod_nit, by = "DateTime")%>%
  mutate(mod_din_srp_ratio = (NIT_amm+NIT_nit)/PHS_frp)

obs_din_srp_ts <- left_join(obs_srp, obs_amm, by = "DateTime")%>%
  left_join(., obs_nit, by = "DateTime")%>%
  mutate(obs_din_srp_ratio = (NIT_amm+NIT_nit)/PHS_frp)

din_srp_ts <- ggplot(mod_din_srp_ts, aes(DateTime, mod_din_srp_ratio))+
  geom_line(size = 1)+
  geom_point(data = obs_din_srp_ts, aes(DateTime, obs_din_srp_ratio),pch = 16, size = 2.5, color = "red")+
  theme_classic()+
  ylab("Hypolimnetic DIN:DRP")+
  xlab("")+
  labs(title = "F")+
  #labs(title = expression('RMSE = 1.32'*~degree*C*''))+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

#patch all of the figures together using patchwork
#this resolution might be a bit much but I am going to leave it for now (RPM:06 January 21)
# Ryan reduced the resolution from 800 to 300 dpi (which is the general requirement for most papers) (RPM:25Mar21)
jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/supp_figs/Model_vs_Observed_ratios_FCR_GLM.jpg", width = 20, height = 20, units = 'in', res = 1000)
figure1 <- (doc_din_ts|doc_nh4_ts)/(doc_no3_ts|doc_srp_ts)/(tn_tp_ts|din_srp_ts)
figure1
dev.off()
