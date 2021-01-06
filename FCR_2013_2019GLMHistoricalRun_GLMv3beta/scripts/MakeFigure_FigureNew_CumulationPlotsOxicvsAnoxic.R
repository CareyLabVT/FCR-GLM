### Develop plots for FCR-GLM MS ###
### Original plots from CCC --> Figure development by AGH, ASL, RPM, & WMW

### Figure #Additional --> Modeled vs Observed

### Last update 06Jan21

# cumulation plots
oxy_scenarios_cum <- ggplot()+
  geom_line(data = mod_oxy_oxic, aes(DateTime, OXYcum, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_oxy_anoxic, aes(DateTime, OXYcum, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Oxygen Accumulation (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "a")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = c(0.15,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

amm_scenarios_cum <- ggplot()+
  geom_line(data = mod_amm_oxic, aes(DateTime, AMMcum, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_amm_anoxic, aes(DateTime, AMMcum, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Ammonium Accumulation (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "b")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

nit_scenarios_cum <- ggplot()+
  geom_line(data = mod_nit_oxic, aes(DateTime, NITcum, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_nit_anoxic, aes(DateTime, NITcum, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Nitrate Accumulation (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "c")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

srp_scenarios_cum <- ggplot()+
  geom_line(data = mod_srp_oxic, aes(DateTime, SRPcum, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_srp_anoxic, aes(DateTime, SRPcum, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("DRP Accumulation (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "d")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

doc_scenarios_cum <- ggplot()+
  geom_line(data = modDOC_oxic, aes(DateTime, DOCcum, color = "Oxic"), size = 1.3)+
  geom_line(data = modDOC_anoxic, aes(DateTime, DOCcum, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("DOC Accumulation (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "e")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

TN_scenarios_cum <- ggplot()+
  geom_line(data = mod_total_pools_oxic, aes(DateTime, TNcum, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_total_pools_anoxic, aes(DateTime, TNcum, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Nitrogen Accumulation (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "f")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

TP_scenarios_cum <- ggplot()+
  geom_line(data = mod_total_pools_oxic, aes(DateTime, TPcum, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_total_pools_anoxic, aes(DateTime, TPcum, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Phosphorus Accumulation (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "g")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/FigureX_added_cumulation_plots_scenarios.jpg", width = 20, height = 25, units = 'in', res = 800)
figure4 <- (plot_spacer()|oxy_scenarios_cum)/(amm_scenarios_cum|nit_scenarios_cum)/(srp_scenarios_cum|doc_scenarios_cum)/(TN_scenarios_cum|TP_scenarios_cum)
figure4
dev.off()
