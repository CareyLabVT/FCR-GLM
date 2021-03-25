### Develop plots for FCR-GLM MS ###
### Original plots from CCC --> Figure development by AGH, ASL, RPM, & WMW

### Figure #3 --> Oxic Vs Anoxic Scenarios

### Last update 25Mar21 --> reduced dpi to 300

temp_scenarios <- ggplot()+
  geom_line(data = modtemp_oxic, aes(DateTime, temp, color = "Oxic"), size = 1.3)+
  geom_line(data = modtemp_anoxic, aes(DateTime, temp, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression('Temperature ('*~degree*C*')'))+
  xlab("")+
  ylim(0,20)+
  labs(title = "a")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = c(0.15,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

oxy_scenarios <- ggplot()+
  geom_line(data = mod_oxy_oxic, aes(DateTime, OXY_oxy, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_oxy_anoxic, aes(DateTime, OXY_oxy, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Oxygen (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "b")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"))

amm_scenarios <- ggplot()+
  geom_line(data = mod_amm_oxic, aes(DateTime, NIT_amm, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_amm_anoxic, aes(DateTime, NIT_amm, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Ammonium (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "c")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"))



nit_scenarios <- ggplot()+
  geom_line(data = mod_nit_oxic, aes(DateTime, NIT_nit, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_nit_anoxic, aes(DateTime, NIT_nit, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Nitrate (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "d")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"))


srp_scenarios <- ggplot()+
  geom_line(data = mod_srp_oxic, aes(DateTime, PHS_frp, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_srp_anoxic, aes(DateTime, PHS_frp, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Dissolved Reactive Phosphorus (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "e")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"))


doc_scenarios <- ggplot()+
  geom_line(data = modDOC_oxic, aes(DateTime, DOCall, color = "Oxic"), size = 1.3)+
  geom_line(data = modDOC_anoxic, aes(DateTime, DOCall, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Dissolved Organic Carbon (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "f")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"))


TN_scenarios <- ggplot()+
  geom_line(data = mod_total_pools_oxic, aes(DateTime, totalN, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_total_pools_anoxic, aes(DateTime, totalN, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Total Nitrogen (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "g")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"))


TP_scenarios <- ggplot()+
  geom_line(data = mod_total_pools_oxic, aes(DateTime, totalP, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_total_pools_anoxic, aes(DateTime, totalP, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Total Phosphorus (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "h")+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"))


jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure3_Oxic_vs_Anoxic_scenarios.jpg", width = 20, height = 25, units = 'in', res = 300)
figure3 <- (temp_scenarios|oxy_scenarios)/(doc_scenarios|TN_scenarios)/(amm_scenarios|nit_scenarios)/(TP_scenarios|srp_scenarios)
figure3
dev.off()