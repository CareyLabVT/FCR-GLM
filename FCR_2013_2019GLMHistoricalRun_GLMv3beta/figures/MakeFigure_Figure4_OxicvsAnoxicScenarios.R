#*****************************************************************
#* TITLE:   FCR GLM-AED script to create figure 4             
#* AUTHORS:  R.P. McClure and C.C. Carey                                          
#* DATE:   Originally developed by CCC in summer 2020; updated by RPM
#*         to convert to ggplot in January 2021                            
#* NOTES:  This script uses the data objects created by the 
#*         "MakeFigure_OrganizeDataForFigures.R" 
#*****************************************************************

temp_scenarios <- ggplot()+
  geom_line(data = modtemp_oxic, aes(DateTime, temp, color = "Oxic"), size = 1.3)+
  geom_line(data = modtemp_anoxic, aes(DateTime, temp, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression('Hypolimnetic Temperature ('*~degree*C*')'))+
  xlab("")+
  ylim(0,20)+
  labs(title = "A")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.title = element_text(size = 23, color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        title = element_text(size = 23, color = "black"),
        legend.position = c(0.15,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 30, color = "black"),
        plot.title = element_text(face = "bold"))

oxy_scenarios <- ggplot()+
  geom_line(data = mod_oxy_oxic, aes(DateTime, OXY_oxy, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_oxy_anoxic, aes(DateTime, OXY_oxy, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic Oxygen (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "B")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))

toc_scenarios <- ggplot()+
  geom_line(data = TOC_oxic, aes(DateTime, TOT_toc_9, color = "Oxic"), size = 1.3)+
  geom_line(data = TOC_anoxic, aes(DateTime, TOT_toc_9, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic TOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "C")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))

doc_scenarios <- ggplot()+
  geom_line(data = modDOC_oxic, aes(DateTime, DOCall, color = "Oxic"), size = 1.3)+
  geom_line(data = modDOC_anoxic, aes(DateTime, DOCall, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic DOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "D")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))

TN_scenarios <- ggplot()+
  geom_line(data = mod_total_pools_oxic, aes(DateTime, totalN, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_total_pools_anoxic, aes(DateTime, totalN, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic TN (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "E")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))

DIN_scenarios <- ggplot()+
  geom_line(data = DIN_oxic, aes(DateTime, DIN_oxic, color = "Oxic"), size = 1.3)+
  geom_line(data = DIN_anoxic, aes(DateTime, DIN_anoxic, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic DIN (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "F")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))


amm_scenarios <- ggplot()+
  geom_line(data = mod_amm_oxic, aes(DateTime, NIT_amm, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_amm_anoxic, aes(DateTime, NIT_amm, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic NH" [" 4"],""^"+"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "G")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))



nit_scenarios <- ggplot()+
  geom_line(data = mod_nit_oxic, aes(DateTime, NIT_nit, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_nit_anoxic, aes(DateTime, NIT_nit, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic NO" [" 3"],""^"-"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "H")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        title = element_text(size = 23, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))

TP_scenarios <- ggplot()+
  geom_line(data = mod_total_pools_oxic, aes(DateTime, totalP, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_total_pools_anoxic, aes(DateTime, totalP, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic TP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "I")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))

srp_scenarios <- ggplot()+
  geom_line(data = mod_srp_oxic, aes(DateTime, PHS_frp, color = "Oxic"), size = 1.3)+
  geom_line(data = mod_srp_anoxic, aes(DateTime, PHS_frp, color = "Anoxic"), size = 1.3)+
  scale_color_manual(breaks = c("Oxic", "Anoxic"),
                     values = c("Oxic" = "blue", "Anoxic" = "red"))+
  theme_classic()+
  ylab(expression(paste("Hypolimnetic DRP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "J")+
  scale_x_datetime(breaks="year", labels=scales::date_format("%Y")) +
  theme(axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        title = element_text(size = 23, color = "black"),
        axis.text.x = element_text(size = 22),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        plot.title = element_text(face = "bold"))





jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure4_Oxic_vs_Anoxic_scenarios_new.jpg", width = 20, height = 25, units = 'in', res = 1000)
figure4 <- (temp_scenarios|oxy_scenarios)/
  (toc_scenarios|doc_scenarios)/
  (TN_scenarios|DIN_scenarios)/
  (amm_scenarios|nit_scenarios)/
  (TP_scenarios|srp_scenarios)
figure4
dev.off()

#panel lettering: A temp B DO, C TOC, D DOC, E TN, F DIN, G NH4, H NO3, I TP, J DRP
