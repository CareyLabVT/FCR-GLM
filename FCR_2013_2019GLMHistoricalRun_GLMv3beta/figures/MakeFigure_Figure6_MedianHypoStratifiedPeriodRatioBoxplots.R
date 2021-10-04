#*****************************************************************
#* TITLE:   FCR GLM-AED script to create figure 6             
#* AUTHORS:  R.P. McClure and C.C. Carey                                          
#* DATE:   Originally developed by CCC in summer 2020; updated by RPM
#*         to convert to ggplot in January 2021; Last modified 4 Oct 2021                            
#* NOTES:  This script uses the data objects created by the 
#*         "MakeFigure_OrganizeDataForFigures.R" 
#*****************************************************************

#devtools::install_github("kassambara/ggpubr")
library(ggpubr)

toc_tn_boxplot <- mediandata%>%
  select(year, med_A_TOC_TN, med_O_TOC_TN)%>%
  rename(Anoxic = med_A_TOC_TN, Oxic = med_O_TOC_TN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("Hypolimnetic TOC:TN")+
  xlab("")+
  labs(title = "A")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

scenario <- c("Anoxic", "Anoxic", "Anoxic", "Anoxic", "Anoxic", "Anoxic", "Anoxic")
ratio <- c(38, 38, 38, 38, 38, 38, 38)
year <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019")
mis_point <- as.data.frame(cbind(year, scenario, ratio))

doc_no3_boxplot <- mediandata%>%
  select(year, med_A_DOC_NO3, med_O_DOC_NO3)%>%
  rename(Anoxic = med_A_DOC_NO3, Oxic = med_O_DOC_NO3)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  mutate(ratio = ifelse(is.infinite(ratio), NA, ratio))%>%
  ggplot(., aes(scenario, as.numeric(ratio), fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8, paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  geom_point(data = mis_point, aes(scenario, as.numeric(ratio)), pch = 4, size = 30, color = "red")+
  scale_shape_identity()+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DOC:NO" [" 3"],""^"-")))+
  xlab("")+
  labs(title = "E")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

doc_nh4_boxplot <- mediandata%>%
  select(year, med_A_DOC_NH4, med_O_DOC_NH4)%>%
  rename(Anoxic = med_A_DOC_NH4, Oxic = med_O_DOC_NH4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DOC:NH" [" 4"],""^"+")))+
  xlab("")+
  labs(title = "D")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


toc_tp_boxplot <- mediandata%>%
  select(year, med_A_TOC_TP, med_O_TOC_TP)%>%
  rename(Anoxic = med_A_TOC_TP, Oxic = med_O_TOC_TP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("Hypolimnetic TOC:TP")+
  xlab("")+
  labs(title = "B")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


doc_drp_boxplot <- mediandata%>%
  select(year, med_A_DOC_PO4, med_O_DOC_PO4)%>%
  rename(Anoxic = med_A_DOC_PO4, Oxic = med_O_DOC_PO4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("Hypolimnetic DOC:DRP")+
  xlab("")+
  labs(title = "F")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


tn_tp_boxplot <- mediandata%>%
  select(year, med_A_TN_TP, med_O_TN_TP)%>%
  rename(Anoxic = med_A_TN_TP, Oxic = med_O_TN_TP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("Hypolimnetic TN:TP")+
  xlab("")+
  labs(title = "G")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


doc_din_boxplot <- mediandata%>%
  select(year, med_A_DOC_DIN, med_O_DOC_DIN)%>%
  rename(Anoxic = med_A_DOC_DIN, Oxic = med_O_DOC_DIN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("Hypolimnetic DOC:DIN")+
  xlab("")+
  labs(title = "C")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


din_drp_boxplot <- mediandata%>%
  select(year, med_A_DIN_PO4, med_O_DIN_PO4)%>%
  rename(Anoxic = med_A_DIN_PO4, Oxic = med_O_DIN_PO4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  # this runs a paired t.test between each boxplot
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("Hypolimnetic DIN:DRP")+
  xlab("")+
  labs(title = "H")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))



jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure6_MedianStratPeriodBoxplots_v1.jpg", width = 20, height = 25, units = 'in', res = 1000)
figure5 <- (toc_tn_boxplot|toc_tp_boxplot)/(doc_din_boxplot|doc_nh4_boxplot)/(doc_no3_boxplot|doc_drp_boxplot)/(tn_tp_boxplot|din_drp_boxplot)
figure5
dev.off()
