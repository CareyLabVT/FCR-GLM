library(tidyverse)
library(ggpubr)

toc_boxplot <- mediandata%>%
  select(year, med_A_TOC, med_O_TOC)%>%
  rename(Anoxic = med_A_TOC, Oxic = med_O_TOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic TOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "A")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 30, color = "black"),
        plot.title = element_text(face = "bold"))

doc_boxplot <- mediandata%>%
  select(year, med_A_DOC, med_O_DOC)%>%
  rename(Anoxic = med_A_DOC, Oxic = med_O_DOC)%>%
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
  labs(title = "B")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

tn_boxplot <- mediandata%>%
  select(year, med_A_TN, med_O_TN)%>%
  rename(Anoxic = med_A_TN, Oxic = med_O_TN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic TN (mmol m" ^"-3",")")))+
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


din_boxplot <- mediandata%>%
  select(year, med_A_DIN, med_O_DIN)%>%
  rename(Anoxic = med_A_DIN, Oxic = med_O_DIN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DIN (mmol m" ^"-3",")")))+
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


nh4_boxplot <- mediandata%>%
  select(year, med_A_NH4, med_O_NH4)%>%
  rename(Anoxic = med_A_NH4, Oxic = med_O_NH4)%>%
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
  labs(title = "E")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


no3_boxplot <- mediandata%>%
  select(year, med_A_NO3, med_O_NO3)%>%
  rename(Anoxic = med_A_NO3, Oxic = med_O_NO3)%>%
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
  labs(title = "F")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))


tp_boxplot <- mediandata%>%
  select(year, med_A_TP, med_O_TP)%>%
  rename(Anoxic = med_A_TP, Oxic = med_O_TP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic TP (mmol m" ^"-3",")")))+
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

drp_boxplot <- mediandata%>%
  select(year, med_A_PO4, med_O_PO4)%>%
  rename(Anoxic = med_A_PO4, Oxic = med_O_PO4)%>%
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
  labs(title = "H")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure4_StratifiedPeriodMedianHypoCNP_concetrations_v1.jpg", width = 20, height = 25, units = 'in', res = 1000)
figure4 <- (toc_boxplot|doc_boxplot)/(tn_boxplot|din_boxplot)/(nh4_boxplot|no3_boxplot)/(tp_boxplot|drp_boxplot)
figure4
dev.off()
