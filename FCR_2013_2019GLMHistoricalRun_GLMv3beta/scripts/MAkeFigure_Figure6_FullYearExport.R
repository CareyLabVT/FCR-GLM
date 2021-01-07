####boxplots of annual fluxes####
#devtools::install_github("kassambara/ggpubr")
library(ggpubr)

toc_export <- retention_full_year%>%
  select(year, Fnet_A_TOC, Fnet_O_TOC)%>%
  rename(Anoxic = Fnet_A_TOC, Oxic = Fnet_O_TOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("TOC flux (%)")+
  xlab("")+
  labs(title = "a")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

tn_export <- retention_full_year%>%
  select(year, Fnet_A_TN, Fnet_O_TN)%>%
  rename(Anoxic = Fnet_A_TN, Oxic = Fnet_O_TN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("TN flux (%)")+
  xlab("")+
  labs(title = "b")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

tp_export <- retention_full_year%>%
  select(year, Fnet_A_TP, Fnet_O_TP)%>%
  rename(Anoxic = Fnet_A_TP, Oxic = Fnet_O_TP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("TP flux (%)")+
  xlab("")+
  labs(title = "c")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

doc_export <- retention_full_year%>%
  select(year, Fnet_A_DOC, Fnet_O_DOC)%>%
  rename(Anoxic = Fnet_A_DOC, Oxic = Fnet_O_DOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("DOC flux (%)")+
  xlab("")+
  labs(title = "d")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

din_export <- retention_full_year%>%
  select(year, Fnet_A_DIN, Fnet_O_DIN)%>%
  rename(Anoxic = Fnet_A_DIN, Oxic = Fnet_O_DIN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("DIN flux (%)")+
  xlab("")+
  labs(title = "e")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


nh4_export <- retention_full_year%>%
  select(year, Fnet_A_NH4, Fnet_O_NH4)%>%
  rename(Anoxic = Fnet_A_NH4, Oxic = Fnet_O_NH4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("NH" [" 4"],""^"+"," flux (%)")))+
  xlab("")+
  labs(title = "f")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


no3_export <- retention_full_year%>%
  select(year, Fnet_A_NO3, Fnet_O_NO3)%>%
  rename(Anoxic = Fnet_A_NO3, Oxic = Fnet_O_NO3)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("NO" [" 3"],""^"-"," flux (%)")))+
  xlab("")+
  labs(title = "g")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


drp_export <- retention_full_year%>%
  select(year, Fnet_A_FRP, Fnet_O_FRP)%>%
  rename(Anoxic = Fnet_A_FRP, Oxic = Fnet_O_FRP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("DRP flux (%)")+
  xlab("")+
  labs(title = "h")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure6_AnnualExportScenarios.jpg", width = 20, height = 25, units = 'in', res = 800)
figure6 <- (toc_export|tn_export)/(tp_export|doc_export)/(din_export|nh4_export)/(no3_export|drp_export)
figure6
dev.off()







####boxplots of seasonal fluxes####
#devtools::install_github("kassambara/ggpubr")
library(ggpubr)

toc_export <- retention_stratified_period%>%
  select(year, Fnet_A_TOC, Fnet_O_TOC)%>%
  rename(Anoxic = Fnet_A_TOC, Oxic = Fnet_O_TOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("TOC flux (%)")+
  xlab("")+
  labs(title = "a")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

tn_export <- retention_stratified_period%>%
  select(year, Fnet_A_TN, Fnet_O_TN)%>%
  rename(Anoxic = Fnet_A_TN, Oxic = Fnet_O_TN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("TN flux (%)")+
  xlab("")+
  labs(title = "b")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

tp_export <- retention_stratified_period%>%
  select(year, Fnet_A_TP, Fnet_O_TP)%>%
  rename(Anoxic = Fnet_A_TP, Oxic = Fnet_O_TP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("TP flux (%)")+
  xlab("")+
  labs(title = "c")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

doc_export <- retention_stratified_period%>%
  select(year, Fnet_A_DOC, Fnet_O_DOC)%>%
  rename(Anoxic = Fnet_A_DOC, Oxic = Fnet_O_DOC)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("DOC flux (%)")+
  xlab("")+
  labs(title = "d")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))

din_export <- retention_stratified_period%>%
  select(year, Fnet_A_DIN, Fnet_O_DIN)%>%
  rename(Anoxic = Fnet_A_DIN, Oxic = Fnet_O_DIN)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("DIN flux (%)")+
  xlab("")+
  labs(title = "e")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


nh4_export <- retention_stratified_period%>%
  select(year, Fnet_A_NH4, Fnet_O_NH4)%>%
  rename(Anoxic = Fnet_A_NH4, Oxic = Fnet_O_NH4)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("NH" [" 4"],""^"+"," flux (%)")))+
  xlab("")+
  labs(title = "f")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


no3_export <- retention_stratified_period%>%
  select(year, Fnet_A_NO3, Fnet_O_NO3)%>%
  rename(Anoxic = Fnet_A_NO3, Oxic = Fnet_O_NO3)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("NO" [" 3"],""^"-"," flux (%)")))+
  xlab("")+
  labs(title = "g")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


drp_export <- retention_stratified_period%>%
  select(year, Fnet_A_FRP, Fnet_O_FRP)%>%
  rename(Anoxic = Fnet_A_FRP, Oxic = Fnet_O_FRP)%>%
  pivot_longer(!year,  names_to = "scenario", values_to = "ratio")%>%
  arrange(scenario)%>%
  ggplot(., aes(scenario, ratio, fill = scenario))+
  geom_boxplot(color = "black")+
  stat_compare_means(label = "p.signif",method = "t.test",label.x = 1.5, size = 8,paired = TRUE)+
  geom_jitter(width = 0.0001, size = 8, color = "black", pch = 21, fill = "grey50", alpha = 0.5)+
  geom_text(aes(label=year), size = 2)+
  scale_fill_manual(values = c("red","blue"))+
  ylab("DRP flux (%)")+
  xlab("")+
  labs(title = "h")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"))


jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/Figure6_StratifiedPeriodExportScenarios.jpg", width = 20, height = 25, units = 'in', res = 800)
figure6x <- (toc_export|tn_export)/(tp_export|doc_export)/(din_export|nh4_export)/(no3_export|drp_export)
figure6x
dev.off()