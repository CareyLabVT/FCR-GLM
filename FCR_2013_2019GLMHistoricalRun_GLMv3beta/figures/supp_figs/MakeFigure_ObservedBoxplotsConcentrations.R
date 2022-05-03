#*****************************************************************
#* TITLE:   FCR GLM-AED script to create supplemental figure highlighting
#*             observed data
#* AUTHORS:  C.C. Carey                                          
#* DATE:   Originally developed by CCC to highlight empirical data, 
#*          and constructed using code to make Figure 5                            
#* NOTES:  This script uses the data objects created by the 
#*         "MakeFigure_OrganizeDataForFigures.R" 
#*****************************************************************

library(tidyverse)
library(ggpubr)
library(patchwork)

realdata <- read_csv("FCR_2013_2019GLMHistoricalRun_GLMv3beta/output/ObservedOxicAnoxicSummerValues.csv") %>% 
  mutate(scenario=ifelse(Status=="Oxic", "Oxygenated", "Non-oxygenated"))


doc_boxplot <- realdata %>%
  ggplot(., aes(scenario, DOC, fill = scenario))+
  geom_boxplot(color = "black")+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DOC (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "(a)")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

tn_boxplot <- realdata %>%
  ggplot(., aes(scenario, TOT_tn, fill = scenario))+
  geom_boxplot(color = "black")+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic TN (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "(b)")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

din_boxplot <- realdata %>%
  select(year, NIT_amm, NIT_nit, scenario) %>%
  mutate(DIN = NIT_amm + NIT_nit) %>%
  ggplot(., aes(scenario, DIN, fill = scenario))+
  geom_boxplot(color = "black")+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DIN (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "(c)")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

nh4_boxplot <- realdata %>%
  ggplot(., aes(scenario, NIT_amm, fill = scenario))+
  geom_boxplot(color = "black")+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic NH" [" 4"],""^"+"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "(d)")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

no3_boxplot <- realdata %>%
  ggplot(., aes(scenario, NIT_nit, fill = scenario))+
  geom_boxplot(color = "black")+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic NO" [" 3"],""^"-"," (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "(e)")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

tp_boxplot <- realdata %>% 
  ggplot(., aes(scenario, TOT_tp, fill = scenario))+
  geom_boxplot(color = "black")+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic TP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "(f)")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

drp_boxplot <- realdata %>%
  filter(PHS_frp > 0.08) %>% 
  ggplot(., aes(scenario, PHS_frp, fill = scenario))+
  geom_boxplot(color = "black")+
  scale_fill_manual(values = c("red","blue"))+
  ylab(expression(paste("Hypolimnetic DRP (mmol m" ^"-3",")")))+
  xlab("")+
  labs(title = "(g)")+
  theme_classic()+
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        title = element_text(size = 24, color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        plot.title = element_text(face = "bold"))

# ghost <- realdata %>%
#   filter(PHS_frp > 0.08) %>% 
#   ggplot(., aes(scenario, PHS_frp, fill = scenario))+
#   geom_boxplot(color = "white")+
#   scale_fill_manual(values = c("white","white"))+
#   ylab(expression(paste("Hypolimnetic DRP (mmol m" ^"-3",")")))+
#   xlab("")+
#   theme_void()+
#   theme(axis.text = element_text(size = 24, color = "white"),
#         axis.title = element_text(size = 24, color = "white"),
#         title = element_text(size = 24, color = "white"),
#         legend.position = "none",
#         legend.title = element_blank(),
#         legend.text = element_text(size = 24, color = "white"))


jpeg("FCR_2013_2019GLMHistoricalRun_GLMv3beta/figures/supp_figs/Supp_Figure_ObservedPatterns.jpg", width = 20, height = 25, units = 'in', res = 1000)
figure <- (doc_boxplot | plot_spacer())/(tn_boxplot | din_boxplot)/(nh4_boxplot | no3_boxplot)/(tp_boxplot | drp_boxplot)
figure
dev.off()
