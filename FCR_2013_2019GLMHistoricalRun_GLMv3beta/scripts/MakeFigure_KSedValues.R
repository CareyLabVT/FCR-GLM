#load packages
library(tidyverse)
library(patchwork)
setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")


d <- read_csv("./output/Kvalues_AED20210204.csv")

oxy <- seq(0,300, 0.1)

up_MM <- function(x, Km){
  x / (x + Km)
}

down_MM <- function(x, Km){
  Km / (x + Km)
} 

up_down_MM <- function(x, Km, fn){
  x / (x + Km) + fn * Km / (x + Km)
}

d <- d %>% mutate(Rate_Value = abs(Rate_Value),
                  Rate_Value = 1)

d <- d %>% mutate(Rate_Value = abs(Rate_Value))


nh4 <- tibble(oxygen = oxy,
              Knitrif = -up_MM(oxy, d$K_Value[1]) * d$Rate_Value[1] ,
              Ksed_amm = down_MM(oxy, d$K_Value[3]) * d$Rate_Value[3] ,
              Kdom_minerl = up_down_MM(oxy, d$K_Value[9], fn = 0.21) * d$Rate_Value[9] ,
              Kdnra_oxy = down_MM(oxy, d$K_Value[6] * d$Rate_Value[6]),
              nutrient = "NH4") %>% 
  pivot_longer(cols = -c("oxygen", "nutrient"), names_to = "Process", values_to = "Rate")

no3 <- tibble(oxygen = oxy,
              Kdenit = -down_MM(oxy, d$K_Value[2] ) * d$Rate_Value[2],
              Knitrif = up_MM(oxy, d$K_Value[1] ) * d$Rate_Value[1],
              Ksed_nit = -down_MM(oxy, d$K_Value[4])  * d$Rate_Value[4],
              Kdnra_oxy = -down_MM(oxy, d$K_Value[6])  * d$Rate_Value[6],
              nutrient = "NO3") %>% 
  pivot_longer(cols = -c("oxygen", "nutrient"), names_to = "Process", values_to = "Rate")


combined <- rbind(nh4, no3) %>% 
  mutate(Process=recode(Process, Ksed_amm = "Sediment flux",Ksed_nit = "Sediment flux", Kdom_minerl="DON mineralization", 
                        Knitrif="Nitrification", Kdnra_oxy = "DNRA",
                        Kdenit = "Denitrification"))

cols_p <- c("Sediment flux" = "red", 
            "DOP mineralization" = "darkgreen")

cols_n <- c("Sediment flux" = "red", 
            "DON mineralization" = "darkgreen", 
            "Nitrification" = "blue",
            "DNRA" = "purple",
            "Denitrification" = "pink")

cols_c <- c("Sediment flux" = "red", 
            "DOC mineralization" = "darkgreen", 
            "POC hydrolysis" = "orange")

p1 <- ggplot(combined, aes(x = oxygen, y = Rate, color = Process)) +
  geom_line() +
  scale_color_manual(name = "Process", values = cols_n) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(facets = vars(nutrient),
             labeller = as_labeller(c("NH4" = "NH[4]", "NO3" = "NO[3]"), default = label_parsed)) +
  labs(title = "Nitrogen", y = "", x = "") +
  theme_bw()


phos <- tibble(oxygen = oxy,
               Ksed_frp = down_MM(oxy, d$K_Value[7])  * d$Rate_Value[7],
               Kdom_minerl = up_down_MM(oxy, d$K_Value[9], fn = 0.21) * d$Rate_Value[9]) %>% 
  pivot_longer(cols = -c("oxygen"), names_to = "Process", values_to = "Rate") %>% 
  mutate(Process=recode(Process, Ksed_frp = "Sediment flux", Kdom_minerl="DOP mineralization"))

p2 <- ggplot(phos, aes(x = oxygen, y = Rate, color = Process)) +
  geom_line() +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(name = "", values = cols_p) +
  #ylim(-1,1) +
  labs(title = "Phosphorus", y = "Oxygen rate multiplier", x = "") +
  theme_bw()

carbon <- tibble(oxygen = oxy,
                 Ksed_dom = down_MM(oxy, d$K_Value[10]) * d$Rate_Value[6],
                 Kdom_minerl = -up_down_MM(oxy, d$K_Value[9], fn = 0.21) * d$Rate_Value[9],
                 Kpom_hydrol = up_MM(oxy, d$K_Value[8]) * d$Rate_Value[8]) %>% 
  pivot_longer(cols = -c("oxygen"), names_to = "Process", values_to = "Rate") %>% 
  mutate(Process=recode(Process, Ksed_dom = "Sediment flux",Kdom_minerl="DOC mineralization",
                        Kpom_hydrol= "POC hydrolysis"))

p3 <- ggplot(carbon, aes(x = oxygen, y = Rate, color = Process)) +
  geom_line() +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(name = "", values = cols_c ) +
  labs(title = "Carbon", y = "", x = expression(Dissolved~oxygen~(mmol~m^{3}))) +
  theme_bw()

pdf("./figures/KsedComparison.pdf")

p1 / p2 / p3
dev.off()
