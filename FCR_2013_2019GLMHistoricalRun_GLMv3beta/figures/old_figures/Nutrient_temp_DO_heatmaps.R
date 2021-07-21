#modified heatmap code from ASL
#added in soluble nutrients, DO, and temp 21 Dec 2020 (HLW)

#load packages
library(tidyverse)
library(akima)
library(colorRamps)
library(lubridate)

#read in data
tntp <- read_csv("./FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/totalNP.csv")
chem <- read_csv("./FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/field_chem.csv")
temp <- read_csv("./FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/CleanedObsTemp.csv")
do <- read_csv("./FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/CleanedObsOxy.csv")

#Create a dataset for just this variable
tp <- tntp %>% filter(!is.na(TOT_tp),  Depth<=9.3, year(DateTime)>2013)
#Interpolate to get values at all depths and dates
tp_interp <- interp2xyz(interp(tp$DateTime,tp$Depth,tp$TOT_tp,xo = seq(min(tp$DateTime), max(tp$DateTime), by = 1),yo = seq(min(tp$Depth), max(tp$Depth), by = .1)),data.frame = T)

#Repeat for TN
tn <- tntp%>%filter(!is.na(TOT_tn), Depth<=9.3)
tn_interp <- interp2xyz(interp(tn$DateTime,tn$Depth,tn$TOT_tn,xo = seq(min(tn$DateTime), max(tn$DateTime), by = 1),yo = seq(min(tn$Depth), max(tn$Depth), by = .1)),data.frame = T)

#and DOC
doc <- chem%>%filter(!is.na(OGM_doc), Depth<=9.3)
doc_interp <- interp2xyz(interp(doc$DateTime,doc$Depth,doc$OGM_doc,xo = seq(min(doc$DateTime), max(doc$DateTime), by = 1),yo = seq(min(doc$Depth), max(doc$Depth), by = .1)),data.frame = T)

#NH4
nh4 <- chem%>%filter(!is.na(NIT_amm), Depth<=9.3)
nh4_interp <- interp2xyz(interp(nh4$DateTime,nh4$Depth,nh4$NIT_amm,xo = seq(min(nh4$DateTime), max(nh4$DateTime), by = 1),yo = seq(min(nh4$Depth), max(nh4$Depth), by = .1)),data.frame = T)

#NO3
no3 <- chem%>%filter(!is.na(NIT_nit),  Depth<=9.3)
no3_interp <- interp2xyz(interp(no3$DateTime,no3$Depth,no3$NIT_nit,xo = seq(min(no3$DateTime), max(no3$DateTime), by = 1),yo = seq(min(no3$Depth), max(no3$Depth), by = .1)),data.frame = T)

#FRP
p <- chem%>%filter(!is.na(PHS_frp), Depth<=9.3)
p_interp <- interp2xyz(interp(p$DateTime,p$Depth,p$PHS_frp,xo = seq(min(p$DateTime), max(p$DateTime), by = 1),yo = seq(min(p$Depth), max(p$Depth), by = .1)),data.frame = T)

#Temp
temp <- temp%>%filter(!is.na(temp), Depth<=9.3)
temp_interp <- interp2xyz(interp(temp$DateTime,temp$Depth,temp$temp,xo = seq(min(temp$DateTime), max(temp$DateTime), by = 1),yo = seq(min(temp$Depth), max(temp$Depth), by = .1)),data.frame = T)

#and DO
do <- do%>%filter(!is.na(OXY_oxy), Depth<=9.3)
do_interp <- interp2xyz(interp(do$DateTime,do$Depth,do$OXY_oxy,xo = seq(min(do$DateTime), max(do$DateTime), by = 1),yo = seq(min(do$Depth), max(do$Depth), by = .1)),data.frame = T)

-------------------------------------------------------------------------------
#Create TP Heatmap - note: sometimes fig looks really weird and need to copy and paste tn code and then replace with tp dfs
#jpeg("./figures/TP_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
tp_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+
  labs(x = "", y = "Depth (m)", title = "Total Phosphorus",fill=expression(paste(mu,'g/L')))+
  scale_fill_gradientn(colours = blue2green2red(100), na.value="gray")+
  geom_point(data = tp, aes(x=DateTime, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()

#TN Heatmap
#jpeg("./figures/TN_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
tn_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+
  labs(x = "", y = "Depth (m)", title = "Total Nitrogen",fill=expression(paste(mu,'g/L')))+
  scale_fill_gradientn(colours = blue2green2red(100), na.value="gray")+
  geom_point(data = tn, aes(x=DateTime, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()

#DOC heatmap
#jpeg("./figures/DOC_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
doc_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+
  labs(x = "", y = "Depth (m)", title = "Dissolved Organic Carbon",fill=expression('mg/L'))+
  scale_fill_gradientn(colours = blue2green2red(100), na.value="gray")+
  geom_point(data = doc, aes(x=DateTime, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()

#NH4 heatmap
#jpeg("./figures/NH4_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
nh4_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+
  labs(x = "", y = "Depth (m)", title = "Ammonium",fill=expression(paste(mu,'g/L')))+
  scale_fill_gradientn(colours = blue2green2red(100), na.value="gray")+
  geom_point(data = nh4, aes(x=DateTime, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()

#NO3 heatmap
#jpeg("./figures/NO3_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
no3_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+
  labs(x = "", y = "Depth (m)", title = "Nitrate",fill=expression(paste(mu,'g/L')))+
  scale_fill_gradientn(colours = blue2green2red(100), na.value="gray")+
  geom_point(data = no3, aes(x=DateTime, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()

#SRP heatmap
#jpeg("./figures/FRP_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
p_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+
  labs(x = "", y = "Depth (m)", title = "Filterable Reactive Phosphorus",fill=expression(paste(mu,'g/L')))+
  scale_fill_gradientn(colours = blue2green2red(100), na.value="gray")+
  geom_point(data = p, aes(x=DateTime, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()

#-------------------------
# Function for red --> green color gradient
red2green2blue <- function(n){
  rgb.tables(n,
             red = c(0.2, 0.2, 1),
             green = c(0.5, 0.4, 0.8),
             blue = c(0.8, 0.2, 1))
}

#DO heatmap
#jpeg("./figures/DO_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
do_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+ theme(legend.spacing = unit(3, "cm")) +
  labs(x = "", y = "Depth (m)", title = "Dissolved Oxygen",fill=expression(paste('mmol/m'^'3'))) +
  scale_fill_gradientn(colours = red2green2blue(100), na.value="gray")+
  geom_point(data = do, aes(x=DateTime, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()

#temp heatmap
#jpeg("./figures/temp_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
temp_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+
  labs(x = "", y = "Depth (m)", title = "Temperature",fill=expression(paste(degree,'C')))+
  scale_fill_gradientn(colours = blue2green2red(100), na.value="gray")+
  geom_point(data = temp, aes(x=DateTime, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()

#-----------------------------------------------------------------------------#
#2020 DO heatmap

#read in 2020 FCR site 50 data
DO_2020 <- read_csv("./fielddata/fcr_2020_DO.csv")

#change date format
DO_2020$Date <- as.Date(DO_2020$Date)

#interpolation DO
DO_2020_interp <- interp2xyz(interp(DO_2020$Date,DO_2020$Depth_m,DO_2020$DO_mgL,xo = seq(min(DO_2020$Date), max(DO_2020$Date), by = .1),yo = seq(min(DO_2020$Depth_m), max(DO_2020$Depth_m), by = .01), duplicate = "strip"),data.frame = T)

#jpeg("./figures/2020_DO_heatmap.jpg",width = 8, height = 6, units = "in", res = 300)
DO_2020_interp%>%
  mutate(x = as.Date(x, origin = "1970-01-01"))%>%
  ggplot(aes(x=x, y=y, fill = z))+
  geom_raster()+
  scale_y_reverse(expand = c(0,0))+ theme(legend.spacing = unit(3, "cm")) +
  labs(x = "", y = "Depth (m)", title = "Dissolved Oxygen",fill="mg/L") +
  scale_fill_gradientn(colours = rev(blue2green2red(100)), na.value="gray")+
  geom_point(data = DO_2020, aes(x=Date, y=0.2), size = .2, fill = "black")+
  scale_x_date(expand = c(0,0))+
  theme(panel.border = element_rect(fill = NA))
#dev.off()
