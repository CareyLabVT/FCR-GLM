#to create scenarios for oxic vs anoxic summer conditions in FCR

#load packages
library(zoo)
library(tidyverse)
library(lubridate)
library(gganimate)

setwd("./FCR_2013_2019GLMHistoricalRun_GLMv3beta")
setwd("../") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#####baseline scenario: based on observed SSS practices
nc_file <- file.path(sim_folder, 'output/observed_output.nc')

#####anoxic scenario: no SSS activation throughout
nc_file <- file.path(sim_folder, 'output/anoxic_output.nc')

#####oxic scenario: SSS on in summer April 15-Nov 15 at full level
nc_file <- file.path(sim_folder, 'output/oxic_output.nc')

#####pull out deep-water chemistry
oxygen <- get_var(nc_file,'OXY_oxy',z_out=9.2,reference = 'surface') 
TN <- get_var(nc_file,'TOT_tn',z_out=9.2,reference = 'surface') 
TP <- get_var(nc_file,'TOT_tp',z_out=9.2,reference = 'surface') 
TOC <- get_var(nc_file,'TOT_toc',z_out=9.2,reference = 'surface') 

#create ratios
data<-as.data.frame(cbind(oxygen,TN[,2],TP[,2],TOC[,2])) 
colnames(data) = c("time", "oxy", "tn", "tp", "toc") 
data <- data %>% 
  mutate(NP = tn/tp,
         CN = toc/tn,
         CP = toc/tp,
         year = year(time),
         doy = yday(time))

#visualize the data & hysteresis 
plot(data$oxy, data$NP, xlim=c(0,500))

ggplot(data = data) +
  geom_path(aes(x = oxy, y = NP, color = doy)) + xlim(0,1000)

ggplot(data = data) +
  geom_path(aes(x = oxy, y = CP, color = doy)) + xlim(0,1000)

ggplot(data = data) +
  geom_path(aes(x = oxy, y = CN, color = doy)) + xlim(0,1000)

plot(oxygen$DateTime, oxygen$OXY_oxy_9.2)
#need to remove crazy high outliers


#####observational data analysis
setwd("../inputs")

#first pull in FCR chem data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/6/2b3dc84ae6b12d10bd5485f1c300af13" 
infile1 <- paste0(getwd(),"/chem.csv")
download.file(inUrl1,infile1,method="curl")

FCRchem <- read.csv("chem.csv", header=T) %>%
  select(Reservoir:DOC_mgL) %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(depth = Depth_m, time=DateTime) %>%
  dplyr::filter(!(depth %in% c(5.2,5.5,5.8,6,9.5,10,10.5))) %>% 
  select(time:DOC_mgL)

depths<-sort(unique(FCRchem$depth)) #depths that we will pull from the CTD

#now let's get CTD data
#need to import CTD observations from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/10/2461524a7da8f1906bfc3806d594f94c" 
infile1 <- paste0(getwd(),"/CTD_final_2013_2019.csv")
download.file(inUrl1,infile1,method="curl")

#read in CTD temp file from EDI to create field file, but first need to subset CTD data per each day to depths
ctd<-read_csv('CTD_final_2013_2019.csv') %>% #read in observed CTD data, which has multiple casts on the same day (problematic for comparison)
  filter(Reservoir=="FCR") %>%
  filter(Site==50) %>%
  rename(time=Date, depth=Depth_m, temp=Temp_C, DO=DO_mgL) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  select(time, depth, temp, DO) %>%
  na.omit() 

#Initialize an empty matrix with the correct number of rows and columns for CTD data
temp<-matrix(data=NA, ncol=ncol(ctd), nrow=length(depths)) #of cols in CTD data, and then nrows = # of layers produced
super_final<-matrix(data=NA, ncol=1, nrow=0)
dates<-unique(ctd$time)

#create a function to chose the matching depth closest to our focal depths
closest<-function(xv, sv){
  xv[which.min(abs(xv-sv))]}

library(plyr) #only use plyr for this for loop, then detach!

#For loop to retrieve CTD depth with the closest function and fill in matrix
for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(ctd, ctd$time == j)
  
  layer1<- q[q[, "depth"] == closest(q$depth,0.1),][1,]
  layer2<- q[q[, "depth"] == closest(q$depth,0.8),][1,]
  layer3<- q[q[, "depth"] == closest(q$depth,1.6),][1,]
  layer4<- q[q[, "depth"] == closest(q$depth,2.8),][1,]
  layer5<- q[q[, "depth"] == closest(q$depth,3.8),][1,]
  layer6<- q[q[, "depth"] == closest(q$depth,5),][1,]
  layer7<- q[q[, "depth"] == closest(q$depth,6.2),][1,]
  layer8<- q[q[, "depth"] == closest(q$depth,8),][1,]
  layer9<- q[q[, "depth"] == closest(q$depth,9),][1,]
  
  temp<-rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9)
  temp[,((ncol(ctd))+1)] <- depths
  colnames(temp)[((ncol(ctd))+1)]<-"new_depth"
  final <- temp
  final <- data.frame(final)
  super_final <- rbind.fill.matrix(super_final,final)
}

detach(package:plyr)#to prevent issues with dplyr vs plyr not playing well together!

#now need to clean up the data frame and make all factors numeric
super_final <- as.data.frame(super_final) %>%
  select(time, new_depth, temp, DO) %>%
  rename(depth = new_depth) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  mutate(depth = as.numeric(levels(depth))[depth]) %>%
  mutate(temp = as.numeric(levels(temp))[temp]) %>%
  mutate(DO = as.numeric(levels(DO))[DO]) 

together <- merge(FCRchem, super_final, by=c("time", "depth"), all.x=T)  
  
#now pull in YSI data to fill in missing temp/DO data
#need to import YSI observations from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/198/7/25b5e8b7f4291614d5c6d959a08148d8" 
infile1 <- paste0(getwd(),"/YSI_PAR_profiles_2013-2019.csv")
download.file(inUrl1,infile1,method="curl")

ysi <- read.csv("YSI_PAR_profiles_2013-2019.csv", header=T) %>% 
  filter(Reservoir == "FCR") %>% 
  filter(Site == 50) %>% 
  select(DateTime:DO_mgL) %>% 
  rename(time = DateTime, depth = Depth_m, temp = Temp_C, DO = DO_mgL) %>% 
  mutate(time = as.POSIXct(strptime(time, "%m/%d/%y %H:%M", tz="EST"))) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
  filter(!(is.na(temp) & is.na(DO)))

more <- merge(together, ysi, all.x=T, by=c("time", "depth"))

for(i in 1:length(more$time)){
  if(is.na(more$temp.x[i])==F){
    more$temp_new[i]=more$temp.x[i]
  }
  if(is.na(more$temp.x[i])==T & is.na(more$temp.y[i])==F){
    more$temp_new[i]=more$temp.y[i]
  }
  if(is.na(more$temp.x[i])==T & is.na(more$temp.y[i])==T){
    more$temp_new[i]=NA
  }
  if(is.na(more$DO.x[i])==F){
    more$DO_new[i]=more$DO.x[i]
  }
  if(is.na(more$DO.x[i])==T & is.na(more$DO.y[i])==F){
    more$DO_new[i]=more$DO.y[i]
  }
  if(is.na(more$DO.x[i])==T & is.na(more$DO.y[i])==T){
    more$DO_new[i]=NA
  }
}     

more1 <- more %>% 
  select(time:DOC_mgL,temp_new,DO_new) %>% 
  rename(temp = temp_new, DO = DO_new) %>% 
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)) %>% 
  mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
  mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
  mutate(OXY_oxy = DO*1000*(1/32)) %>% 
  mutate(NP = TN_ugL/TP_ugL) %>% 
  mutate(CN = OGM_doc/TN_ugL) %>% 
  mutate(CP = OGM_doc/TP_ugL) %>% 
  mutate(doy = yday(time)) %>% 
  select(time, depth,temp, OXY_oxy, NIT_amm:OGM_doc, TN_ugL, TP_ugL,NP:doy)

more2 <- more1 %>% 
  filter(depth==9)

ggplot(data = more2) +
  geom_path(aes(x = OXY_oxy, y = NP, color = time)) + xlim(0,1000)
ggplot(data = more2) +
  geom_path(aes(x = OXY_oxy, y = CP, color = time)) + xlim(0,1000)
ggplot(data = more2) +
  geom_path(aes(x = OXY_oxy, y = CN, color = time)) + xlim(0,1000)

#let's animate these! https://gganimate.com/articles/gganimate.html
#other good resource: https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#23
p <- ggplot(data = more2) +
  geom_path(aes(x = OXY_oxy, y = NP, color = time)) + xlim(0,1000)

anim <- p + 
  transition_reveal(time) +
  ease_aes('linear')

anim #wow!


ggplot(data = more2) +
  geom_path(aes(x = OXY_oxy, y = CP, color = time)) + xlim(0,1000)
ggplot(data = more2) +
  geom_path(aes(x = OXY_oxy, y = CN, color = time)) + xlim(0,1000)
