#CTD vs YSI comparison for FCR GLM manuscript
#21 Dec 2020 - HLW

#load packages
pacman::p_load(tidyverse,lubridate,data.table,ggpubr,Metrics)

#import CTD observations from EDI
#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/10/2461524a7da8f1906bfc3806d594f94c" 
#infile1 <- paste0(getwd(),"/field_data/CTD_final_2013_2019.csv")
#download.file(inUrl1,infile1,method="curl")

ctd<-read.csv('field_data/CTD_final_2013_2019.csv') %>% #read in observed CTD data, which has multiple casts on the same day (problematic for comparison)
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  mutate(Date = as.POSIXct(strptime(Date, "%Y-%m-%d", tz="EST"))) %>%
  select(Date, Depth_m, DO_mgL,Temp_C) %>%
  na.omit() 

#import YSI observations from EDI
#inUrl2 <- "https://pasta.lternet.edu/package/data/eml/edi/198/7/25b5e8b7f4291614d5c6d959a08148d8"
#infile2 <- paste0(getwd(),"/field_data/YSI_PAR_profiles_2013-2019.csv")
#download.file(inUrl2,infile2,method="curl")

ysi <- read_csv('field_data/YSI_PAR_profiles_2013-2019.csv') %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%m/%d/%y", tz="EST"))) %>%
  select(DateTime, Depth_m, DO_mgL,Temp_C) %>%
  na.omit() 

#------------------------------------------------------------------------------#
#convert to as.Date format
ctd$Date <- as.Date(ctd$Date)
ysi$DateTime <- as.Date(ysi$DateTime)

#find matching dates between ysi and ctd (and make sure that there is only ctd cast per day)
CTD_and_YSI_days <- unique(ysi$DateTime[ysi$DateTime %in% ctd$Date])  

#subset ctd and ysi to include only overlapping dates
ctd <- ctd[ctd$Date %in% CTD_and_YSI_days,]
ysi <- ysi[ysi$DateTime %in% CTD_and_YSI_days,]

#rename ysi DateTime to Date
names(ysi)[1] <- "Date"

#Now manually select the first cast for days where we took more than 1
#for 2019-06-10
ctd<- ctd[-c(which(ctd$Depth_m==9.63 & ctd$Date=="2019-06-10")[1]:
         which(ctd$Depth_m==9.483 & ctd$Date=="2019-06-10")),]
#for 2016-10-07
ctd <- ctd[-c(which(ctd$Depth_m==9.859 & ctd$Date=="2016-10-07"):
                which(ctd$Depth_m==9.704 & ctd$Date=="2016-10-07")),]
#for 2016-08-26
ctd <- ctd[-c(which(ctd$Depth_m==10.110 & ctd$Date=="2016-08-26"):
                which(ctd$Depth_m==9.804 & ctd$Date=="2016-08-26")),]
#for 2016-08-01
ctd <- ctd[-c(which(ctd$Depth_m==9.773 & ctd$Date=="2016-08-01"):
                which(ctd$Depth_m==9.794 & ctd$Date=="2016-08-01")),]
#for 2016-07-25
ctd <- ctd[-c(which(ctd$Depth_m==9.817 & ctd$Date=="2016-07-25")[1]:
                which(ctd$Depth_m==9.894 & ctd$Date=="2016-07-25")[3]),]
#for 2016-07-18
ctd <- ctd[-c(which(ctd$Depth_m==10.085 & ctd$Date=="2016-07-18"):
                which(ctd$Depth_m==9.706 & ctd$Date=="2016-07-18")),]
#for 2016-06-30
ctd <- ctd[-c(which(ctd$Depth_m==9.986 & ctd$Date=="2016-06-30"):
                which(ctd$Depth_m==6.366 & ctd$Date=="2016-06-30")),]
#for 2016-06-28
ctd <- ctd[-c(which(ctd$Depth_m==9.630 & ctd$Date=="2016-06-28"):
                which(ctd$Depth_m==9.766 & ctd$Date=="2016-06-28")),]
#for 2016-06-27
ctd <- ctd[-c(which(ctd$Depth_m==9.645 & ctd$Date=="2016-06-27")[1]:
                which(ctd$Depth_m==10.068  & ctd$Date=="2016-06-27")),]
#for 2016-05-30
ctd <- ctd[-c(which(ctd$Depth_m==9.981 & ctd$Date=="2016-05-30"):
                which(ctd$Depth_m==9.732 & ctd$Date=="2016-05-30")),]

#get data ready for rolling join
setDT(ysi)
setDT(ctd)

#rolling join to match closest ctd depth with ysi depth by each unique date
ctd_vs_ysi <- ctd[ysi,roll="nearest", on=.(Date, Depth_m)]

#rename columns
names(ctd_vs_ysi)[3:6] <- c("ctd_do","ctd_temp","ysi_do","ysi_temp")

#------------------------------------------------------------------------------#
#checking that there is only 1 CTD cast per day
#for(i in 1:length(CTD_and_YSI_days)){
#plot<- ggplot(subset(ctd,Date %in% CTD_and_YSI_days[i]), aes(DO_mgL, Depth_m,color=as.factor(Date))) + geom_point()
#print(plot)
#}

#ysi and ctd by date
ggplot(data=ctd, aes(DO_mgL, Depth_m,color=as.factor(Date))) + geom_line()
ggplot(data=ctd, aes(Temp_C, Depth_m,color=as.factor(Date))) + geom_line()

ggplot(data=ysi, aes(DO_mgL, Depth_m,color=as.factor(Date))) + geom_line() 
ggplot(data=ysi, aes(Temp_C, Depth_m,color=as.factor(Date))) + geom_line() 

#-----------------------------
#CTD vs YSI - DO at all depths
#jpeg("./figures/CTD_vs_YSI_DO.jpg",width = 5, height = 6, units = "in", res = 300)
ggplot(ctd_vs_ysi,aes(x=ctd_do,y=ysi_do,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)
#dev.off()

#0.1m comp
ggplot(subset(ctd_vs_ysi, Depth_m==0.1),aes(x=ctd_do,y=ysi_do,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)
#1.6m comp
ggplot(subset(ctd_vs_ysi, Depth_m==1.6),aes(x=ctd_do,y=ysi_do,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)
#3.8m comp
ggplot(subset(ctd_vs_ysi, Depth_m==1.6),aes(x=ctd_do,y=ysi_do,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)
#5m comp
ggplot(subset(ctd_vs_ysi, Depth_m==5),aes(x=ctd_do,y=ysi_do,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)
#9m comp
ggplot(subset(ctd_vs_ysi, Depth_m==9),aes(x=ctd_do,y=ysi_do,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)

#----------------------------
#DO vs ysi temp - all depths 
#jpeg("./figures/CTD_vs_YSI_temp.jpg",width = 5, height = 6, units = "in", res = 300)
ggplot(ctd_vs_ysi,aes(x=ctd_temp,y=ysi_temp,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)
#dev.off()

#0.1m comp
ggplot(subset(ctd_vs_ysi, Depth_m==0.1),aes(x=ctd_temp,y=ysi_temp,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)
#5m comp
ggplot(subset(ctd_vs_ysi, Depth_m==5),aes(x=ctd_temp,y=ysi_temp,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)
#9m comp
ggplot(subset(ctd_vs_ysi, Depth_m==9),aes(x=ctd_temp,y=ysi_temp,color=Date)) + geom_point(size=3)+
  geom_abline(slope=1, size=1.2)

#-----------------------------------------------------------------------------#
#normality and homoscedasticity assumptions

# Shapiro-Wilk normality (p > 0.05) test for DO
shapiro.test(ctd_vs_ysi$ctd_do) # p < 0.05
shapiro.test(ctd_vs_ysi$ysi_do) #p < 0.05
# Shapiro-Wilk normality test for temp
shapiro.test(ctd_vs_ysi$ctd_temp) # p < 0.05
shapiro.test(ctd_vs_ysi$ysi_temp) #p < 0.05

#testing for homoscedasticity - DO
lmMod_do <- lm(ctd_do ~ ysi_do, data=ctd_vs_ysi) 
par(mfrow=c(2,2)) 
plot(lmMod_do)
#some heteroscedasticity for DO

#testing for homoscedasticity - temp
lmMod_temp <- lm(ctd_temp ~ ysi_temp, data=ctd_vs_ysi) 
par(mfrow=c(2,2)) 
plot(lmMod_temp)

#histogram 
hist(ctd_vs_ysi$ctd_do)
hist(ctd_vs_ysi$ysi_do)
hist(ctd_vs_ysi$ctd_temp)
hist(ctd_vs_ysi$ysi_temp)

#correlation test
cor.test(ctd_vs_ysi$ctd_do, ctd_vs_ysi$ysi_do, method="spearman")
cor.test(ctd_vs_ysi$ctd_temp, ctd_vs_ysi$ysi_temp, method="spearman")
#p < 0.05 so ctd and ysi DO and temp are significantly correlated

#calculate bias and sd between CTD and YSI
bias(ctd_vs_ysi$ysi_do,ctd_vs_ysi$ctd_do)
bias(ctd_vs_ysi$ysi_temp,ctd_vs_ysi$ctd_temp)

#mean/sd ysi and CTD DO and temp
mean(ctd_vs_ysi$ctd_do)
mean(ctd_vs_ysi$ysi_do)
sd(c(ctd_vs_ysi$ysi_do,ctd_vs_ysi$ctd_do))

mean(ctd_vs_ysi$ctd_temp)
mean(ctd_vs_ysi$ysi_temp)
sd(c(ctd_vs_ysi$ysi_temp,ctd_vs_ysi$ctd_temp))

#number of unique days
length(unique(ctd_vs_ysi$Date))
