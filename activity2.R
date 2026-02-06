#install packages
# install.packages(c("dplyr","lubridate"))
library(dplyr)
library(lubridate)

#reading in files
streamH <- read.csv("/cloud/project/activity02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activity02/site_info.csv")

#parsing data
streamH$dateF <- ymd_hm(streamH$datetime)
streamH$date <- year(streamH$datetime)


peaceH <- streamH %>%
  filter(siteID == 2295637)

# make plot 
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="date", 
     ylab="Stage Height (ft)")

#joining tables 
floods <- full_join(streamH, siteInfo, by="siteID")


#summary statistics
height.ave <- floods %>%
  group_by(names) %>%
  summarise(mean.height = mean(gheight.ft))

floods$day <- yday(floods$dateF)

height.day <- floods%>%
  group_by(names, day) %>%
  summarise(mean.height = mean(gheight.ft))

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major=n())

#prompt 3
floodcat <- floods%>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names)%>%
  summarise(min_date=min(dateF))


# Homework Prompts --------------------------------------------------------
#question 1: Make a separate plot of the stream stage data for each river.
#subset stream stage data for Fisheating Creek
FishE <- streamH %>%
  filter(siteID == 2256500)

#make plot for FishE
plot(FishE$dateF, FishE$gheight.ft, type="b", pch=19, 
     main = "Fisheating Creek Stage Height", 
     xlab="Date", ylab="Stream Stage (ft)")
#make plot for Peace River
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, main ="Peace River Stage Height", 
     xlab="Date", ylab="Stream Stage (ft)")
#make plot for Santa Fe River
#first subset strem stage data for Santa Fe River
SantaFe <- streamH %>%
  filter(siteID==2322500)
plot(SantaFe$dateF, SantaFe$gheight.ft, type="b", pch=19, main = "Santa Fe River Stage Height", xlab="date", ylab="Stream Stage (ft)")
#make plot for WITHLACOOCHEE
#first subset data
Withla <- streamH %>%
  filter(siteID == 2312000)
plot(Withla$dateF, Withla$gheight.ft, 
     type="b", pch=10, main="Withlacoochee River Stage Height", 
     xlab="date", ylab="Stream Stage (ft)")
