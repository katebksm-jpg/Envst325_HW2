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
